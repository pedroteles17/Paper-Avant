library(tidyverse)
library(readxl)
library(xts)
library(PerformanceAnalytics)
library(lmtest)
library(AER)
library(ggthemes)
library(lubridate)
library(stargazer)
library(kableExtra)
library(furrr)
library(progressr)
library(parallel)
library(plotly)
library(DT)

base_indic <- function(arquivo){
  dbEstat_ip <- read_csv(paste(path_dado, arquivo, sep = "\\"))
  dbEstat <- data.frame(as.Date(dbEstat_ip$Data), apply(dbEstat_ip[,-1], 2, as.numeric))
  colnames(dbEstat) <- colnames(dbEstat_ip)
  dbEstat$Data <- as.Date(as.Date(dbEstat$Data[1]) %m+% months(0:(nrow(dbEstat) - 1)))
  
  return(dbEstat)
}

"%ni%" <- Negate("%in%")

plan(multisession, workers = parallel::detectCores())

source("Funcoes.R")

path_dado <- paste(getwd(), "Brasil", sep = "/")

# Importa a composicao do indice
dbComp_ip <- read_csv(paste(path_dado, "comp.csv", sep = "\\"), col_types = paste0("c", paste(rep("n", 244), collapse = ""), collapse = ""))
dbComp_ip <- dbComp_ip[,-2]
data_inicial <- as.Date(colnames(dbComp_ip)[2]) + days(1)
colnames(dbComp_ip) <- append("Ativos", as.character(data_inicial %m+% months(0:(ncol(dbComp_ip)-2))))
rm(data_inicial)
dbComp_ip <- dbComp_ip %>% dplyr::filter(Ativos %ni% "RIPI4 BS Equity")

# Importa o retorno diario do indice
dbRet_ind <- read_csv(paste(path_dado, "indice.csv", sep = "\\"), col_types = "Dn")

# Importa o retorno diario da taxa livre de risco
dbRisk_free <- read_csv(paste(path_dado, "rf.csv", sep = "\\"), col_types = "Dn")

# Importa o retorno diário dos ativos que compoe o indice
dbRet_ativos <- read_csv(paste(path_dado, "ativos.csv", sep = "\\"), col_types = paste0("D", paste(rep("n", 304), collapse = ""), collapse = ""))

# Momentum ----

dbComp <- dbComp_ip

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(12), dbRet_ativos, "Momentum-1", "asc", 0)

ret_port_mom <- backtest(lista_ativos[[1]][[1]], 20021231, 20211231, 0.3, "EW")

colnames(ret_port_mom) <- c('mom_lo', 'mom_ls')

# Value ----

# Importa dados mensais do indicador em questão
dbEstat <- base_indic("value.csv")

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Value", "desc", 0)

ret_port_value <- backtest(lista_ativos[[1]][[1]], 20021231, 20211231, 0.3, "EW")

colnames(ret_port_value) <- c('value_lo', 'value_ls')

# Quality ----

# Importa dados mensais do indicador em questão
dbEstat <- base_indic("quality.csv")

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Quality", "asc", 0)

ret_port_quality <- backtest(lista_ativos[[1]][[1]], 20021231, 20211231, 0.3, "EW")

colnames(ret_port_quality) <- c('quality_lo', 'quality_ls')

# Size ----

# Importa dados mensais do indicador em questão
dbEstat <- base_indic("size.csv")

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Size", "desc", 0)

ret_port_size <- backtest(lista_ativos[[1]][[1]], 20021231, 20211231, 0.5, "EW")

colnames(ret_port_size) <- c('size_lo', 'size_ls')

# Fusao dos portfolios ----

port_geral <- list(ret_port_size, ret_port_value, ret_port_mom, ret_port_quality)

port_geral <- do.call('cbind.xts', port_geral)

# Seleciona apenas as colunas impares - aquelas que possuem o retorno do port Long e Short
port_geral <- port_geral[,seq(2, ncol(port_geral), by = 2)]

port_geral <- data.frame(Data = index(port_geral), port_geral)

# Fusao CDI e IBX-CDI ----

cdi <- read_excel('CDI.xlsx') %>% dplyr::select(-1)
cdi$Data <- as.Date(cdi$Data)

indice <- dbRet_ind %>% dplyr::filter(Data > '1999-12-31', Data <= '2021-12-31')

cdi_ind <- merge(indice, cdi, all.x = TRUE, by = 'Data')

# Calculate returns
cdi_ind$CDI <- append(NA, diff(cdi_ind$CDI)/cdi_ind$CDI[-length(cdi_ind$CDI)])

# First line is NA
cdi_ind <- cdi_ind[-1,]

# Market return above risk free rate
cdi_ind$Rm_rf <- cdi_ind$`IBX Index` - cdi_ind$CDI

cdi_ind <- cdi_ind %>% dplyr::select(!`IBX Index`)

# Fusao CDI e premio mercado com os fatores

fatores_risco <- merge(port_geral, cdi_ind, all.x = TRUE, by = 'Data')

colnames(fatores_risco) <- c('Data', 'SMB', 'HML', 'WML', 'QMJ', 'Risk_free', 'Rm_minus_Rf')

fatores_risco <- fatores_risco %>% dplyr::select(Data, Rm_minus_Rf, SMB, HML, WML, QMJ, Risk_free)

writexl::write_xlsx(fatores_risco, 'fatores.xlsx')