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
dbComp_ip <- read_csv(paste(path_dado, "comp.csv", sep = "\\"), col_types = c("c", rep("n", 244)))
dbComp_ip <- dbComp_ip[,-2]
data_inicial <- as.Date(colnames(dbComp_ip)[2]) + days(1)
colnames(dbComp_ip) <- append("Ativos", as.character(data_inicial %m+% months(0:(ncol(dbComp_ip)-2))))
rm(data_inicial)
dbComp_ip <- dbComp_ip %>% dplyr::filter(Ativos %ni% "RIPI4 BS Equity")

# Importa o retorno diário dos ativos que compoe o indice
dbRet_ativos <- read_csv(paste(path_dado, "ativos.csv", sep = "\\"), col_types = c("D", rep("n", 304)))
  
# Volatilidade ----

dbComp <- dbComp_ip

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(12), dbRet_ativos, "Volatilidade", "desc", 0)

ret_port_vol <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")

ret_port_vol <- do.call("cbind.xts", lapply(ret_port_vol, function(x) do.call("rbind.xts", x)))
colnames(ret_port_vol) <- paste0("D", seq(1,10))

ret_port_vol_3 <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 3, "EW")

ret_port_vol_3 <- do.call("cbind.xts", lapply(ret_port_vol_3, function(x) do.call("rbind.xts", x)))
colnames(ret_port_vol_3) <- paste0("D", seq(1,3))

# Momentum ----

dbComp <- dbComp_ip

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(6), dbRet_ativos, "Momentum", "asc", 0)

ret_port_mom <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")

ret_port_mom <- do.call("cbind.xts", lapply(ret_port_mom, function(x) do.call("rbind.xts", x)))
colnames(ret_port_mom) <- paste0("D", seq(1,10))

# Value ----

# Importa dados mensais do indicador em questão
dbEstat <- base_indic("value.csv")

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Value", "desc", 0)

ret_port_value <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")
  
ret_port_value <- do.call("cbind.xts", lapply(ret_port_value, function(x) do.call("rbind.xts", x)))
colnames(ret_port_value) <- paste0("D", seq(1,10))

# Quality ----

# Importa dados mensais do indicador em questão
dbEstat <- base_indic("quality.csv")

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Quality", "asc", 0)

ret_port_quality <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")

ret_port_quality <- do.call("cbind.xts", lapply(ret_port_quality, function(x) do.call("rbind.xts", x)))
colnames(ret_port_quality) <- paste0("D", seq(1,10))

# Size ----

# Importa dados mensais do indicador em questão
dbEstat <- base_indic("size.csv")

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Size", "desc", 0)

ret_port_size <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")

ret_port_size <- do.call("cbind.xts", lapply(ret_port_size, function(x) do.call("rbind.xts", x)))
colnames(ret_port_size) <- paste0("D", seq(1,10))

# Export ----

file_name <- paste0(getwd(), "/Portfolios")

write.csv(data.frame(Data = index(ret_port_vol), ret_port_vol), paste0(file_name, "\\simple_sort_vol_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_vol_3), ret_port_vol_3), paste0(file_name, "\\simple_sort_vol_3.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_mom), ret_port_mom), paste0(file_name, "\\simple_sort_mom_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_size), ret_port_size), paste0(file_name, "\\simple_sort_size_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_quality), ret_port_quality), paste0(file_name, "\\simple_sort_quality_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_value), ret_port_value), paste0(file_name, "\\simple_sort_value_10.csv"), row.names = FALSE)

