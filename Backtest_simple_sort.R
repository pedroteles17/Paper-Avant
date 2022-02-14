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

#Importa o retorno diario do indice
dbRet_ind <- read_csv(paste(path_dado, "indice.csv", sep = "\\"), col_types = c("D", "n"))

# Importa o retorno diario da taxa livre de risco
dbRisk_free <- read_csv(paste(path_dado, "rf.csv", sep = "\\"), col_types = c("D", "n"))
  
# Importa o retorno diário dos ativos que compoe o indice
dbRet_ativos <- read_csv(paste(path_dado, "ativos.csv", sep = "\\"), col_types = c("D", rep("n", 304)))
  
# Importa dados mensais do indicador em questão
dbEstat_ip <- read_csv(paste(path_dado, "value.csv", sep = "\\"))
dbEstat <- data.frame(as.Date(dbEstat_ip$Data), apply(dbEstat_ip[,-1], 2, as.numeric))
colnames(dbEstat) <- colnames(dbEstat_ip)
rm(dbEstat_ip)
dbEstat$Data <- as.Date(as.Date(dbEstat$Data[1]) %m+% months(0:(nrow(dbEstat) - 1)))

dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Value", "desc", 12)

ret_port_geral <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")
  
ret_port_geral <- do.call("cbind.xts", lapply(ret_port_geral, function(x) do.call("rbind.xts", x)))



