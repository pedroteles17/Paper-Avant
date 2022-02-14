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

simple_sort_3 <- function(lista_ativos){
  high_fact <- list() # fact = factor
  mid_fact <- list()
  low_fact <- list()
  for (i in seq_along(lista_ativos)) {
    df <- lista_ativos[[i]]
    
    high <- c(1, floor(nrow(df) * 0.3))
    low <- c(nrow(df) - high[2] + 1, nrow(df))
    mid <- c(high[2] + 1, low[1] - 1)
    
    high_fact[[i]] <- df[seq(high[1], high[2]), , drop = FALSE]
    mid_fact[[i]] <- df[seq(mid[1], mid[2]), , drop = FALSE]
    low_fact[[i]] <- df[seq(low[1], low[2]), , drop = FALSE]
  }
  
  high_fact <- backtest(high_fact, 20021231, 20211231, 1, "EW")[,1]
  mid_fact <- backtest(mid_fact, 20021231, 20211231, 1, "EW")[,1]
  low_fact <- backtest(low_fact, 20021231, 20211231, 1, "EW")[,1]
  
  fact <- cbind.xts(cbind.xts(high_fact, mid_fact), low_fact)
  
  return(fact) 
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
  
# Volatilidade ----

dbComp <- dbComp_ip

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(12), dbRet_ativos, "Volatilidade", "desc", 0)

ret_port_vol <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")

ret_port_vol <- do.call("cbind.xts", lapply(ret_port_vol, function(x) do.call("rbind.xts", x)))
colnames(ret_port_vol) <- paste0("D", seq(1,10))

ret_port_vol_3 <- simple_sort_3(lista_ativos[[1]][[1]])

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

# Beta ----

dbComp <- dbComp_ip

# inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist
lista_ativos <- params_estim(20021231, 20211231, c(1), c(12), dbRet_ativos, "Low Beta", "desc", 0)

ret_port_beta <- backtest_decil(lista_ativos[[1]][[1]], 20021231, 20211231, 10, "EW")

ret_port_beta <- do.call("cbind.xts", lapply(ret_port_beta, function(x) do.call("rbind.xts", x)))
colnames(ret_port_beta) <- paste0("D", seq(1,10))

ret_port_beta_3 <- simple_sort_3(lista_ativos[[1]][[1]])

colnames(ret_port_beta_3) <- paste0("D", seq(1,3))

# IBX Equal Weighted ----

ibx_ew <-  backtest(lista_ativos[[1]][[1]], 20021231, 20211231, 1, "EW")[,1]
colnames(ibx_ew) <- "IBX_EW"

# Export ----

file_name <- paste0(getwd(), "/Portfolios")

write.csv(data.frame(Data = index(ret_port_vol), ret_port_vol), paste0(file_name, "\\simple_sort_vol_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_vol_3), ret_port_vol_3), paste0(file_name, "\\simple_sort_vol_3.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_mom), ret_port_mom), paste0(file_name, "\\simple_sort_mom_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_size), ret_port_size), paste0(file_name, "\\simple_sort_size_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_quality), ret_port_quality), paste0(file_name, "\\simple_sort_quality_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_value), ret_port_value), paste0(file_name, "\\simple_sort_value_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_beta), ret_port_beta), paste0(file_name, "\\simple_sort_beta_10.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ret_port_beta_3), ret_port_beta_3), paste0(file_name, "\\simple_sort_beta_3.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(ibx_ew), ibx_ew), paste0(file_name, "\\ibx_ew.csv"), row.names = FALSE)
