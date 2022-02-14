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

double_sort_3 <- function(lista_ativos){
  vol_high_fact <- list() # fact = factor
  vol_mid_fact <- list()
  vol_low_fact <- list()
  for (i in seq_along(lista_ativos)) {
    df <- lista_ativos[[i]]
    
    high <- c(1, floor(nrow(df) * 0.3))
    mid <- c(high[2] + 1, high[2] + nrow(df) - high[2]*2)
    low <- c(mid[2] + 1, nrow(df))
    
    high_fact <- df[seq(high[1], high[2]), , drop = FALSE]
    mid_fact <- df[seq(mid[1], mid[2]), , drop = FALSE]
    low_fact <- df[seq(low[1], low[2]), , drop = FALSE]
    
    df_vol <- lista_vol[[1]][[1]][[i]]
    
    vol_high_fact[[i]] <- df_vol %>% dplyr::filter(row.names(df_vol) %in% rownames(high_fact))
    vol_mid_fact[[i]] <- df_vol %>% dplyr::filter(row.names(df_vol) %in% rownames(mid_fact))
    vol_low_fact[[i]] <- df_vol %>% dplyr::filter(row.names(df_vol) %in% rownames(low_fact))
  }
  
  vol_high_fact <- backtest_decil(vol_high_fact, 20021231, 20211231, 3, "EW")
  vol_mid_fact <- backtest_decil(vol_mid_fact, 20021231, 20211231, 4, "EW")
  vol_low_fact <- backtest_decil(vol_low_fact, 20021231, 20211231, 3, "EW")
  
  vol_high_fact <- do.call("cbind.xts", lapply(vol_high_fact, function(x) do.call("rbind.xts", x)))
  vol_mid_fact <- do.call("cbind.xts", lapply(vol_mid_fact, function(x) do.call("rbind.xts", x)))
  vol_low_fact <- do.call("cbind.xts", lapply(vol_low_fact, function(x) do.call("rbind.xts", x)))
  
  vol <- cbind.xts(cbind.xts(vol_high_fact, vol_mid_fact), vol_low_fact)
  
  return(vol)
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

## Lista de Ativos ----

dbComp <- dbComp_ip

# Lista Volatilidade
lista_vol <- params_estim(20021231, 20211231, c(1), c(12), dbRet_ativos, "Volatilidade", "desc", 0)

# Lista Momentum
lista_mom <- params_estim(20021231, 20211231, c(1), c(6), dbRet_ativos, "Momentum", "asc", 0)

# Lista Value
dbEstat <- base_indic("value.csv")
dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))
lista_value <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Value", "desc", 0)

# Lista Quality
dbEstat <- base_indic("quality.csv")
dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))
lista_quality <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Quality", "asc", 0)

# Lista Size
dbEstat <- base_indic("size.csv")
dbComp <- dbComp_ip %>% dplyr::filter(Ativos %ni% setdiff(dbComp_ip$Ativos, colnames(dbEstat)))
lista_size <- params_estim(20021231, 20211231, c(1), c(1), dbEstat, "Size", "desc", 0)

## Double Sort ----

# Momentum
mom_vol <- double_sort_3(lista_mom[[1]][[1]])
colnames(mom_vol) <- paste0("D", 1:10)

# Value
value_vol <- double_sort_3(lista_value[[1]][[1]])
colnames(value_vol) <- paste0("D", 1:10)

# Quality
quality_vol <- double_sort_3(lista_quality[[1]][[1]])
colnames(quality_vol) <- paste0("D", 1:10)

# Size
lista_ativos <- lista_size[[1]][[1]]

vol_high_fact <- list()
vol_low_fact <- list()
for (i in seq_along(lista_ativos)) {
  df <- lista_ativos[[i]]
  
  corte <- 1:(nrow(df)*0.5)
  
  high_fact <- df[corte, , drop = FALSE]
  low_fact <- df[-corte, , drop = FALSE]
  
  df_vol <- lista_vol[[1]][[1]][[i]]
  
  vol_high_fact[[i]] <- df_vol %>% dplyr::filter(row.names(df_vol) %in% rownames(high_fact))
  vol_low_fact[[i]] <- df_vol %>% dplyr::filter(row.names(df_vol) %in% rownames(low_fact))
}

vol_high_fact <- backtest_decil(vol_high_fact, 20021231, 20211231, 5, "EW")
vol_low_fact <- backtest_decil(vol_low_fact, 20021231, 20211231, 5, "EW")

vol_high_fact <- do.call("cbind.xts", lapply(vol_high_fact, function(x) do.call("rbind.xts", x)))
vol_low_fact <- do.call("cbind.xts", lapply(vol_low_fact, function(x) do.call("rbind.xts", x)))

size_vol <- cbind.xts(vol_high_fact, vol_low_fact)
colnames(size_vol) <- paste0("D", 1:10)

# Export ----

file_name <- paste0(getwd(), "/Portfolios")

write.csv(data.frame(Data = index(size_vol), size_vol), paste0(file_name, "\\double_sort_size.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(quality_vol), quality_vol), paste0(file_name, "\\double_sort_quality.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(mom_vol), mom_vol), paste0(file_name, "\\double_sort_mom.csv"), row.names = FALSE)
write.csv(data.frame(Data = index(value_vol), value_vol), paste0(file_name, "\\double_sort_value.csv"), row.names = FALSE)

