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
library(pBrackets)
library(grid)

nefin <- read_excel("nefin.xlsx", col_types = c("date", rep("numeric", 6))) %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31")

indice <- read_csv("Brasil\\indice.csv", col_types = "Dn") %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31") %>% 
  set_names(c("Data", "IBX"))

trad_strat <- read_csv("Portfolios\\raw_trad_strat.csv", col_types = "Dnnn")

# Utilizado para tabela 8
argumentos <- function(inicio, fim, hold_period, periodo_estim) {
  n_meses <- ceiling(interval(ymd(inicio), ymd(fim)) / months(1)) - 1 #Num de meses entre inicio e fim
  
  estim_inicial <- ymd(inicio) %m-% months(periodo_estim) %m+% months(seq(0, n_meses, by = hold_period))
  estim_final <- ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period))
  aval_inicial <- ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period))
  aval_final <- ymd(inicio) %m+% months(hold_period + seq(0, n_meses, by = hold_period))
  
  data.frame(estim_inicial, estim_final, aval_inicial, aval_final)
}

trad_strat$IBX <- indice$IBX

trad_strat$Risk_free <- nefin$Risk_free

datas_beta <- argumentos(20031231, 20211231, 1, 12)

# Calcula, para cada mes, o beta do portfolio Long e o beta do portfolio Short
short_exp <- vector(length = nrow(datas_beta))
short_exp_blume <- vector(length = nrow(datas_beta))
for (i in 1:nrow(datas_beta)) {
  datas <- datas_beta[i, , drop = TRUE]
  
  trad_strat_estim <- trad_strat %>% dplyr::filter(Data > datas[[1]] & Data <= datas[[2]])
  
  beta_long <- as.numeric(coef(lm(I(trad_strat_estim$Long - trad_strat_estim$Risk_free) ~ I(trad_strat_estim$IBX - trad_strat_estim$Risk_free)))[2])
  
  beta_short <- as.numeric(coef(lm(I(trad_strat_estim$Short - trad_strat_estim$Risk_free) ~ I(trad_strat_estim$IBX - trad_strat_estim$Risk_free)))[2])
  
  # Calcula a exposicao short 
  short_exp[i] <- beta_long / beta_short
  
  # Calcula os betas ajustados pela metodologia de Blume
  beta_long_blume <- beta_long * (2/3) + 1 * (1/3)
  
  beta_short_blume <- beta_short * (2/3) + 1 * (1/3)
  
  # Calcula a exposicao short utilizando a metodologia de Blume
  short_exp_blume[i] <- beta_long_blume / beta_short_blume
}

# Calcula o retorno do Portfolio L&S com base na "short exposure"
ret_beta_ratio <- vector("list", length = nrow(datas_beta))
ret_beta_ratio_blume <- vector("list", length = nrow(datas_beta))
for (i in 1:nrow(datas_beta)) {
  datas <- datas_beta[i, , drop = TRUE]
  
  trad_strat_aval <- trad_strat %>% dplyr::filter(Data > datas[[3]] & Data <= datas[[4]]) %>%
    select(Data, Long, Short, Risk_free)
  
  trad_strat_aval <- xts(trad_strat_aval[,-1], trad_strat_aval$Data)
  
  # Calcula o retorno para o port L&S
  ret_beta_ratio[[i]] <- PerformanceAnalytics::Return.portfolio(trad_strat_aval, c(1, -short_exp[i], short_exp[i]))
  
  # Calcula o retorno para o port L&S usando a metodologia de Blume
  ret_beta_ratio_blume[[i]] <- PerformanceAnalytics::Return.portfolio(trad_strat_aval, c(1, -short_exp_blume[i], short_exp_blume[i]))
}

rf <- nefin %>% dplyr::filter(Data >= '2004-01-01')

ret_beta_ratio <- do.call('rbind.xts', ret_beta_ratio)
ret_beta_ratio_blume <- do.call('rbind.xts', ret_beta_ratio_blume)
ret_beta <- merge.xts(ret_beta_ratio, ret_beta_ratio_blume)
ret_beta$RiskFree <- rf$Risk_free
colnames(ret_beta) <- c('BetaPuro', 'BetaBlume', 'RiskFree')

file_name <- paste0(getwd(), "/Portfolios")
write.csv(data.frame(Data = index(ret_beta), ret_beta), paste0(file_name, "\\trad_strat.csv"), row.names = FALSE)

