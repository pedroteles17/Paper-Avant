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


dados <- readr::read_csv("Portfolios\\simple_sort_vol_3.csv", col_types = "Dnnn") %>% 
  set_names(c("Data", "LowVol", "MidVol", "HighVol"))

dados$LongShort <- dados$LowVol - dados$HighVol

nefin <- read_excel("nefin.xlsx", col_types = c("date", rep("numeric", 6))) %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31")

indice <- read_csv("Brasil\\indice.csv", col_types = "Dn") %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31") %>% 
  set_names(c("Data", "IBX"))

# Funções ----

jobson_korkie_memmel <- function(shrp1, shrp2, corel, n_obs){
  z <- (shrp1 - shrp2) / sqrt((1 / n_obs) * (2*(1 - corel) + 0.5 * (shrp1^2 + shrp2^2 - shrp1 * shrp2 * (1 + corel^2)))) 
  return(z)
}

estat_jkm <- function(ret1, ret2, ind, rf){
  SR1 <- mean((ret1 - rf)) / sd(ret1)
  SR2 <- mean((ret2 - rf)) / sd(ret2)
  
  t_SR <- jobson_korkie_memmel(SR1, SR2, cor(ret1, ret2), length(ret1))
  
  return(round(t_SR, 2))
}

estat_ret <- function(ret_port, ind, rf) {
  
  ret_period <- function(ret) {
    prod(ret + 1)^(252 / length(ret)) - 1
  }
  
  regres <- lm(I(ret_port - rf) ~ I(ind - rf))
  
  alfa <- as.numeric((coefficients(regres)[1] + 1)^252 - 1)
  p_valor_alfa <- summary(regres)$coefficients[1, 4]
  t_alfa <- summary(regres)$coefficients[1, 3]
  beta <- as.numeric(coefficients(regres)[2])
  
  ret_acumul <- ret_period(ret_port)
  vol <- sd(ret_port) * sqrt(252)
  vol_ind <- sd(ind) * sqrt(252)
  SR <- mean((ret_port - rf)) / sd(ret_port)
  SR_ind <- mean((ind - rf)) / sd(ind)
  
  t_SR <- jobson_korkie_memmel(SR, SR_ind, cor(ret_port, ind), length(ret_port))
  
  rp_rf <- ret_period(ret_port - rf)
  
  
  resultados <- data.frame(c(
    ret_acumul, vol, SR, t_SR,
    rp_rf, beta, alfa, t_alfa
  )) 
  
  rownames(resultados) <- c(
    "Retorno Anualizado (%)",
    "Sigma (%)", "IS", "t(IS)",
    "Rp-Rf Anualizado (%)",
    "Beta", "Alfa (%)", "t(alfa)"
  )
  
  resultados[c(1, 2, 5, 7), 1] <- round(resultados[c(1, 2, 5, 7), 1]*100, 2)
  resultados[c(3, 4, 6, 8), 1] <- round(resultados[c(3, 4, 6, 8), 1], 2)
  
  return(resultados)
}

media_max_perda <- function(ret, nome_col){
  media_alta <- mean(ret[ret > 0]) * 100
  media_baixa <- mean(ret[ret < 0]) * 100
  maior_perda <- min(ret) * 100
  
  media_max <- data.frame(c(
    media_alta, media_baixa,
    maior_perda
  ))
  
  rownames(media_max) <-c("Media das alturas", "Media das baixas", "Maior perda observada")
  
  media_max <- round(media_max, 2)
}

analise_fatores <- function(retornos, tipo, nome_col){
  capm <- summary(lm(I(retornos - nefin$Risk_free) ~ nefin$Rm_minus_Rf))$coefficients
  
  fatores3 <- summary(lm(I(retornos - nefin$Risk_free) ~ nefin$Rm_minus_Rf + nefin$SMB + nefin$HML))$coefficients
  
  fatores4 <- summary(lm(I(retornos - nefin$Risk_free) ~ nefin$Rm_minus_Rf + nefin$SMB + nefin$HML + nefin$WML))$coefficients
  
  fatores5 <- summary(lm(I(retornos - nefin$Risk_free) ~ nefin$Rm_minus_Rf + nefin$SMB + nefin$HML + nefin$WML + nefin$IML))$coefficients
  
  if(tipo == "Alfa"){
    result <- data.frame(c(
      capm[1,1],
      fatores3[1,1],
      fatores4[1,1],
      fatores5[1,1]
    )) %>% set_names(nome_col)
    
    result <- round(((result + 1) ^ 252 - 1)*100, 2)
    
    rownames(result) <- c("CAPM", "3 Fatores", "4 Fatores", "5 Fatores") 
    
    return(result)
  } else if(tipo == "t"){
    result <- data.frame(c(
      capm[1,3],
      fatores3[1,3],
      fatores4[1,3],
      fatores5[1,3]
    )) %>% set_names(nome_col)
    
    result <- round(result, 2)
    
    rownames(result) <- c("CAPM", "3 Fatores", "4 Fatores", "5 Fatores")
    
    return(result)
  }
}

funcao_painel <- function(df_ret, ind, rf){
  df_painel <- data.frame(df_ret, IBX = ind)
  
  painel <-  lapply(df_painel, function(x) estat_ret(x, ind, rf))
  
  painel <- do.call("cbind", painel) 
  
  colnames(painel) <- c(paste0("D", 1:(ncol(painel)-2)), "VOL", "Univ.")
  
  painel[c(3, 4), (ncol(painel)-1)] <- ""
  painel[c(4, 6, 7, 8), ncol(painel)] <- ""
  
  regres_ls <- summary(lm(df_ret$LongShort ~ I(ind - rf)))$coefficients
  alfa_regres <- round((prod(1 + regres_ls[1,1]) ^ 12 - 1) * 100, 2)
  painel[c(6, 7, 8), (ncol(painel)-1)] <- c(round(regres_ls[2,1],2), alfa_regres, round(regres_ls[2,2],2))
  
  return(painel)
  
}

# Utilizado para tabela 8
argumentos <- function(inicio, fim, hold_period, periodo_estim) {
  n_meses <- ceiling(interval(ymd(inicio), ymd(fim)) / months(1)) - 1 #Num de meses entre inicio e fim
  
  estim_inicial <- ymd(inicio) %m-% months(periodo_estim) %m+% months(seq(0, n_meses, by = hold_period))
  estim_final <- ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period))
  aval_inicial <- ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period))
  aval_final <- ymd(inicio) %m+% months(hold_period + seq(0, n_meses, by = hold_period))
  
  data.frame(estim_inicial, estim_final, aval_inicial, aval_final)
}

# Figura 1 – Plotagem das carteiras ordenadas por volatilidade ----

ibx_ew <- read_csv("Portfolios\\ibx_ew.csv", col_types = "Dn")

dados_fig <- data.frame(dados[,c(-1, -ncol(dados))], IBX = indice$IBX, IBX_EW = ibx_ew$IBX_EW)

dados_fig <- read_csv("Portfolios\\simple_sort_vol_10.csv", col_types = "Dnnnnnnnnnn") %>%
  mutate(IBX = indice$IBX, IBX_EW = ibx_ew$IBX_EW) %>% dplyr::select(-1)

vetor_betas <- vector(length = ncol(dados_fig))
for (i in seq_along(vetor_betas)) {
  vetor_betas[i] <- coef(lm(I(dados_fig[,i, drop = TRUE] - nefin$Risk_free) ~ I(indice$IBX - nefin$Risk_free)))[2]
}

port_betas_ret <- data.frame(Nome = c(paste0("P", 1:(length(vetor_betas)-2)), "IBX", "IBX EW"),
                             Beta = vetor_betas,
                             Ret_acumul = apply(dados_fig, 2, function(x) prod(1+x)^(252/length(x)) - 1))

reg_mod_coef <- coef(lm(Ret_acumul ~ Beta, data = port_betas_ret))

ret_rf <- prod(1 + nefin$Risk_free) ^ (252/length(nefin$Risk_free)) - 1
ret_mercado_rf <- prod(1 + (indice$IBX)) ^ (252/length(indice$IBX)) - 1 - ret_rf

sml <- ret_rf + seq(0.5,1.4, by = 0.1) * ret_mercado_rf

ggplot() + geom_point(data = port_betas_ret, aes(x = Beta, y = Ret_acumul)) + 
  geom_abline(slope = reg_mod_coef[2], intercept = reg_mod_coef[1], colour = "yellow3", linetype = "dashed") +
  geom_line(aes(x = seq(0.5,1.4, by = 0.1), y = sml), colour = "dark blue") + theme_classic() + 
  geom_text(data = port_betas_ret, aes(label = Nome, x = Beta, y = Ret_acumul), hjust = 0.5,  vjust = -1) +
  xlab("Beta") + ylab("Annualized Return") + 
  scale_y_continuous(expand = c(0.01, 0.02), labels = scales::percent_format(accuracy = 1)) 


rm(ibx_ew, dados_fig, i, reg_mod_coef, sml, vetor_betas, port_betas_ret, ret_rf, ret_mercado_rf)



# Tabela 1 – Resultados dos portfólios de baixa volatilidade ----

## Painel A
df_vol_10 <- read_csv("Portfolios\\simple_sort_vol_10.csv", col_types = "Dnnnnnnnnnn")
df_vol_10$LongShort <- df_vol_10$D1 - df_vol_10$D10

tb1_painel_a <- funcao_painel(df_vol_10[,-1], indice$IBX, nefin$Risk_free)

## Painel B
tb1_painel_b <- lapply(df_vol_10[,-1], function(x) media_max_perda(x))
tb1_painel_b <- do.call("cbind", tb1_painel_b)

tb1_painel_b <- data.frame(tb1_painel_b, media_max_perda(indice$IBX))

colnames(tb1_painel_b) <- c(paste0("D", 1:10), "D1_D10", "Univ")

## Painel C
tb1_painel_c <- funcao_painel(dados[,-1], indice$IBX, nefin$Risk_free)

rm(df_vol_10)


# Tabela 2 – Divisão em períodos ----

dados_03_08 <- dados %>% dplyr::filter(Data >= "2003-01-01" & Data <= "2008-12-31")
indice_03_08 <- indice %>% dplyr::filter(Data >= "2003-01-01" & Data <= "2008-12-31")
nefin_03_08 <- nefin %>% dplyr::filter(Data >= "2003-01-01" & Data <= "2008-12-31")

dados_09 <- dados %>% dplyr::filter(Data >= "2009-01-01" & Data <= "2009-12-31")
indice_09 <- indice %>% dplyr::filter(Data >= "2009-01-01" & Data <= "2009-12-31")
nefin_09 <- nefin %>% dplyr::filter(Data >= "2009-01-01" & Data <= "2009-12-31")

dados_10_21 <- dados %>% dplyr::filter(Data >= "2010-01-01" & Data <= "2021-12-31")
indice_10_21 <- indice %>% dplyr::filter(Data >= "2010-01-01" & Data <= "2021-12-31")
nefin_10_21 <- nefin %>% dplyr::filter(Data >= "2010-01-01" & Data <= "2021-12-31")

## Painel A
tb2_painel_a <- funcao_painel(dados_03_08[,-1], indice_03_08$IBX, nefin_03_08$Risk_free)

## Painel B
tb2_painel_b <- funcao_painel(dados_09[,-1], indice_09$IBX, nefin_09$Risk_free)

## Painel C
tb2_painel_c <- funcao_painel(dados_10_21[,-1], indice_10_21$IBX, nefin_10_21$Risk_free)

rm(dados_03_08, dados_09, dados_10_21,
   indice_03_08, indice_09, indice_10_21,
   nefin_03_08, nefin_09, nefin_10_21)


# Tabela 3 – Regressão nos modelos multi-fatoriais ----

## Painel A
tb3_painel_a <- data.frame(analise_fatores(dados$LowVol, "Alfa", "Menores Vol"),
                           analise_fatores(dados$MidVol, "Alfa", "Intermediaria"),
                           analise_fatores(dados$HighVol, "Alfa", "Maiores Vol"))

## Painel B
tb3_painel_b <- data.frame(analise_fatores(dados$LowVol, "t", "Menores Vol"),
                           analise_fatores(dados$MidVol, "t", "Intermediaria"),
                           analise_fatores(dados$HighVol, "t", "Maiores Vol"))

# Tabela 4 -  Resultados dos portfólios formados com outros fatores ----

## Painel A
df_size <- read_csv("Portfolios\\simple_sort_size_10.csv", col_types = "Dnnnnnnnnnn")
df_size$LongShort <- df_size$D1 - df_size$D10

tb4_painel_a <- funcao_painel(df_size[,-1], indice$IBX, nefin$Risk_free)

## Painel B
df_value <- read_csv("Portfolios\\simple_sort_value_10.csv", col_types = "Dnnnnnnnnnn")
df_value$LongShort <- df_value$D1 - df_value$D10

tb4_painel_b <- funcao_painel(df_value[,-1], indice$IBX, nefin$Risk_free)

## Painel C
df_mom <- read_csv("Portfolios\\simple_sort_mom_10.csv", col_types = "Dnnnnnnnnnn")
df_mom$LongShort <- df_mom$D1 - df_mom$D10

tb4_painel_c <- funcao_painel(df_mom[,-1], indice$IBX, nefin$Risk_free)

## Painel D
df_quality <- read_csv("Portfolios\\simple_sort_quality_10.csv", col_types = "Dnnnnnnnnnn")
df_quality$LongShort <- df_quality$D1 - df_quality$D10

tb4_painel_d <- funcao_painel(df_quality[,-1], indice$IBX, nefin$Risk_free)

rm(df_size, df_value, df_mom, df_quality)


# Tabela 5 - Resultados dos portfólios formados por ordenamento duplo ----

## Painel A
df_size_vol <- read_csv("Portfolios\\double_sort_size.csv", col_types = "Dnnnnnnnnnn")
df_size_vol$LongShort <- df_size_vol$D1 - df_size_vol$D10

tb5_painel_a <- funcao_painel(df_size_vol[,-1], indice$IBX, nefin$Risk_free)
tb5_painel_a <- tb5_painel_a[ ,-11]

## Painel B
df_value_vol <- read_csv("Portfolios\\double_sort_value.csv", col_types = "Dnnnnnnnnnn")
df_value_vol$LongShort <- df_value_vol$D1 - df_value_vol$D10

tb5_painel_b <- funcao_painel(df_value_vol[,-1], indice$IBX, nefin$Risk_free)
tb5_painel_b <- tb5_painel_b[ ,-11]

## Painel C
df_mom_vol <- read_csv("Portfolios\\double_sort_mom.csv", col_types = "Dnnnnnnnnnn")
df_mom_vol$LongShort <- df_mom_vol$D1 - df_mom_vol$D10

tb5_painel_c <- funcao_painel(df_mom_vol[,-1], indice$IBX, nefin$Risk_free)
tb5_painel_c <- tb5_painel_c[ ,-11]

## Painel D
df_quality_vol <- read_csv("Portfolios\\double_sort_quality.csv", col_types = "Dnnnnnnnnnn")
df_quality_vol$LongShort <- df_quality_vol$D1 - df_quality_vol$D10

tb5_painel_d <- funcao_painel(df_quality_vol[,-1], indice$IBX, nefin$Risk_free)
tb5_painel_d <- tb5_painel_d[ ,-11]

rm(df_size_vol, df_value_vol, df_mom_vol, df_quality_vol)

# Tabela 6 – t-valor da comparação entre IS ----

## Painel A: Tamanho
s_size10 <- read_csv("Portfolios\\simple_sort_size_10.csv", col_types = "Dnnnnnnnnnn")
d_size10 <- read_csv("Portfolios\\double_sort_size.csv", col_types = "Dnnnnnnnnnn")

tb6_painel_a <- as.data.frame(matrix(nrow = (ncol(s_size10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_painel_a)) {
  tb6_painel_a[i, 1] <- estat_jkm(d_size10[,i+1, drop = TRUE], s_size10[,i+1, drop = TRUE], indice$IBX, nefin$Risk_free)
}

rm(s_size10, d_size10)

## Painel B: Valor
s_value10 <- read_csv("Portfolios\\simple_sort_value_10.csv", col_types = "Dnnnnnnnnnn")
d_value10 <- read_csv("Portfolios\\double_sort_value.csv", col_types = "Dnnnnnnnnnn")

tb6_painel_b <- as.data.frame(matrix(nrow = (ncol(s_value10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_painel_b)) {
  tb6_painel_b[i, 1] <- estat_jkm(d_value10[,i+1, drop = TRUE], s_value10[,i+1, drop = TRUE], indice$IBX, nefin$Risk_free)
}

rm(s_value10, d_value10)

## Painel C: Momentum
s_mom10 <- read_csv("Portfolios\\simple_sort_mom_10.csv", col_types = "Dnnnnnnnnnn")
d_mom10 <- read_csv("Portfolios\\double_sort_mom.csv", col_types = "Dnnnnnnnnnn")

tb6_painel_c <- as.data.frame(matrix(nrow = (ncol(s_mom10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_painel_c)) {
  tb6_painel_c[i, 1] <- estat_jkm(d_mom10[,i+1, drop = TRUE], s_mom10[,i+1, drop = TRUE], indice$IBX, nefin$Risk_free)
}

rm(s_mom10, d_mom10)

## Painel D: Qualidade
s_quality10 <- read_csv("Portfolios\\simple_sort_quality_10.csv", col_types = "Dnnnnnnnnnn")
d_quality10 <- read_csv("Portfolios\\double_sort_quality.csv", col_types = "Dnnnnnnnnnn")

tb6_painel_d <- as.data.frame(matrix(nrow = (ncol(s_quality10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_painel_d)) {
  tb6_painel_d[i, 1] <- estat_jkm(d_quality10[,i+1, drop = TRUE], s_quality10[,i+1, drop = TRUE], indice$IBX, nefin$Risk_free)
}

rm(s_quality10, d_quality10)


# Tabela 7 - Resultados dos portfólios ordenados por Beta ----

## Painel A
df_beta_10 <- read_csv("Portfolios\\simple_sort_beta_10.csv", col_types = "Dnnnnnnnnnn")
df_beta_10$LongShort <- df_beta_10$D1 - df_beta_10$D10

tb7_painel_a <- funcao_painel(df_beta_10[,-1], indice$IBX, nefin$Risk_free)

## Painel B
tb7_painel_b <- lapply(df_beta_10[,-1], function(x) media_max_perda(x))
tb7_painel_b <- do.call("cbind", tb7_painel_b)

tb7_painel_b <- data.frame(tb7_painel_b, media_max_perda(indice$IBX))

colnames(tb7_painel_b) <- c(paste0("D", 1:10), "D1_D10", "Univ")

## Painel C
df_beta_3 <- read_csv("Portfolios\\simple_sort_beta_3.csv", col_types = "Dnnnnnnnnnn")
df_beta_3$LongShort <- df_beta_3$D1 - df_beta_3$D3

tb7_painel_c <- funcao_painel(df_beta_3[,-1], indice$IBX, nefin$Risk_free)

rm(df_beta_10,df_beta_3)

# Tabela 8 e Figura - Trading Strategy ----
trad_strat <- read_csv("Portfolios\\trad_strat.csv", col_types = "Dnnn")

trad_strat$BetaPuro <- trad_strat$BetaPuro + trad_strat$RiskFree
trad_strat$BetaBlume <- trad_strat$BetaBlume + trad_strat$RiskFree

## Figura 
trad_strat_acumul <- apply(trad_strat[,-1], 2, function(x) cumprod(1 + x) - 1)
trad_strat_acumul <- data.frame(Data = trad_strat$Data, trad_strat_acumul)

ggplot() + geom_line(data = trad_strat_acumul, aes(x = Data, y = BetaPuro, colour = "Pure Beta")) + 
  geom_line(data = trad_strat_acumul, aes(x = Data, y = BetaBlume, colour = "Adjusted Beta")) +
  geom_line(data = trad_strat_acumul, aes(x = Data, y = RiskFree, colour = "Risk Free")) + theme_classic() + 
  xlab("") + ylab('') + scale_color_manual(values = c("Pure Beta" = "darkblue", "Adjusted Beta" = "dark green", "Risk Free" = "black")) +
  theme(legend.position="bottom") + theme(legend.title=element_blank())

## Tabela 8
indice_tb8 <- indice %>% dplyr::filter(Data > '2003-12-31')
rf_tb8 <- nefin %>% dplyr::filter(Data > '2003-12-31') %>% select(Data, Risk_free)

estat_beta <- data.frame(estat_ret(trad_strat$BetaPuro, indice_tb8$IBX, rf_tb8$Risk_free),
                         estat_ret(trad_strat$BetaBlume, indice_tb8$IBX, rf_tb8$Risk_free)) %>% set_names('Beta Puro', 'Beta Blume')

trad_strat_xts <- xts(trad_strat[,-1], trad_strat$Data)

max_drawd <- data.frame(round(maxDrawdown(trad_strat_xts$BetaPuro)*100, 2), round(maxDrawdown(trad_strat_xts$BetaBlume)*100, 2), row.names = 'Max. Drawdown') %>% set_names('Beta Puro', 'Beta Blume')

tb8 <- rbind(estat_beta, max_drawd)

rm(estat_beta, indice_tb8, max_drawd, rf_tb8, trad_strat, trad_strat_acumul, trad_strat_xts)

clipr::write_clip(tb8)
