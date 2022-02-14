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


dados <- readr::read_csv("Portfolios\\simple_sort_vol_3.csv") %>% 
  set_names(c("Data", "LowVol", "MidVol", "HighVol"))

dados$LongShort <- dados$LowVol - dados$HighVol

nefin <- read_excel("nefin.xlsx", col_types = c("date", rep("numeric", 6))) %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31")

indice <- read_csv("Brasil\\indice.csv", col_types = paste(c("D", "n"), collapse = "")) %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31") %>% 
  set_names(c("Data", "IBX"))

# Funções ----

jobson_korkie_memmel <- function(shrp1, shrp2, corel, n_obs){
  z <- (shrp1 - shrp2) / sqrt((1 / n_obs) * (2*(1 - corel) + 0.5 * (shrp1^2 + shrp2^2 - shrp1 * shrp2 * (1 + corel^2)))) 
  return(z)
}

estat_ret <- function(ret_port, ind, rf, nome_col) {
  
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
  SR <- ret_period((ret_port - rf)) / vol
  SR_ind <- ret_period((ind - rf)) / vol
  
  t_SR <- jobson_korkie_memmel(SR, SR_ind, cor(ret_port, ind), length(ret_port))
  
  rp_rf <- ret_period(ret_port - rf)
  
  
  resultados <- data.frame(c(
    ret_acumul, vol, SR, t_SR,
    rp_rf, beta, alfa, t_alfa
  )) %>% set_names(nome_col)
  
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
  )) %>% set_names(nome_col)
  
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

# Figura 1 – Plotagem das carteiras ordenadas por volatilidade ----

vetor_betas <- vector(length = 3)
for (i in seq_along(vetor_betas)) {
  vetor_betas[i] <- coef(lm(I(dados[,i+1, drop = TRUE] - nefin$Risk_free) ~ I(indice$IBX - nefin$Risk_free)))[2]
}

port_betas_ret <- data.frame(Nome = c(paste0("P", 1:3)),
                             Beta = vetor_betas,
                             Ret_acumul = apply(dados[,c(2, 3, 4)], 2, function(x) prod(1+x)^(252/length(x)) - 1))

port_betas_ret <- rbind(port_betas_ret, c("IBX", 1, prod(1+indice$IBX)^(252/length(indice$IBX)) - 1))

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

#grid::grid.locator()

#grid.brackets(232, 80, 232, 247, lwd=0.5, col="blue", type = 2)
#grid.brackets(690, 345, 690, 169, lwd=0.5, col="blue", type = 2)
#grid.text(x=unit(555,'native'), y=unit(250,'native'), label=expression("Alfa negativo"), hjust = 0, vjust=0)
#grid.text(x=unit(555,'native'), y=unit(275,'native'), label=expression("por ações de"), hjust = 0, vjust=0)
#grid.text(x=unit(555,'native'), y=unit(295,'native'), label=expression("alta volat."), hjust = 0, vjust=0)
#grid.text(x=unit(275,'native'), y=unit(150,'native'), label=expression("Alfa positivo"), hjust = 0, vjust=0)
#grid.text(x=unit(275,'native'), y=unit(175,'native'), label=expression("por ações de"), hjust = 0, vjust=0)
#grid.text(x=unit(275,'native'), y=unit(195,'native'), label=expression("baixa volat."), hjust = 0, vjust=0)

rm(i, reg_mod_coef, sml, vetor_betas, port_betas_ret, ret_rf, ret_mercado_rf)




# Tabela 1 – Resultados dos portfólios de baixa volatilidade ----

## Painel C

tb1_painel_c <- funcao_painel(dados)

## Painel B

tb1_painel_b <- data.frame(media_max_perda(dados$LowVol, "D1"),
                           media_max_perda(dados$MidVol, "D2"),
                           media_max_perda(dados$HighVol, "D3"),
                           media_max_perda(dados$LongShort, "D1-D3"),
                           media_max_perda(dados$IBX, "Univ."))




# Tabela 2 – Divisão em períodos ----

dados_03_08 <- dados %>% dplyr::filter(Data >= "2003-01-01" & Data <= "2008-12-31")

dados_09 <- dados %>% dplyr::filter(Data >= "2009-01-01" & Data <= "2009-12-31")

dados_10_21 <- dados %>% dplyr::filter(Data >= "2010-01-01" & Data <= "2021-12-31")

## Painel A
tb2_painel_a <- funcao_painel(dados_03_08)

## Painel B
tb2_painel_b <- funcao_painel(dados_09)

## Painel C
tb2_painel_c <- funcao_painel(dados_10_21)

rm(dados_03_08, dados_09, dados_10_21)




# Tabela 3 – Regressão nos modelos multi-fatoriais ----

## Painel A
tb3_painel_a <- data.frame(analise_fatores(dados$LowVol, "Alfa", "Menores Vol"),
                           analise_fatores(dados$MidVol, "Alfa", "Intermediaria"),
                           analise_fatores(dados$HighVol, "Alfa", "Maiores Vol"))

## Painel B
tb3_painel_b <- data.frame(analise_fatores(dados$LowVol, "t", "Menores Vol"),
                           analise_fatores(dados$MidVol, "t", "Intermediaria"),
                           analise_fatores(dados$HighVol, "t", "Maiores Vol"))

clipr::write_clip(tb3_painel_b)

