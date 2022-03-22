
dados_bbg <- read_excel( "C:/Users/Pedro/Desktop/Avantgarde/Paper Luciano\\low vol.xlsx", col_types = c("date", rep("numeric", 6)))

# Dados port vol
dados_pd <- read_csv("Portfolios\\simple_sort_vol_3.csv", col_types = "Dnnn")

apply(dados_pd[,-1], 2, function(x) prod(1 + x) ^ (252 / length(x)) - 1)

dados_pd <- xts(dados_pd[,-1], dados_pd$Data)

dados_pd_mes <- list()
for (i in 1:ncol(dados_pd)) {
  dados_pd_mes[[i]] <- apply.monthly(dados_pd[,i], function(x) prod(1+x)-1)
}

dados_pd_mes <- do.call("cbind.xts", dados_pd_mes)

dados_pd_mes <- data.frame(Data = index(dados_pd_mes), dados_pd_mes) %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31")

# ibx ew
ibx_ew <- read_csv("Portfolios\\ibx_ew.csv", col_types = "Dn")

ibx_ew <- xts(ibx_ew[,-1], ibx_ew$Data)

ibx_ew_mes <- list()
for (i in 1:ncol(ibx_ew)) {
  ibx_ew_mes[[i]] <- apply.monthly(ibx_ew[,i], function(x) prod(1+x)-1)
}

ibx_ew_mes <- do.call("cbind.xts", ibx_ew_mes)

ibx_ew_mes <- data.frame(Data = index(ibx_ew_mes), ibx_ew_mes) %>%
  dplyr::filter(Data >= "2003-01-01" & Data <= "2021-12-31")

# Correl

cor(dados_bbg$LowVol, dados_pd_mes$D1)
cor(dados_bbg$MidVol, dados_pd_mes$D2)
cor(dados_bbg$HighVol, dados_pd_mes$D3)
cor(dados_bbg$IBX_EW, ibx_ew_mes$IBX_EW)

c(prod(1 + dados_bbg$LowVol)^(12 / length(dados_bbg$LowVol) - 1), prod(1 + dados_pd_mes$D1)^(12 / length(dados_pd_mes$D1) - 1))
c(prod(1 + dados_bbg$MidVol)^(12 / length(dados_bbg$MidVol) - 1), prod(1 + dados_pd_mes$D2)^(12 / length(dados_pd_mes$D2) - 1))
c(prod(1 + dados_bbg$HighVol)^(12 / length(dados_bbg$HighVol) - 1), prod(1 + dados_pd_mes$D3)^(12 / length(dados_pd_mes$D3) - 1))
