library(tidyverse)
library(readxl)
library(xts)
library(lubridate)
library(writexl)


preco_raw <- read_xlsx("Tratamento dos Dados\\PrecoRaw.xlsx", sheet = 2)
  
preco_raw <- preco_raw[-1, ]
colnames(preco_raw)[1] <- "Data"

preco_raw$Data <- as.Date(as.numeric(preco_raw$Data), origin="1899-12-30")

preco_raw <- preco_raw[order(preco_raw$Data), ]

preco_raw <- data.frame(Data = preco_raw$Data, sapply(preco_raw[,-1], as.numeric)) %>% set_names(colnames(preco_raw))
  
ind <- read_xlsx("Tratamento dos Dados\\PrecoRaw.xlsx", sheet = 3)
ind <- ind[-1:-3, ]
colnames(ind)[1] <- "Data"
ind$Data <- as.Date(as.numeric(ind$Data), origin="1899-12-30")
ind <- ind[order(ind$Data), ]
ind$`IBX Index` <- as.numeric(ind$`IBX Index`)
ind <- ind %>% drop_na() %>% dplyr::filter(Data > "2000-12-31" & Data <= "2021-12-31")
  
###############################################################################
  
precos <- merge(ind[,-2], preco_raw, by = "Data", all.x = TRUE)
precos <- precos[ ,colSums(is.na(precos)) != nrow(precos)]
  
# Realiamos iterações para preencher os NAs no meio da amostra
precos_locf <- data.frame(matrix(ncol = ncol(precos) - 1, nrow = nrow(precos)))
for(i in 2:ncol(precos)){
  ind_data <- ind[,1]
  # Selecionamos a data e a coluna do fundo para iterações ("i")
  suporte1 <- precos[,c(1,i)]
  # Criamos um vetor que informa a posição das observações que não são NA
  NonNAindex <- which(!is.na(suporte1[,2]))
  # Filtramos para podermos trabalhar apenas com as datas antes de um ativo, possivelmente, deslistar. Isso evitará problemas na hora de utilizar a função na.locf() 
  suporte2 <- suporte1[1:max(NonNAindex),]
  # Usamos a função na.locf para substituir as observações faltantes pela observação anterior
  suporte3 <- na.locf(suporte2)
  precos_locf[[i-1]] <- merge(ind_data, suporte3, by = "Data", all.x = TRUE)[,2]
}

retornos <- as.data.frame(lapply(precos_locf, function(x) diff(x)/x[-length(x)])) %>% set_names(colnames(precos)[-1])

retornos <- retornos %>% dplyr::mutate(Data = precos$Data[-1], .before = 1)

# Ajustamos os dados da taxa livre de risco para coincidir com as datas do índice  
rf <- read_xlsx("Tratamento dos Dados\\CDI.xlsx") %>% dplyr::select(Data, CDI) %>% set_names('Data', 'Risk_free')
rf$Data <- as.Date(rf$Data)
rf <- rf[order(rf$Data), ]
  
rf1 <- merge(ind[,1], rf, by = "Data", all.x = TRUE)
rf1$Risk_free <- na.locf(rf1$Risk_free)
rf1 <- data.frame(Data = rf1$Data[-1], Risk_free = diff(rf1$Risk_free)/rf1$Risk_free[-length(rf1$Risk_free)])
  
# Calculamos o retorno diário do índice
ind[,2] <- append(diff(ind[,2, drop = TRUE])/ind[,2, drop = TRUE][-length(ind[,2, drop = TRUE])], NA, after = 0)
ind <- ind[-1, ]  

file_name <- paste0(getwd(), "/Brasil")
  
write.csv(rf1, paste0(file_name, "\\rf.csv"), row.names=FALSE)
write.csv(retornos, paste0(file_name, "\\ativos.csv"), row.names=FALSE)
write.csv(ind, paste0(file_name, "\\indice.csv"), row.names=FALSE)



