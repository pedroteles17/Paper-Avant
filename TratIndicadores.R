library(tidyverse)
library(readxl)
library(xts)
library(lubridate)
library(writexl)

nome_indic <- c("value", "quality", "size")

for (i in seq_along(nome_indic)) {
  indic <- read_excel("IndicBalancoRaw.xlsx", sheet = i+1)
  
  colnames(indic)[1] <- "Data"
  
  indic <- indic[-1, ]
  
  if(nome_indic[i] == "size"){
    indic <- indic[-1, ]
  }
  
  indic$Data <-  as.Date(as.numeric(indic$Data), origin = "1899-12-30")
  
  indic <- data.frame(Data = indic$Data, apply(indic[,-1], 2, as.numeric)) %>% set_names(colnames(indic))
  
  indic <- indic[order(indic$Data), ]
  
  indic1 <- indic[rowSums(is.na(indic[,-1])) != ncol(indic[,-1]), colSums(is.na(indic)) != nrow(indic)]
  
  print(nome_indic[i])
  print(paste("Dif de linhas: ", nrow(indic) -  nrow(indic1)))
  print(paste("Dif de colunas: ", ncol(indic) -  ncol(indic1)))
  
  file_name <- paste0(getwd(), "/Brasil")
  
  write.csv(indic1, paste(file_name, paste(nome_indic[i], ".csv"), sep = "\\"), row.names=FALSE)
}
