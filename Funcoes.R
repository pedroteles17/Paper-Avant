## Wrapper functions ----

# Retorna uma df ordenada com o indicador para cada ativo
comp_port <- function(i, arg_datas, df_indicador, estrat, ord, restr_exist) {
    comp_data <- arg_datas$comp_data[i]
    estim_inicial <- arg_datas$estim_inicial[i]
    estim_final <- arg_datas$estim_final[i]
    
    dComp_mes <- comp_mes(dbComp, dbRet_ativos, comp_data, restr_exist)
    
    mes_indic(df_indicador, dComp_mes, estim_inicial, estim_final, estrat, ord, dbRet_ind, dbRisk_free)
}

# Retorna uma lista de listas contendo o indicador para cada ativo e para cada data de rebalanceamento e periodo de estim
params_estim <- function(inicio, fim, vetor_hp, vetor_estim, df_indicador, estrat, ord, restr_exist){
    port <- vector("list", length = length(vetor_estim))
    cont <- 1
    
    for(i in vetor_estim){
        port[[cont]] <- suppressWarnings(future_map(1:nrow(argumentos(inicio, fim, 1, i)), comp_port, df_indicador = df_indicador, arg_datas =  argumentos(inicio, fim, 1, i), estrat = estrat, ord = ord, restr_exist = restr_exist))
        cont <- cont + 1
    }
    
    port_hp <- vector("list", length = length(port))
    meses <- interval(ymd(inicio), ymd(fim)) %/% months(1)
    
    for(j in seq_along(port)){
        cont = 1
        for(i in vetor_hp){
            port_hp[[j]][[paste("hp", i, sep = "")]] <- port[[j]][seq(1, meses, by = i)]
            cont = cont + 1
        }
    } 
    
    names(port_hp) <- paste("est", vetor_estim, sep = "")
    
    return(port_hp)
}

# Retorna o retorno do portfolio
backtest <- function(lista_estat, inicio, fim, perc, peso){
    
    n_meses <- interval(ymd(inicio), ymd(fim)) %/% months(1)
    hp <- n_meses %/% (length(lista_estat) - 1) 
    arg_datas <- argumentos(inicio, fim, hp, 6)
    
    ret_comp <- vector("list", length(lista_estat))
    ret_ls <- vector("list", length(lista_estat))
    
    for(i in seq_along(lista_estat)){
        aval_inicial <- arg_datas$aval_inicial[i]
        aval_final <- arg_datas$aval_final[i]
        
        ativos_comp <- ativos_port(lista_estat[[i]], "comprado", perc)
        ret_comp[[i]] <- ret_ativos_port(dbRet_ativos, ativos_comp, aval_inicial, aval_final, peso)
        
        ativos_vend <- ativos_port(lista_estat[[i]], "vendido", perc)
        ret_vend <- ret_ativos_port(dbRet_ativos, ativos_vend, aval_inicial, aval_final, peso)
        
        ret_ls[[i]] <- ret_comp[[i]] - ret_vend
    }
    
    ret_comp <- do.call("rbind.xts", ret_comp)
    ret_ls <- do.call("rbind.xts", ret_ls)
    
    return(cbind.xts(ret_comp, ret_ls))
}


## Funcoes Backtest ----

# Funcao que irá fornecer as datas (inicio_estim, fim_estim, inicio_aval, fim_aval) para o backtest
argumentos <- function(inicio, fim, hold_period, periodo_estim) {
    n_meses <- ceiling(interval(ymd(inicio), ymd(fim)) / months(1)) - 1 #Num de meses entre inicio e fim
    
    comp_data <- as.character(ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period)))
    estim_inicial <- ymd(inicio) %m-% months(periodo_estim) %m+% months(seq(0, n_meses, by = hold_period))
    estim_final <- ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period))
    aval_inicial <- ymd(inicio) %m+% months(seq(0, n_meses, by = hold_period))
    aval_final <- ymd(inicio) %m+% months(hold_period + seq(0, n_meses, by = hold_period))
    
    data.frame(comp_data, estim_inicial, estim_final, aval_inicial, aval_final)
}

# Funcao para encontrar a composicao do indice em determinado mes;
# Eliminamos aqueles ativos que nao estao na base de retornos para evitar divergencias
comp_mes <- function(matriz_comp, df_indicador, mes, restr_exist = 0) {
    diverg <- setdiff(matriz_comp$Ativos, colnames(df_indicador))
    
    comp_mes <- matriz_comp %>%
        dplyr::filter(Ativos %ni% diverg) %>%
        select(Ativos, all_of(mes)) %>%
        set_names("Codigos", "Mes") %>%
        dplyr::filter(Mes == 1)
    ativos_eleg <- comp_mes$Codigos
    
    if(restr_exist > 0){
        inicio_restr <- ymd(mes) %m-% months(restr_exist)
        restr <- df_indicador %>%
            select(Data, all_of(comp_mes$Codigos)) %>%
            dplyr::filter(Data > inicio_restr & Data <= ymd(mes)) %>%
            dplyr::select_if(~ !any(is.na(.))) %>%
            dplyr::select_if(function(x) sum(x != 0) %ni% 0:(restr_exist*21*0.10))
        ativos_eleg <- colnames(restr)[-1]
    }
    return(ativos_eleg)
}


# Retorna uma df ja ordenada com o indicador para cada um dos ativos sejam de preco ou balanco
mes_indic <- function(df_indicador, comp_mes, mes_inicial, mes_final, estrat, ord, ret_fatores = NULL, ret_rf = NULL) {
    if (estrat != "Momentum" & estrat != "Volatilidade" & estrat != "Low Beta" & estrat != "High Alfa" & estrat != "AR" & estrat != "AR1-Rf" & estrat != "Momentum-1" & estrat != "Downside Dev" & estrat != "CVaR") {
        periodo_indicador <- df_indicador %>%
            dplyr::filter(Data == mes_final) %>%
            select(all_of(comp_mes)) %>%
            t() %>%
            as.data.frame() %>%
            drop_na()
    } else {
        ret_ativos_mes <- df_indicador %>%
            dplyr::filter(Data > mes_inicial & Data <= mes_final) %>%
            select(Data, all_of(comp_mes)) %>%
            dplyr::select_if(~ !any(is.na(.))) # Dropamos os ativos que nao possuem dados para toda a amostra
        
        periodo_indicador <- indic_preco(ret_ativos_mes, estrat, ord, ret_fatores, ret_rf)
    }
    colnames(periodo_indicador) <- "indic"
    if (ord == "asc") {
        return(periodo_indicador[order(-periodo_indicador$indic), , drop = FALSE])
    } else {
        return(periodo_indicador[order(periodo_indicador$indic), , drop = FALSE])
    }
}

# Seleciona os ativos que iremos comprar e/ou vender
ativos_port <- function(dMes, tipo_port, percentil) {
    if (tipo_port == "comprado") {
        ativos <- dMes[1:round(nrow(dMes) * percentil), , drop = FALSE]
        return(rownames(ativos))
    } else {
        dMes <- data.frame(indic = rev(dMes$indic), row.names = rev(rownames(dMes)))
        ativos <- dMes[1:round(nrow(dMes) * percentil), , drop = FALSE]
        return(rownames(ativos))
    }
}

# Seleciona os retornos para os ativos que farao parte do portfolio no periodo de avaliacao
ret_ativos_port <- function(ret_ativos, ativos_comp, inicio_aval, fim_aval, tipo_peso) {
    ret_ativos_ponta <- ret_ativos %>%
        select(Data, all_of(ativos_comp)) %>%
        dplyr::filter(Data > inicio_aval & Data <= fim_aval) %>%
        dplyr::select_if(~ !any(is.na(.)))
    
    ret_ativos_ponta <- xts(ret_ativos_ponta[, -1], ret_ativos_ponta$Data)
    
    dRet_port <- ret_port(ret_ativos, ret_ativos_ponta, tipo_peso)
}

# Calcula os retornos do portfolio com base em algum metodo para definir os pesos
ret_port <- function(ret_ativos, ret_ativos_port, tipo_peso) {
    if (tipo_peso == "EW") {
        pesos <- rep(1 / ncol(ret_ativos_port), ncol(ret_ativos_port))
        
        ret_port <- Return.portfolio(ret_ativos_port, weights = pesos)
    }else if (tipo_peso == "HRP-O") {
        nMonths = 6 # meses lookback (6 as per parameters from allocateSmartly)
        nVol = 21 # day lookback for volatility (20 ibid)
        
        inicio_estim_cov <- index(ret_ativos_port)[1] %m-% months(nMonths)
        fim_estim_cov <- index(ret_ativos_port)[1]
        selectedSubset <- ret_ativos %>%
            dplyr::filter(Data > inicio_estim_cov & Data <= fim_estim_cov) %>%
            select(all_of(colnames(ret_ativos_port)))
        
        cors <- cor(selectedSubset) # correlation
        volSubset <- tail(selectedSubset, nVol) # 20 day volatility
        vols <- t(as.matrix(apply(volSubset, 2, sd)))
        covs <- t(vols) %*% vols * cors
        
        # hrp weights
        clustOrder <- hclust(dist(cors), method = 'single')$order
        pesos <- getRecBipart(covs, clustOrder)

        ret_port <- Return.portfolio(ret_ativos_port, weights = pesos)
    }else if (tipo_peso == "HRP") {
        inicio_estim_cov <- index(ret_ativos_port)[1] %m-% months(12)
        fim_estim_cov <- index(ret_ativos_port)[1]
        ret_cov <- ret_ativos %>%
          dplyr::filter(Data > inicio_estim_cov & Data <= fim_estim_cov) %>%
          select(all_of(colnames(ret_ativos_port))) 
        
        pesos <- hierarc_risk_opt(ret_cov)
        ret_port <- Return.portfolio(ret_ativos_port, weights = pesos)
    }else if (tipo_peso == "MV"){
        inicio_estim_cov <- index(ret_ativos_port)[1] %m-% months(12)
        fim_estim_cov <- index(ret_ativos_port)[1]
        ret_cov <- ret_ativos %>%
            dplyr::filter(Data > inicio_estim_cov & Data <= fim_estim_cov) %>%
            select(all_of(colnames(ret_ativos_port))) 
        
        pesos <- pesos_min_var(ret_cov)
        ret_port <- Return.portfolio(ret_ativos_port, weights = pesos)
    }else if (tipo_peso == "VT"){
        inicio_estim_cov <- index(ret_ativos_port)[1] %m-% months(12)
        fim_estim_cov <- index(ret_ativos_port)[1]
        ret_cov <- ret_ativos %>%
            dplyr::filter(Data > inicio_estim_cov & Data <= fim_estim_cov) %>%
            select(all_of(colnames(ret_ativos_port))) 
        
        pesos <- pesos_vol_timing(ret_cov)
        ret_port <- Return.portfolio(ret_ativos_port, weights = pesos)
    }
    
    
    return(ret_port)
}



## Funcoes Auxiliares Backtest ----

# Calcula o indicador para as estrategias de preco;
# Funcao utilizada por "mes_indic"
indic_preco <- function(dMes_preco, estrat, ord, ret_fatores, ret_rf) {
    if (estrat == "Momentum") {
        indicador <- as.data.frame(apply(dMes_preco[, -1], 2, function(x) prod(1 + x) - 1))
    } else if (estrat == "Momentum-1"){
        xts_aux <- xts(dMes_preco[,-1], dMes_preco$Data)
        dMes_preco_1 <- dMes_preco[1:endpoints(xts_aux)[length(endpoints(xts_aux))-1], , drop = FALSE]
        indicador <- as.data.frame(apply(dMes_preco_1[, -1], 2, function(x) prod(1 + x) - 1))
    } else if (estrat == "Volatilidade") {
        indicador <- as.data.frame(apply(dMes_preco[, -1], 2, sd))
    } else if (estrat == "Downside Dev") {
        indicador <- as.data.frame(apply(dMes_preco[, -1], 2, function(x) sqrt(sum(x[x < 0]^2) / length(x))))
    } else if (estrat == "CVaR"){
        xts_aux <- xts(dMes_preco[,-1], dMes_preco$Data)
        indicador <- as.data.frame(apply(dMes_preco[, -1], 2, PerformanceAnalytics::CVaR))
    } else if (estrat == "AR"){
        indicador <- as.data.frame(apply(dMes_preco[, -1], 2, function(x) as.numeric(coef(lm(x[-1] ~ lag(x)[-1]))[1])))
    } else {
        periodo_fatores <- merge(dMes_preco[, 1, drop = FALSE], ret_fatores)
        colnames(periodo_fatores) <- c("Data", "Indice")
        periodo_rf <- merge(dMes_preco[,1, drop = FALSE], ret_rf)
        colnames(periodo_rf) <- c("Data", "Risk_free")
        
        form <- "I(x - periodo_rf$Risk_free) ~ I(periodo_fatores$Indice - periodo_rf$Risk_free)"
        form_ar1 <- "I(x[-1] - periodo_rf$Risk_free[-1]) ~ I(lag(x)[-1] - lag(periodo_rf$Risk_free)[-1])" 
        if (estrat == "Low Beta") {
            indicador <- as.data.frame(apply(dMes_preco[, -1], 2, function(x) as.numeric(coef(lm(as.formula(form)))[2])))
        } else if (estrat == "High Alfa") {
            indicador <- as.data.frame(apply(dMes_preco[, -1], 2, function(x) as.numeric(coef(lm(as.formula(form)))[1])))
        } else if (estrat == "AR1-Rf") {
            indicador <- as.data.frame(apply(dMes_preco[, -1], 2, function(x) as.numeric(coef(lm(as.formula(form_ar1)))[1])))
        }
    }
    return(indicador)
}

# Funcoes auxiliares do Hierarchical Risk Parity
getIVP <- function(covMat) {
    invDiag <- 1/diag(as.matrix(covMat))
    weights <- invDiag/sum(invDiag)
    return(weights)
}
getClusterVar <- function(covMat, cItems) {
    covMatSlice <- covMat[cItems, cItems]
    weights <- getIVP(covMatSlice)
    cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
    return(cVar)
}

getRecBipart <- function(covMat, sortIx) {
    w <- rep(1,ncol(covMat))
    w <- recurFun(w, covMat, sortIx)
    return(w)
}

recurFun <- function(w, covMat, sortIx) {
    subIdx <- 1:trunc(length(sortIx)/2)
    cItems0 <- sortIx[subIdx]
    cItems1 <- sortIx[-subIdx]
    cVar0 <- getClusterVar(covMat, cItems0)
    cVar1 <- getClusterVar(covMat, cItems1)
    alpha <- 1 - cVar0/(cVar0 + cVar1)
    
    # scoping mechanics using w as a free parameter
    w[cItems0] <- w[cItems0] * alpha
    w[cItems1] <- w[cItems1] * (1-alpha)
    
    if(length(cItems0) > 1) {
        w <- recurFun(w, covMat, cItems0)
    }
    if(length(cItems1) > 1) {
        w <- recurFun(w, covMat, cItems1)
    }
    return(w)
}

hierarc_risk_opt <- function(returns) {
    
    train_covMat <- cov(returns)
    train_corMat <- cor(returns)
    
    clustOrder <- hclust(dist(train_corMat), method = 'single')$order
    
    out <- getRecBipart(train_covMat, clustOrder) #Pesos!
}
# Fim Funcoes auxiliares do Hierarchical Risk Parity

# Pesos Mínima Variância
pesos_min_var <- function(returns){
    mcov=cov(returns,use = "pairwise.complete.obs")
    if(det(mcov) == 0){
        pes <- rep(1/ncol(returns), ncol(returns))
    }else{
        wVM=solve(mcov)%*%rep(1,ncol(mcov))
        wVM = as.vector(t(wVM))
        for(i in seq_along(wVM)){
            if(wVM[i] < 0){
                wVM[i] <- 0
            }
        }
        pes=wVM/sum(wVM)
    }
    
    return(pes)
}

# Pesos Volatility Timing
pesos_vol_timing <- function(returns){
    mcov=cov(returns,use = "pairwise.complete.obs")
    vari =diag(mcov)
    eta=4
    wVT=(1/vari)^eta/sum((1/vari)^eta) # sem restricoes
    as.vector(t(wVT))
}



## Resultados ----

# Retorna o retorno anualizado de cada decil da nossa df ordenada
backtest_decil <- function(lista_estat, inicio, fim, n, peso){
    
    n_meses <- interval(ymd(inicio), ymd(fim)) %/% months(1)
    hp <- n_meses %/% (length(lista_estat) - 1) 
    arg_datas <- argumentos(inicio, fim, hp, 6)
    
    ret_decil <- vector("list", length = n)
    for(i in seq_along(lista_estat)){
        df <- lista_estat[[i]]
        
        x <- 1:nrow(df) 
        split_seq <- split(x, cut(x, n, labels = FALSE))        
        
        aval_inicial <- arg_datas$aval_inicial[i]
        aval_final <- arg_datas$aval_final[i]
        
        for (j in 1:n) {
            ativos_decil <- rownames(df[split_seq[[j]], , drop = FALSE])
            ret_decil[[j]][[i]] <- ret_ativos_port(dbRet_ativos, ativos_decil, aval_inicial, aval_final, peso)
        }
    }
    
    
    return(ret_decil)
}


tabela_anos <- function(ret_port, ano_inicio, ano_fim) {
    anos <- as.character(seq(from = ano_inicio, to = ano_fim, by = 1))
    estrat_anos <- numeric(length(anos))
    for (i in 1:length(anos)) {
        ret_port_ano <- ret_port[anos[i]]
        estrat_anos[i] <- prod(1 + ret_port_ano) - 1
    }
    
    tabela_anos <- data.frame(Estrat = paste(round(estrat_anos * 100, 2), "%", sep = "")) %>%
        set_names(colnames(ret_port))
    row.names(tabela_anos) <- anos
    
    return(tabela_anos)
}






