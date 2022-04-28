############################################################################
############################################################################
###                                                                      ###
###                              SECTION 0:                              ###
###                             INTRODUCTION                             ###
###                                                                      ###
############################################################################
############################################################################

# code author: Pedro Teles (pteles@avantgardeam.com.br)

"%ni%" <- Negate("%in%")

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                             IMPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

# Function to import the table with the financial indicators
base_indic <- function(path2file){
  indic_import <- read_csv(path2file)
  # Transform date to date and the indicators to numeric
  indic <- data.frame(as.Date(indic_import$Date), apply(indic_import[,-1], 2, as.numeric))
  colnames(indic) <- colnames(indic_import)
  
  return(indic)
}

# To export our xts data we need to transform it to a data frame object
xts2df <- function(xts_object){
  df <- data.frame(Date = index(xts_object), xts_object)
}

# Prepare for parallel processing
plan(multisession, workers = parallel::detectCores())

# Import functions needed for running the backtests
source("99_functions.R")

# Where are the cleaned data stored?
path2file <- paste(getwd(), "brazil", sep = "/")

# Import index composition
## We need to eliminate "RIPI4 BS Equity" because of a data quality issue:
## On the day 2004-06-03 the price jumps from R$0.0078 to R$7.78
## We don't have price data for "PRGA4 BS Equity"
comp <- read_csv(paste(path2file, "comp.csv", sep = "\\"), col_types = paste0("c", paste(rep("n", 241), collapse = ""), collapse = ""))  %>% 
  dplyr::filter(Assets %ni% c("RIPI4 BS Equity", "PRGA4 BS Equity"))

# Import the IBX Index returns
ret_ind <- read_csv(paste(path2file, "index_returns.csv", sep = "\\"), col_types = "Dn")

# Import the risk free return (CDI)
ret_risk_free <- read_csv(paste(path2file, "risk_free_returns.csv", sep = "\\"), col_types = "Dn")

# Import the assets returns
ret_assets <- read_csv(paste(path2file, "asset_returns.csv", sep = "\\"), col_types = paste0("D", paste(rep("n", 304), collapse = ""), collapse = ""))

# Get the backtests dates. This will be useful for seeing how many times 
## we need to rebalance the portfolios 
backtest_dates <- get_backtest_dates(20021231, 20211231, c(1), c(12))

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                 GET THE ORDERED FINANCIAL INDICATORS                 ###
###                                                                      ###
############################################################################
############################################################################

# Low Volatility
ord_indic_vol <- future_map(1:nrow(backtest_dates), get_ord_indic,
                           start_date = 20021231, end_date = 20211231,
                           hold_period = c(1), estim_period = c(12),
                           fin_indic = ret_assets, comp_matrix = comp,
                           factor_name = 'Low Volatility', order = 'ascending')

# Momentum
ord_indic_mom <- future_map(1:nrow(backtest_dates), get_ord_indic,
                           start_date = 20021231, end_date = 20211231,
                           hold_period = c(1), estim_period = c(6),
                           fin_indic = ret_assets, comp_matrix = comp,
                           factor_name = 'Momentum', order = 'descending')

# Value
value <- base_indic(paste0(path2file, '\\value.csv'))

ord_indic_value <- future_map(1:nrow(backtest_dates), get_ord_indic,
                            start_date = 20021231, end_date = 20211231,
                            hold_period = c(1), estim_period = c(0), # estim_period won't be used
                            fin_indic = value, comp_matrix = comp,
                            factor_name = 'Value', order = 'ascending')

# Size
size <- base_indic(paste0(path2file, '\\size.csv'))
## We make sure that the dates correspond to the last day of a month
size$Date <- as.Date(as.Date(size$Date[1]) %m+% months(0:(nrow(size) - 1)))

## We don't have market cap information for two assets.
### We drop them for the size factor.
diff_size_comp <- setdiff(comp$Assets, colnames(size))
comp_size <- comp %>% dplyr::filter(Assets %ni% diff_size_comp)

ord_indic_size <- future_map(1:nrow(backtest_dates), get_ord_indic,
                            start_date = 20021231, end_date = 20211231,
                            hold_period = c(1), estim_period = c(0),
                            fin_indic = size, comp_matrix = comp_size,
                            factor_name = 'Size', order = 'ascending')

# Quality
quality <- base_indic(paste0(path2file, '\\quality.csv'))

ord_indic_quality <- future_map(1:nrow(backtest_dates), get_ord_indic,
                            start_date = 20021231, end_date = 20211231,
                            hold_period = c(1), estim_period = c(0),
                            fin_indic = quality, comp_matrix = comp,
                            factor_name = 'Quality', order = 'descending')

rm(value, size, diff_size_comp, comp_size, quality)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 2:                             ###
###                          RUN THE BACKTESTS                          ###
###                                                                     ###
###########################################################################
###########################################################################

##################################################################
##        Proprietary Factors Portfolios for Regression         ##
##################################################################

# Here we will build the proprietary factors that are going to be used for
## the factor regressions.

ordered_regres <- list(ord_indic_size, ord_indic_value, ord_indic_mom, ord_indic_quality)
names(ordered_regres) <- c('Size', 'Value', 'Momentum', 'Quality')

port_regres <- vector('list', length = length(ordered_regres))
for (i in seq_along(ordered_regres)) {
  
  if(names(ordered_regres)[i] == 'Size'){
    percent <- 0.5
  } else {
    percent <- 0.3
  }
  
  # We select only the second column because it's the Long e Short portfolio
  port_regres[[i]] <- run_backtest(ret_assets, ordered_regres[[i]], 20021231, 20211231, percent)[,2]
}

port_regres <- do.call('cbind.xts', port_regres)

colnames(port_regres) <- c('SMB', 'HML', 'WML', 'QMJ')

rm(ordered_regres, percent)


##################################################################
##                       Three Portfolios                       ##
##################################################################

# We only need portfolios of the type 0.3, 0.4, 0.3* for the low vol factor 
## *Buy 30% with lower vol, 40$% core, 30% higher vol

# Low Volatility
ret_port_vol_3 <- run_backtest_three_ports(ret_assets, ord_indic_vol, 20021231, 20211231, 0.3)
colnames(ret_port_vol_3) <- paste0("D", seq(1,3))

## Trading Strategy
trad_strat <- xts2df(ret_port_vol_3)
trad_strat <- trad_strat %>% dplyr::select(Date, D1, D3) %>% set_names('Date', 'Long', 'Short')


##################################################################
##                        Ten Portfolios                        ##
##################################################################

ordered_factor <- list(ord_indic_size, ord_indic_value, ord_indic_mom, ord_indic_quality)
names(ordered_factor) <- c('SMB', 'HML', 'WML', 'QMJ') # RMS: Risky Minus Safe

port_factor_10 <- vector('list', length = length(ordered_factor))
for (i in seq_along(ordered_factor)) {
  
  port_factor_10[[i]] <- run_backtest_decile(ret_assets, ordered_factor[[i]], 20021231, 20211231, 10)

  names(port_factor_10[[i]]) <- paste0("D", seq(1,10))
}

# RMS: Risky Minus Safe (Low Volaitility)
names(port_factor_10) <- c('SMB', 'HML', 'WML', 'QMJ')

rm(ordered_factor)

##################################################################
##                        Double Sorting                        ##
##################################################################

## Here we first sort the assets based on one of the four factors
### 'SMB', 'HML', 'WML', 'QMJ' and then sort based on low volatility

ord_fact_double <- list(ord_indic_size, ord_indic_value, ord_indic_mom, ord_indic_quality)
names(ord_fact_double) <- c('SMB', 'HML', 'WML', 'QMJ') 

port_double_sort <- vector('list', length = length(ord_fact_double))
for (i in seq_along(ord_fact_double)) {
  
  if(names(ord_fact_double)[i] == 'Size'){
    percent <- 0.5
  } else {
    percent <- 0.3
  }
  
  # We select only the second column because it's the Long e Short portfolio
  port_double_sort[[i]] <- run_backtest_double_sort(ret_assets, ord_fact_double[[i]], ord_indic_vol, 20021231, 20211231, percent)
  
  colnames(port_double_sort[[i]]) <- paste0("D", seq(1,10))
}

names(port_double_sort) <- c('SMB', 'HML', 'WML', 'QMJ')

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 3:                              ###
###                           TRADING STRATEGY                           ###
###                                                                      ###
############################################################################
############################################################################

## First we make sure that the dates are the same for all databases
index <- ret_ind %>%
  dplyr::filter(Date >= "2003-01-01" & Date <= "2021-12-31") %>% 
  set_names(c("Date", "IBX"))
rf <- ret_risk_free %>%
  dplyr::filter(Date >= "2003-01-01" & Date <= "2021-12-31") %>% 
  set_names(c("Date", "Risk_free"))

trad_strat$Risk_free <- rf$Risk_free

trad_strat$IBX <- index$IBX

dates_beta <- get_backtest_dates(20031231, 20211231, 1, 12)[,-1]

ret_ports_trad_strat <- vector('list', length = nrow(dates_beta))
for (i in 1:nrow(dates_beta)) {
  ## Select the dates needed to run the backtest
  dates <- dates_beta[i, , drop = TRUE]
  
  trad_strat_estim <- trad_strat %>% dplyr::filter(Date > dates[[1]] & Date <= dates[[2]])
  
  ## Extract the beta of a regression of the long and the short portfolio against IBX
  beta_long <- as.numeric(coef(lm(I(trad_strat_estim$Long - trad_strat_estim$Risk_free) ~ I(trad_strat_estim$IBX - trad_strat_estim$Risk_free)))[2])
  beta_short <- as.numeric(coef(lm(I(trad_strat_estim$Short - trad_strat_estim$Risk_free) ~ I(trad_strat_estim$IBX - trad_strat_estim$Risk_free)))[2])
  
  ## We calculate what should be the short exposure. Meaning
  ### we will be 100% long and X% short. X% will be the short exposure
  short_exp <- beta_long / beta_short
  
  ## We calculate the blume beta
  beta_long_blume <- (beta_long * 2/3) + (1 * 1/3)
  beta_short_blume <- (beta_short * 2/3) + (1 * 1/3)
  
  short_exp_blume <- beta_long_blume / beta_short_blume
  
  ## We filter our df to have only the evaluation period
  trad_strat_aval <- trad_strat %>% dplyr::filter(Date > dates[[3]] & Date <= dates[[4]]) %>%
    select(Date, Long, Short, Risk_free)
  
  trad_strat_aval <- xts(trad_strat_aval[,-1], trad_strat_aval$Date)
  
  ## Calculate the Long and Short portfolio return
  ### 100% long, -X% Short, X% Long Risk free
  weights <- c(1, -short_exp, short_exp)
  ret_beta <- Return.portfolio(trad_strat_aval, weights = weights)
  
  ## Calculate the blume Long Short portfolio return
  weights <- c(1, -short_exp_blume, short_exp_blume)
  ret_beta_blume <- Return.portfolio(trad_strat_aval, weights = weights)
  
  ## Calculate the common Long e Short portfolio
  weights <-c(1, -1, 1)
  ret_ls <- Return.portfolio(trad_strat_aval, weights = weights)
  
  # Bind the portfolios
  ret_ports_trad_strat[[i]] <- cbind.xts(cbind.xts(ret_beta, ret_beta_blume), ret_ls)
  
  colnames(ret_ports_trad_strat[[i]]) <- c('Beta', 'BetaBlume', 'OrigLS')
}

ret_ports_trad_strat <- do.call('rbind.xts', ret_ports_trad_strat)

rm(index, rf, dates_beta, dates, trad_strat_estim, beta_long, beta_short, short_exp,
   beta_long_blume, beta_short_blume, short_exp_blume, trad_strat_aval, ret_beta, 
   ret_beta_blume, ret_ls)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###                             EXPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

file_name <- paste0(getwd(), '/portfolios')

# Comment if you already have this directory
dir.create(file_name)

# Export Proprietary Factors Portfolios for Regression
write.csv(xts2df(port_regres), paste0(file_name, "\\proprietary_factors.csv"), row.names = FALSE)

# Three Portfolios
write.csv(xts2df(ret_port_vol_3), paste0(file_name, "\\simple_sort_vol_3.csv"), row.names = FALSE)
write.csv(xts2df(trad_strat), paste0(file_name, "\\trad_strat.csv"), row.names = FALSE)

# Ten Portfolios
write.csv(xts2df(port_factor_10[['SMB']]), paste0(file_name, "\\simple_sort_size_10.csv"), row.names = FALSE)
write.csv(xts2df(port_factor_10[['HML']]), paste0(file_name, "\\simple_sort_value_10.csv"), row.names = FALSE)
write.csv(xts2df(port_factor_10[['WML']]), paste0(file_name, "\\simple_sort_mom_10.csv"), row.names = FALSE)
write.csv(xts2df(port_factor_10[['QMJ']]), paste0(file_name, "\\simple_sort_quality_10.csv"), row.names = FALSE)

# Double Sorting
write.csv(xts2df(port_double_sort[['SMB']]), paste0(file_name, "\\double_sort_size.csv"), row.names = FALSE)
write.csv(xts2df(port_double_sort[['HML']]), paste0(file_name, "\\double_sort_value.csv"), row.names = FALSE)
write.csv(xts2df(port_double_sort[['WML']]), paste0(file_name, "\\double_sort_mom.csv"), row.names = FALSE)
write.csv(xts2df(port_double_sort[['QMJ']]), paste0(file_name, "\\double_sort_quality.csv"), row.names = FALSE)

# Trading Strategy
write.csv(xts2df(ret_ports_trad_strat), paste0(file_name, "\\trad_strat.csv"), row.names = FALSE)

# Clean all variables so we can run the next code (graphs_tables.R)
rm(list=ls()) 
