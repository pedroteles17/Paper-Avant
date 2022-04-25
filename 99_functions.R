
############################################################################
############################################################################
###                                                                      ###
###                              SECTION 0:                              ###
###                             INTRODUCTION                             ###
###                                                                      ###
############################################################################
############################################################################

# code author: Pedro Teles (pteles@avantgardeam.com.br)

# This is not a script. These are the set of functions that will be used to run
# the backtests. So this file will be called (`source()`) from an R Script that
# will be responsible for effectively running the backtests and generating
# the results.

############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                          BACKTEST FUNCTIONS                          ###
###                                                                      ###
############################################################################
############################################################################

##################################################################
##                      get_backtest_dates                      ##
##################################################################

## For a backtest, we need some dates like rebalance frequency and estimation
## period if it's a price factor (momentum, low volatility, e.g.).
## This function will return a data frame containing all these dates

## start_date: YYYYMMDD when our backtest starts
## end_date: YYYYMMDD when our backtest ends
## hold_period: rebalance frequency
### (monthly = 1, quarterly = 3, semiannual = 6, annual = 12)
## estim_period: how long should we look back to calculate return statistics?
### (number of months) (estimation period)

get_backtest_dates <- function(start_date, end_date, hold_period, estim_period) {
  # Number of months between the start and end date
  n_months <- ceiling(interval(ymd(start_date), ymd(end_date)) / months(1)) - 1

  # Last available data before we do the rebalancing
  end_estim <- ymd(start_date) %m+% months(seq(0, n_months, by = hold_period))
  # estimation end date minus number of months we look back to calculate return statistics
  start_estim <- ymd(start_date) %m-% months(estim_period) %m+% months(seq(0, n_months, by = hold_period))

  # After the estimation ends, we start the holding period
  start_hold_period <- end_estim
  # start holding period plus the rebalance frequency
  end_hold_period <- ymd(start_date) %m+% months(hold_period + seq(0, n_months, by = hold_period))

  # What index composition should we consider to select the available assets
  comp_date <- as.character(end_estim)

  backtest_dates <- data.frame(comp_date, start_estim, end_estim, start_hold_period, end_hold_period)

  return(backtest_dates)
}

##################################################################
##                     get_index_compostion                     ##
##################################################################

##  When we are rebalancing, we need to know what assets are available in that month.
##  That's what this function does.

## comp_matrix: table containing the index compostion every month.
### Columns: months; rows: assets.
## fin_indic: a table containing the assets' financial indicators.
### If we are working with size, for example, this could be a table
### containing market cap information for every asset and every month (financial indicators)
## month: for which month would we like to extract the available assets?

get_index_composition <- function(comp_matrix, fin_indic, month) {
  # Which assets are in the index composition table but
  # are not in the financial indicators table.
  diverg <- setdiff(comp_matrix$Assets, colnames(fin_indic))

  monthly_comp <- comp_matrix %>%
    # Assets that are in both data tables
    dplyr::filter(Assets %ni% diverg) %>%
    # Select assets names and the indication of presence (1 or 0) for that month
    select(Assets, all_of(month)) %>%
    set_names("Assets", "Month") %>%
    # Select only those assets that are in the index composition that month
    dplyr::filter(Month == 1)

  index_comp <- monthly_comp$Assets

  return(index_comp)
}

##################################################################
##                   get_financial_indicators                   ##
##################################################################

## In a factor portfolio, for every rebalance date we need to calculate the
## statistics  and sort the available assets by this statistic.
## This function does that and returns an ordered data frame
## containing the assets' names and the statistics values.

## fin_indic: a table containing the assets' financial indicators (financial indicators)
## index_comp: a vector containing the eligible assets for a rebalance date.
### It's an output from the 'get_index_composition' function.
## start_estim: if it's a price factor (momentum, low volatility, e.g.),
### what date should we start considering for calculating the return statistics? (start estimation)
## end_estim: if it's a balance sheet factor (value, size, e.g.), what date
### should we select to pick the financial indicator information? If it's a price (end estimation)
### factor, what date should we stop considering for calculating the return statistics?
## factor_name: what's the factor name (value, size, momentum, e.g.)?
## order: ascending or descending? If the factor is size, for example,
### and we are using the market cap, we would like to ascending order.
## factor_returns: if we are working with low beta, for example, we also need
### the factor returns (market factor. e.g.)
## risk_free_return: if we are working with low beta, for example, we also need
### the risk free returns

get_financial_indicators <- function(fin_indic, index_comp, start_estim, end_estim, factor_name, order, factor_returns = NULL, risk_free_return = NULL) {
  # If it's a price factor, we need to use the returns to calculate the desired
  ## financial indicator
  if (factor_name == "Momentum" | factor_name == "Low Volatility" | factor_name == "Low Beta" | factor_name == "Momentum-1") {
    assets_return_period <- fin_indic %>%
      dplyr::filter(Date > start_estim & Date <= end_estim) %>%
      # Select only the date column and the eligible assets
      select(Date, all_of(index_comp)) %>%
      # Drop assets that don't have any return information for the period
      dplyr::select_if(~ !any(is.na(.)))

    indic <- calculate_price_indic(assets_return_period, factor_name, factor_returns, risk_free_return)
  } else { # it's a balance sheet factor

    indic <- fin_indic %>%
      # Select the balance sheet financial indicator for the desired date
      dplyr::filter(Date == end_estim) %>%
      # Select only the eligible assets
      select(all_of(index_comp)) %>%
      t() %>%
      as.data.frame() %>%
      # Drop assets that don't have data available for that date
      drop_na()
  }

  colnames(indic) <- "statistic"

  if (order == "descending") {
    indic <- indic[order(-indic$statistic), , drop = FALSE]
  } else if (order == "ascending") {
    indic <- indic[order(indic$statistic), , drop = FALSE]
  }

  return(indic)
}

#################################################################
##                     get_assets_buy_sell                     ##
#################################################################

## After ordering the financial indicator, we need to decide which assets
## will be bought or sold. In the literature, we usually buy the X% assets
## that have the higher (positive) exposure to the factor and sell those that
## have the lower (negative) exposure. For the size factor in the IBX Index
## (100 members), for example, we would buy the 30 assets (X = 30%) with the
## lowest market cap and sell the 30 assets with the biggest market cap.

## ord_fin_indic: ordered data frame containing the assets' names and
### the value of the financial indicator. (ordered financial indicator)
### It's an output from the 'get_financial_indicators' function.
## buy_sell_action: we are interested in the long (buy) or short (sell) portfolio?
## percent: What should be the value of X in 'X% assets...'

assets_buy_sell <- function(ord_fin_indic, buy_sell_action, percent) {
  # Get the number of assets we should buy/sell
  n_assets <- ceiling(nrow(ord_fin_indic) * percent)

  # If we are interested in the short portfolio,
  # we need to change the data frame order upside down
  if (buy_sell_action == "sell") {
    ord_fin_indic <- data.frame(statistic = rev(ord_fin_indic$statistic), row.names = rev(rownames(ord_fin_indic)))
  }

  # Get only the X% assets with the higher/lower exposure to the factor
  assets_names <- rownames(ord_fin_indic[1:n_assets, , drop = FALSE])

  return(assets_names)
}

##################################################################
##                 calculate_port_returns                       ##
##################################################################

## After selecting which assets we will be buying or selling for the holding
## period, we need to select the returns for these assets in the holding
## period and then calculate the portfolio returns. Here we equal weight the
## assets returns.

## assets_returns: data table containing daily returns for every asset
## assets_names: vector containing the assets in the long or short portfolio
## start_hold_period: day that we will be buying the assets
## end_hold_period: day that we will be selling the assets

calculate_port_returns <- function(assets_returns, assets_names, start_hold_period, end_hold_period) {
  assets_port_returns <- assets_returns %>%
    select(Date, all_of(assets_names)) %>%
    dplyr::filter(Date > start_hold_period & Date <= end_hold_period) %>%
    # Drop if an asset has only NAs in the holding period
    dplyr::select_if(~ !any(is.na(.)))

  assets_port_returns <- xts(assets_port_returns[, -1], assets_port_returns$Date)

  # Equal weighted (same weight for all the assets)
  weights <- rep(1 / ncol(assets_port_returns), ncol(assets_port_returns))

  # Use PerformanceAnalytics function to generate the portfolio returns
  port_ret <- Return.portfolio(assets_port_returns, weights = weights)

  return(port_ret)
}

##################################################################
##                  calculate_price_indic                       ##
##################################################################

## If we have a price factor (momentum, low volatility, e.g.), we need to
## calculate some statistics based on the returns of the assets. If the
## factor is momentum, for example, we need to calculate the accumulated return.

## assets_ret_estim: return for every eligible asset in the estimation period
## factor_name: what's the factor name (value, size, momentum, e.g.)?
## factor_returns: if we are working with low beta, for example, we also need
### the factor returns (market factor. e.g.)
## risk_free_return: if we are working with low beta, for example, we also need
### the risk free returns

calculate_price_indic <- function(assets_ret_estim, factor_name, factor_returns = NULL, risk_free_return = NULL) {
  if (factor_name == "Momentum") {
    # Calculate the accumulated return for each and every asset eligible
    indic <- as.data.frame(apply(assets_ret_estim[, -1], 2, function(x) prod(1 + x) - 1))
  } else if (factor_name == "Momentum-1") {
    # In 'Momentum-1' we don't consider the last availiable month (12-1, e.g.)
    xts_aux <- xts(assets_ret_estim[, -1], assets_ret_estim$Date)
    index_last_month <- endpoints(xts_aux)[length(endpoints(xts_aux)) - 1]

    assets_ret_estim <- assets_ret_estim[1:index_last_month, , drop = FALSE]
    indic <- as.data.frame(apply(assets_ret_estim[, -1], 2, function(x) prod(1 + x) - 1))
  } else if (factor_name == "Low Volatility") {
    # Standard deviation for each and every asset eligible
    indic <- as.data.frame(apply(assets_ret_estim[, -1], 2, sd))
  } else if (factor_name == "Low Beta") {
    # Low beta also needs factor returns and the risk free return
    factor_returns <- merge(assets_ret_estim[, 1, drop = FALSE], factor_returns)
    colnames(factor_returns) <- c("Date", "Index")
    risk_free_return <- merge(assets_ret_estim[, 1, drop = FALSE], risk_free_return)
    colnames(risk_free_return) <- c("Date", "Risk_free")

    formula <- "I(x - risk_free_return$Risk_free) ~ I(factor_returns$Index - risk_free_return$Risk_free)"

    # Run the regression for every asset and extract only the beta_1 coefficient
    indic <- as.data.frame(apply(assets_ret_estim[, -1], 2, function(x) as.numeric(coef(lm(as.formula(formula)))[2])))
  }
  return(indic)
}

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                          WRAPPER FUNCTIONS                          ###
###                                                                     ###
###########################################################################
###########################################################################

#################################################################
##                        get_ord_indic                        ##
#################################################################

## i: iteration variable. Iterate through the rows from 'get_backtest_dates'
## start_date: YYYYMMDD when our backtest starts
## end_date: YYYYMMDD when our backtest ends
## hold_period: rebalance frequency
## estim_period: how long should we look back to calculate return statistics?
## fin_indic: a table containing the assets' financial indicators.
## comp_matrix: table containing the index compostion every month
## factor_name: what's the factor name (value, size, momentum, e.g.)?
## order: ascending or descending?
## factor_returns: index returns
## risk_free_return: risk free returns

get_ord_indic <- function(i, start_date, end_date, hold_period, estim_period, fin_indic, comp_matrix, factor_name, order, factor_returns = NULL, risk_free_return = NULL) {
  dates <- get_backtest_dates(start_date, end_date, hold_period, estim_period)

  comp_date <- dates$comp_date[i]
  start_estim <- dates$start_estim[i]
  end_estim <- dates$end_estim[i]

  comp_month <- get_index_composition(comp_matrix, fin_indic, comp_date)

  ord_indic <- get_financial_indicators(fin_indic, comp_month, start_estim, end_estim, factor_name, order, factor_returns, risk_free_return)

  return(ord_indic)
}

##################################################################
##                         run_backtest                         ##
##################################################################

## 'run_backtest', together with 'get_ord_indic', are the functions
## responsible for effectively running the backtests. After we get
## the ordered financial indicators, we need to form the portfolios.
## That's what this function accomplishes.

## assets_returns: data table containing daily returns for every asset
## list_indic: A list containing an ordered data frame of financial
### indicators for every rebalance date.
### It's an output from the 'get_ord_indic' function
## start_date: YYYYMMDD when our backtest starts
## end_date: YYYYMMDD when our backtest ends
## percent: What should be the value of X in 'X% assets...'

run_backtest <- function(assets_returns, list_indic, start_date, end_date, percent) {
  # Number of months between the start and end date
  n_months <- interval(ymd(start_date), ymd(end_date)) %/% months(1)
  # Holding Period: rebalance frequency
  hp <- n_months %/% (length(list_indic) - 1)
  # Data frame containing all the dates needed for the backtest
  ## estimation period won't interfere in this analysis
  ## 6 is a random number
  dates <- get_backtest_dates(start_date, end_date, hp, 6)

  ret_buy <- vector("list", length(list_indic)) # Return Long
  ret_ls <- vector("list", length(list_indic)) # Return Long e Short
  # We iterate through the dfs inside the list_indic and the rows in the dates df
  for (i in seq_along(list_indic)) {
    # Start and end dates for the ith rebalancing
    start_hold_period <- dates$start_hold_period[i]
    end_hold_period <- dates$end_hold_period[i]

    # What assets should we buy?
    assets_buy <- assets_buy_sell(list_indic[[i]], "buy", percent)
    # Create the portfolio with this assets and hold it during the holding period
    ret_buy[[i]] <- calculate_port_returns(assets_returns, assets_buy, start_hold_period, end_hold_period)

    # What assets should we sell?
    assets_sell <- assets_buy_sell(list_indic[[i]], "sell", percent)
    # Create the portfolio with this assets and hold it during the holding period
    ret_vend <- calculate_port_returns(assets_returns, assets_sell, start_hold_period, end_hold_period)

    # Long e Short Return = Long Return - Short Return
    long_short_xts <- cbind.xts(ret_buy[[i]], ret_vend)
    ret_ls[[i]] <- Return.portfolio(long_short_xts, weights = c(1, -1))
  }

  # Every element of this list is the return of the generated portfolio for a rebalance period.
  ## We bind all these rebalance periods.
  ret_buy <- do.call("rbind.xts", ret_buy)
  ret_ls <- do.call("rbind.xts", ret_ls)

  # Return both portfolios returns
  return(cbind.xts(ret_buy, ret_ls))
}

#################################################################
##                     run_backtest_decile                     ##
#################################################################

## We already have the 'run_backtest' function, but we may need to do a different
## kind of backtest. This function breaks our ordered data frame into N data frames
## with the same number of assets and returns the return of all the N
## portfolios built based on these assets.

## assets_returns: data table containing daily returns for every asset
## list_indic: A list containing an ordered data frame of financial
### indicators for every rebalance date.
### It's an output from the 'get_ord_indic' function
## start_date: YYYYMMDD when our backtest starts
## end_date: YYYYMMDD when our backtest ends
## n_ports: Number of times we wish to break our data frame.
### If we pick N, we will have N data frames with the same number of assets

run_backtest_decile <- function(assets_returns, list_indic, start_date, end_date, n_ports) {

  # Number of months between the start and end date
  n_months <- interval(ymd(start_date), ymd(end_date)) %/% months(1)
  # Holding Period: rebalance frequency
  hp <- n_months %/% (length(list_indic) - 1)
  # Data frame containing all the dates needed for the backtest
  ## estimation period won't interfere in this analysis
  ## 6 is a random number
  dates <- get_backtest_dates(start_date, end_date, hp, 6)

  ret_decile <- vector("list", length = n_ports)
  for (i in seq_along(list_indic)) {
    # Pick a single df from the list
    df <- list_indic[[i]]

    # We find the break points to divide the data frame in 'n_ports' (10, e.g.) equal parts
    x <- 1:nrow(df)
    split_seq <- split(x, cut(x, n_ports, labels = FALSE))

    # Start and end dates for the ith rebalancing
    start_hold_period <- dates$start_hold_period[i]
    end_hold_period <- dates$end_hold_period[i]

    # Create 'n_ports' different portfolios
    for (j in 1:n_ports) {
      # What assets should we buy?
      assets_decile <- rownames(df[split_seq[[j]], , drop = FALSE])
      ret_decile[[j]][[i]] <- calculate_port_returns(assets_returns, assets_decile, start_hold_period, end_hold_period)
    }
  }

  # Every element of this list of list is the return of one generated portfolio for a rebalance period.
  ## We bind all these rebalance periods for each generated portfolio
  ret_decile <- lapply(ret_decile, function(x) do.call("rbind.xts", x))
  # We bind every portfolio in a single xts object
  ret_decile <- do.call("cbind.xts", ret_decile)


  return(ret_decile)
}

##################################################################
##                   run_backtest_three_ports                   ##
##################################################################

## In what way this is different from the other two? This one takes a
## percentage, like 0.3 (30%), and returns three portfolios based on this
## percentage and the ordered data frame with financial indicators: 0 to 30%;
## 31% to 70%; and 71% to 100%. In this case, 'high' are the assets with higher
## positive exposure to a factor, 'mid' (or core) has lower or zero exposure,
## and 'low' has a higher negative exposure.

## assets_returns: data table containing daily returns for every asset
## list_indic: A list containing an ordered data frame of financial
### indicators for every rebalance date.
### It's an output from the 'get_ord_indic' function
## start_date: YYYYMMDD when our backtest starts
## end_date: YYYYMMDD when our backtest ends
## percent: What should be the value of X in 'X% assets...'

run_backtest_three_ports <- function(assets_returns, list_indic, start_date, end_date, percent) {
  high_fact <- list() # fact = factor
  mid_fact <- list()
  low_fact <- list()
  for (i in seq_along(list_indic)) {
    # Pick a single df from the list
    df <- list_indic[[i]]

    # Find the break points for each part of the ordered data frame
    high <- c(1, ceiling(nrow(df) * percent))
    low <- c(nrow(df) - high[2] + 1, nrow(df))

    # Use the break points to filter the data frame
    high_fact[[i]] <- df[seq(high[1], high[2]), , drop = FALSE]
    low_fact[[i]] <- df[seq(low[1], low[2]), , drop = FALSE]
    
    # If the percent is bigger than 0.5 there is no way of constructing the core portfolio
    if(percent < 0.5){
      mid <- c(high[2] + 1, low[1] - 1)
      mid_fact[[i]] <- df[seq(mid[1], mid[2]), , drop = FALSE]
    }
  }

  # Run the backtests based on this filtered that frame
  high_fact <- run_backtest(assets_returns, high_fact, start_date, end_date, 1)[, 1]
  low_fact <- run_backtest(assets_returns, low_fact, start_date, end_date, 1)[, 1]

  # Bind the three portfolios
  fact <- cbind.xts(high_fact, low_fact)
  
  if(percent < 0.5){
    mid_fact <- run_backtest(assets_returns, mid_fact, start_date, end_date, 1)[, 1]
    fact <- cbind.xts(fact, mid_fact)
    # Re-arrange xts columns to have high fact, mid fact and low fact 
    fact <- fact[, c(1,3,2)]
  }

  return(fact)
}

##################################################################
##                   run_backtest_double_sort                   ##
##################################################################

run_backtest_double_sort <- function(assets_returns, list_indic, list_vol, start_date, end_date, percent) {
  high_fact_vol <- vector("list", length = length(list_indic))
  mid_fact_vol <- vector("list", length = length(list_indic))
  low_fact_vol <- vector("list", length = length(list_indic))
  for (i in seq_along(list_indic)) {
    # Pick a single df from the list
    ord_indic <- list_indic[[i]]

    # Find the break points for each part of the ordered data frame
    high <- c(1, ceiling(nrow(ord_indic) * percent))
    low <- c(nrow(ord_indic) - high[2] + 1, nrow(ord_indic))
    mid <- c(high[2] + 1, low[1] - 1)

    # Use the break points to filter the data frame
    high_fact <- ord_indic[seq(high[1], high[2]), , drop = FALSE]
    mid_fact <- ord_indic[seq(mid[1], mid[2]), , drop = FALSE]
    low_fact <- ord_indic[seq(low[1], low[2]), , drop = FALSE]

    # Select the corresponding data frame ordered by volatility
    ord_vol <- list_vol[[i]]

    # Filter the df ordered by volatility based on the filtered df based on the other factor (value, e.g.)
    high_fact_vol[[i]] <- ord_vol %>% dplyr::filter(row.names(ord_vol) %in% rownames(high_fact)) %>% arrange(statistic)
    low_fact_vol[[i]] <- ord_vol %>% dplyr::filter(row.names(ord_vol) %in% rownames(low_fact)) %>% arrange(statistic)
    
    # If the percent is bigger than 0.5 there is no way of constructing the core portfolio
    if(percent < 0.5){
      mid <- c(high[2] + 1, low[1] - 1)
      mid_fact <- ord_indic[seq(mid[1], mid[2]), , drop = FALSE]
      mid_fact_vol[[i]] <- ord_vol %>% dplyr::filter(row.names(ord_vol) %in% rownames(mid_fact)) %>% arrange(statistic)
    }
  }

  # Run the backtests based on the assets double sorted
  ## We multiply percent * 10 to find the number of deciles
  high_fact_vol <- run_backtest_decile(assets_returns, high_fact_vol, start_date, end_date, percent*10)
  low_fact_vol <- run_backtest_decile(assets_returns, low_fact_vol, start_date, end_date, percent*10)

  double_sort <- high_fact_vol

  if(percent < 0.5){
    n_ports <- (1 - percent * 2) * 10
    mid_fact_vol <- run_backtest_decile(assets_returns, mid_fact_vol, start_date, end_date, n_ports)
    double_sort <- cbind.xts(double_sort, mid_fact_vol)
  }
  
  double_sort <- cbind.xts(double_sort, low_fact_vol)
  
  return(double_sort)
}


