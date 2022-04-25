############################################################################
############################################################################
###                                                                      ###
###                              SECTION 0:                              ###
###                             INTRODUCTION                             ###
###                                                                      ###
############################################################################
############################################################################

# code author: Pedro Teles (pteles@avantgardeam.com.br)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                           TABLE FUNCTIONS                           ###
###                                                                     ###
###########################################################################
###########################################################################

calculate_long_short <- function(ret){
  # Calculate the Long e Short portfolio returns
  data_xts <- xts(ret[,-1], ret$Date)
  long_short <- Return.portfolio(data_xts, weights = c(1, -1), rebalance_on = 'months')
  
  return(long_short$portfolio.returns)
}

## https://papers.ssrn.com/sol3/papers.cfm?abstract_id=412588
### shrp1: Sharpe ratio of the first portfolio
### shrp2: Sharpe ratio of the second portfolio
### corel: Correlation between first and second portfolio
### n_obs: Number of observations
jobson_korkie_memmel <- function(shrp1, shrp2, corel, n_obs){
  z <- (shrp1 - shrp2) / sqrt((1 / n_obs) * (2*(1 - corel) + 0.5 * (shrp1^2 + shrp2^2 - shrp1 * shrp2 * (1 + corel^2)))) 
  return(z)
}

## ret1: Return of the first portfilio
## ret2: Return of the second portfolio
## ind: Market portfolio return
## rf: Risk free return
estat_jkm <- function(ret1, ret2, ind, rf){
  SR1 <- mean((ret1 - rf)) / sd(ret1)
  SR2 <- mean((ret2 - rf)) / sd(ret2)
  
  t_SR <- jobson_korkie_memmel(SR1, SR2, cor(ret1, ret2), length(ret1))
  
  return(round(t_SR, 2))
}

## Calculate the return statistics of a portfolio
stat_ret <- function(ret_port, ind, rf) {
  
  # Cumulative Return
  ret_period <- function(ret) {
    prod(ret + 1)^(252 / length(ret)) - 1
  }
  
  # CAPM
  regres <- lm(I(ret_port - rf) ~ I(ind - rf))
  # Get regression coefficients
  regres_coef <- summary(regres)$coefficients
  
  alpha <- (regres_coef[1, 1] + 1) ^ 252 - 1
  t_alpha <- regres_coef[1, 3]
  p_value_alpha <- regres_coef[1, 4]
  beta <- regres_coef[2, 1]
  
  cumulative_ret <- ret_period(ret_port)
  # Portfolio volatility
  vol <- sd(ret_port) * sqrt(252)
  # Index volatility
  vol_ind <- sd(ind) * sqrt(252)
  # Sharpe Ratio
  SR <- mean((ret_port - rf)) / sd(ret_port)
  SR_ind <- mean((ind - rf)) / sd(ind)
  
  # Difference in the portfolio SR and the index SR
  t_SR <- jobson_korkie_memmel(SR, SR_ind, cor(ret_port, ind), length(ret_port))
  
  port_excess_ret <- ret_period(ret_port - rf)
  
  
  results <- data.frame(c(
    cumulative_ret, vol, SR, t_SR,
    port_excess_ret, beta, alpha, t_alpha
  )) 
  
  rownames(results) <- c(
    "Annualized Return (%)",
    "Standard Deviation (%)", "Sharpe Ratio", "z(SR)",
    "Annualized Rp-Rf (%)",
    "Beta", "Alpha (%)", "t(alpha)"
  )
  
  results[c(1, 2, 5, 7), 1] <- round(results[c(1, 2, 5, 7), 1]*100, 2)
  results[c(3, 4, 6, 8), 1] <- round(results[c(3, 4, 6, 8), 1], 2)
  
  return(results)
}

## Uses the stat_ret function and displays the results in a presentable form
### df_ret: Data frame containing the portfolios returns in this order: D1:Dn, Long e Short
panel_function <- function(df_ret, ind, rf){
  # Add the index returns to the data frame
  df_panel <- data.frame(df_ret, IBX = ind)
  
  # Calculate the return statistics for every column
  panel <-  lapply(df_panel, function(x) stat_ret(x, ind, rf))
  panel <- do.call("cbind", panel) 
  n_ports <- ncol(panel)-2
  colnames(panel) <- c(paste0("D", 1:n_ports), paste0('D1-D', n_ports), "IBX")
  
  # Some statistics don't apply to all portfolios (beta for the market index returns, e.g.)
  panel[c(3, 4), (ncol(panel)-1)] <- NA
  panel[c(4, 6, 7, 8), ncol(panel)] <- NA
  
  # The regression of the Long and Short portfolio is different because it does not have a risk free component
  regres_ls <- summary(lm(df_ret$LongShort ~ I(ind - rf)))$coefficients
  ## Portfolio annualized alpha
  alpha_regres <- round((prod(1 + regres_ls[1, 1]) ^ 12 - 1) * 100, 2)
  ## t-value alpha
  t_alpha <- round(regres_ls[1, 3], 2)
  ## Portfolio beta
  beta_regres <- round(regres_ls[2, 1], 2)
  
  # Correct the panel data frame
  panel[c(6, 7, 8), (ncol(panel)-1)] <- c(beta_regres, alpha_regres, t_alpha)
  
  return(panel)
  
}

## Calculate the average of positive and negative returns and the biggest loss
avg_max_loss <- function(ret){
  # Average positive returns
  avg_positive <- mean(ret[ret > 0]) * 100
  # Average negative returns
  avg_negative <- mean(ret[ret < 0]) * 100
  # Biggest single day loss
  max_loss <- min(ret) * 100
  
  avg_max <- data.frame(c(
    avg_positive, avg_negative,
    max_loss
  ))
  
  rownames(avg_max) <-c("Average positive returns (%)", "Average negative returns (%)", "Biggest loss - day (%)")
  
  avg_max <- round(avg_max, 2)
  
  return(avg_max)
}

## Regression of the portfolio returns against asset pricing models
factor_analysis <- function(ret, type, col_name){
  # CAPM
  capm <- summary(lm(I(ret - factors$Risk_free) ~ factors$Rm_minus_Rf))$coefficients
  
  # Fama e French 3 factors
  factors3 <- summary(lm(I(ret - factors$Risk_free) ~ factors$Rm_minus_Rf + factors$SMB + factors$HML))$coefficients
  
  # Cahart
  factors4 <- summary(lm(I(ret - factors$Risk_free) ~ factors$Rm_minus_Rf + factors$SMB + factors$HML + factors$WML))$coefficients
  
  # Cahart + Quality
  factors5 <- summary(lm(I(ret - factors$Risk_free) ~ factors$Rm_minus_Rf + factors$SMB + factors$HML + factors$WML + factors$QMJ))$coefficients
  
  # Type can be the intercept (alpha) or the t-statistics
  if (type == 'Intercept'){
    column_index <- 1 # intercept is in the first column
  } else if (type == 't'){
    column_index <- 3 # t-value is in the third column
  }
  
  result <- data.frame(c(
    capm[1, column_index],
    factors3[1, column_index],
    factors4[1, column_index],
    factors5[1, column_index]
  )) %>% set_names(col_name)
  
  if (type == 'Intercept'){
    result <- round(((result + 1) ^ 252 - 1)*100, 2) # Anlualized alpha
  } else if (type == 't'){
    result <- round(result, 2)
  }
  
  rownames(result) <- c("CAPM", "F&F", "Cahart", "5 Factors") 
  
  return(result)
  
}


###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 2:                             ###
###                             IMPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################


# Import the base portfolio: Low Vol factor 3 portfolios
data <- read_csv("Portfolios\\simple_sort_vol_3.csv", col_types = "Dnnn") %>% 
  set_names(c("Date", "LowVol", "MidVol", "HighVol"))

# Calculate the Long e Short portfolio returns
data_xts <- xts(data[,c(-1, -3)], data$Date)
long_short <- Return.portfolio(data_xts, weights = c(1, -1), rebalance_on = 'months')
# Add it to the original data frame
data$LongShort <- long_short$portfolio.returns

rm(data_xts, long_short)

# Import the index returns
index <- read_csv("Brazil\\index_returns.csv", col_types = "Dn") %>%
  dplyr::filter(Date >= "2003-01-01" & Date <= "2021-12-31") %>% 
  set_names(c("Date", "IBX"))

# Import the risk free returns
rf <- read_csv("Brazil\\risk_free_returns.csv", col_types = "Dn") %>%
  dplyr::filter(Date >= "2003-01-01" & Date <= "2021-12-31") %>% 
  set_names(c("Date", "Risk_free"))

# Import the proprietary factor returns and add the market factor, the risk free and the index returns
factors <- read_csv("Portfolios\\proprietary_factors.csv") %>%
  dplyr::filter(Date >= "2003-01-01" & Date <= "2021-12-31") %>%
  mutate(Risk_free = rf$Risk_free, IBX = index$IBX, Rm_minus_Rf = index$IBX - rf$Risk_free)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 3:                             ###
###                     GENERATE FIGURES AND TABLES                     ###
###                                                                     ###
###########################################################################
###########################################################################

##################################################################
##                           Figure 1                           ##
##################################################################

# Plot portfolios ordered by volatility

## Exclude Risk Free return and add IBX return
data_fig <- data.frame(data[,c(-1, -ncol(data))], IBX = index$IBX)

## Download the returns of the 10 portfolios sorted on volatility
data_fig <- read_csv("Portfolios\\simple_sort_vol_10.csv", col_types = "Dnnnnnnnnnn") %>%
  mutate(IBX = index$IBX) %>% dplyr::select(-1)

## Add each portfolio's beta to a vector
beta_vector <- vector(length = ncol(data_fig))
for (i in seq_along(beta_vector)) {
  beta_vector[i] <- coef(lm(I(data_fig[,i, drop = TRUE] - factors$Risk_free) ~ I(index$IBX - factors$Risk_free)))[2]
}

port_betas_ret <- data.frame(Name = c(paste0("P", 1:(length(beta_vector)-1)), "IBX"),
                             Beta = beta_vector,
                             # Calculate, for each portfolio, the annualized return
                             Cumulative_return = apply(data_fig, 2, function(x) prod(1 + x) ^ (252 / length(x)) - 1))

# Cumulative return against beta regression
reg_mod_coef <- coef(lm(Cumulative_return ~ Beta, data = port_betas_ret))

# Index annualized return
ret_rf <- prod(1 + factors$Risk_free) ^ (252/length(factors$Risk_free)) - 1
# Index annualized excess return
ret_index_rf <- prod(1 + (index$IBX)) ^ (252/length(index$IBX)) - 1 - ret_rf

# Security Market Line https://www.investopedia.com/terms/s/sml.asp
sml <- ret_rf + seq(0.5,1.4, by = 0.1) * ret_index_rf

# create a png plot
png("fig1.png", height=1600, width=2400, res=250, pointsize=8)

# Generate the graph
ggplot() + geom_point(data = port_betas_ret, aes(x = Beta, y = Cumulative_return)) + 
  geom_abline(slope = reg_mod_coef[2], intercept = reg_mod_coef[1], colour = "yellow3", linetype = "dashed") +
  geom_line(aes(x = seq(0.5,1.4, by = 0.1), y = sml), colour = "dark blue") + theme_classic() + 
  geom_text(data = port_betas_ret, aes(label = Name, x = Beta, y = Cumulative_return), hjust = 0.5,  vjust = -1) +
  xlab("Beta") + ylab("Annualized Return") + 
  scale_y_continuous(expand = c(0.01, 0.02), labels = scales::percent_format(accuracy = 1)) 

dev.off()

rm(data_fig, i, reg_mod_coef, sml, beta_vector, port_betas_ret, ret_rf, ret_index_rf)

#################################################################
##                           Table 1                           ##
#################################################################

# Low volatility portfolios results

## Panel A
df_vol_10 <- read_csv("Portfolios\\simple_sort_vol_10.csv", col_types = "Dnnnnnnnnnn")
df_vol_10$LongShort <- calculate_long_short(df_vol_10[,c(1, 2, 11)])

tb1_panel_a <- panel_function(df_vol_10[,-1], index$IBX, factors$Risk_free)

## Panel B
tb1_panel_b <- lapply(df_vol_10[,-1], function(x) avg_max_loss(x))
tb1_panel_b <- do.call("cbind", tb1_panel_b)
tb1_panel_b <- data.frame(tb1_panel_b, avg_max_loss(index$IBX)) %>% set_names(c(paste0("D", 1:10), "D1-D10", "IBX"))

## Panel C
tb1_panel_c <- panel_function(data[,-1], index$IBX, factors$Risk_free)

## Panel D
tb1_panel_d <- lapply(data[,c(-1, -5), drop = FALSE], function(x) avg_max_loss(x))
tb1_panel_d <- do.call("cbind", tb1_panel_d)

tb1_panel_d <- data.frame(tb1_panel_d, avg_max_loss(index$IBX)) %>% set_names(c(paste0("D", 1:3), "IBX"))

rm(df_vol_10)

#################################################################
##                           Table 2                           ##
#################################################################

# Period Divisions

data_03_08 <- data %>% dplyr::filter(Date >= "2003-01-01" & Date <= "2008-12-31")
index_03_08 <- index %>% dplyr::filter(Date >= "2003-01-01" & Date <= "2008-12-31")
factors_03_08 <- factors %>% dplyr::filter(Date >= "2003-01-01" & Date <= "2008-12-31")

data_09 <- data %>% dplyr::filter(Date >= "2009-01-01" & Date <= "2009-12-31")
index_09 <- index %>% dplyr::filter(Date >= "2009-01-01" & Date <= "2009-12-31")
factors_09 <- factors %>% dplyr::filter(Date >= "2009-01-01" & Date <= "2009-12-31")

data_10_21 <- data %>% dplyr::filter(Date >= "2010-01-01" & Date <= "2021-12-31")
index_10_21 <- index %>% dplyr::filter(Date >= "2010-01-01" & Date <= "2021-12-31")
factors_10_21 <- factors %>% dplyr::filter(Date >= "2010-01-01" & Date <= "2021-12-31")

## Panel A
tb2_panel_a <- panel_function(data_03_08[,-1], index_03_08$IBX, factors_03_08$Risk_free)

## Panel B
tb2_panel_b <- panel_function(data_09[,-1], index_09$IBX, factors_09$Risk_free)

## Panel C
tb2_panel_c <- panel_function(data_10_21[,-1], index_10_21$IBX, factors_10_21$Risk_free)

rm(data_03_08, data_09, data_10_21,
   index_03_08, index_09, index_10_21,
   factors_03_08, factors_09, factors_10_21)

#################################################################
##                           Table 3                           ##
#################################################################

# Factor regressions

## Panel A
tb3_panel_a <- data.frame(factor_analysis(data$LowVol, "Intercept", "Low Vol."),
                           factor_analysis(data$MidVol, "Intercept", "Mid Vol."),
                           factor_analysis(data$HighVol, "Intercept", "High Vol."))

## Panel B
tb3_panel_b <- data.frame(factor_analysis(data$LowVol, "t", "Low Vol."),
                           factor_analysis(data$MidVol, "t", "Mid Vol."),
                           factor_analysis(data$HighVol, "t", "High Vol."))

#################################################################
##                           Table 4                           ##
#################################################################

# Other factors portfolios results

## Panel A
df_size <- read_csv("Portfolios\\simple_sort_size_10.csv", col_types = "Dnnnnnnnnnn")
df_size$LongShort <- calculate_long_short(df_size[,c(1, 2, 11)])

tb4_panel_a <- panel_function(df_size[,-1], index$IBX, factors$Risk_free)

## Panel B
df_value <- read_csv("Portfolios\\simple_sort_value_10.csv", col_types = "Dnnnnnnnnnn")
df_value$LongShort <- calculate_long_short(df_value[,c(1, 2, 11)])

tb4_panel_b <- panel_function(df_value[,-1], index$IBX, factors$Risk_free)

## Panel C
df_mom <- read_csv("Portfolios\\simple_sort_mom_10.csv", col_types = "Dnnnnnnnnnn")
df_mom$LongShort <- calculate_long_short(df_mom[,c(1, 2, 11)])

tb4_panel_c <- panel_function(df_mom[,-1], index$IBX, factors$Risk_free)

## Panel D
df_quality <- read_csv("Portfolios\\simple_sort_quality_10.csv", col_types = "Dnnnnnnnnnn")
df_quality$LongShort <- calculate_long_short(df_quality[,c(1, 2, 11)])

tb4_panel_d <- panel_function(df_quality[,-1], index$IBX, factors$Risk_free)

rm(df_size, df_value, df_mom, df_quality)

#################################################################
##                           Table 5                           ##
#################################################################

# Double sorted portfolios results

## Panel A
df_size_vol <- read_csv("Portfolios\\double_sort_size.csv", col_types = "Dnnnnnnnnnn")
df_size_vol$LongShort <- calculate_long_short(df_size_vol[,c(1, 2, 11)])

tb5_panel_a <- panel_function(df_size_vol[,-1], index$IBX, factors$Risk_free)[ ,-11]

## Panel B
df_value_vol <- read_csv("Portfolios\\double_sort_value.csv", col_types = "Dnnnnnnnnnn")
df_value_vol$LongShort <- calculate_long_short(df_value_vol[,c(1, 2, 11)])

tb5_panel_b <- panel_function(df_value_vol[,-1], index$IBX, factors$Risk_free)[ ,-11]

## Panel C
df_mom_vol <- read_csv("Portfolios\\double_sort_mom.csv", col_types = "Dnnnnnnnnnn")
df_mom_vol$LongShort <- calculate_long_short(df_mom_vol[,c(1, 2, 11)])

tb5_panel_c <- panel_function(df_mom_vol[,-1], index$IBX, factors$Risk_free)[ ,-11]

## Panel D
df_quality_vol <- read_csv("Portfolios\\double_sort_quality.csv", col_types = "Dnnnnnnnnnn")
df_quality_vol$LongShort <- calculate_long_short(df_quality_vol[,c(1, 2, 11)])

tb5_panel_d <- panel_function(df_quality_vol[,-1], index$IBX, factors$Risk_free)[ ,-11]

rm(df_size_vol, df_value_vol, df_mom_vol, df_quality_vol)

#################################################################
##                           Table 6                           ##
#################################################################

# t-value comparison between Sharpe Ratios

## Panel A
s_size10 <- read_csv("Portfolios\\simple_sort_size_10.csv", col_types = "Dnnnnnnnnnn")
d_size10 <- read_csv("Portfolios\\double_sort_size.csv", col_types = "Dnnnnnnnnnn")

tb6_panel_a <- as.data.frame(matrix(nrow = (ncol(s_size10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_panel_a)) {
  tb6_panel_a[i, 1] <- estat_jkm(d_size10[,i+1, drop = TRUE], s_size10[,i+1, drop = TRUE], index$IBX, factors$Risk_free)
}

rm(s_size10, d_size10)

## Panel B
s_value10 <- read_csv("Portfolios\\simple_sort_value_10.csv", col_types = "Dnnnnnnnnnn")
d_value10 <- read_csv("Portfolios\\double_sort_value.csv", col_types = "Dnnnnnnnnnn")

tb6_panel_b <- as.data.frame(matrix(nrow = (ncol(s_value10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_panel_b)) {
  tb6_panel_b[i, 1] <- estat_jkm(d_value10[,i+1, drop = TRUE], s_value10[,i+1, drop = TRUE], index$IBX, factors$Risk_free)
}

rm(s_value10, d_value10)

## Panel C
s_mom10 <- read_csv("Portfolios\\simple_sort_mom_10.csv", col_types = "Dnnnnnnnnnn")
d_mom10 <- read_csv("Portfolios\\double_sort_mom.csv", col_types = "Dnnnnnnnnnn")

tb6_panel_c <- as.data.frame(matrix(nrow = (ncol(s_mom10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_panel_c)) {
  tb6_panel_c[i, 1] <- estat_jkm(d_mom10[,i+1, drop = TRUE], s_mom10[,i+1, drop = TRUE], index$IBX, factors$Risk_free)
}

rm(s_mom10, d_mom10)

## Panel D
s_quality10 <- read_csv("Portfolios\\simple_sort_quality_10.csv", col_types = "Dnnnnnnnnnn")
d_quality10 <- read_csv("Portfolios\\double_sort_quality.csv", col_types = "Dnnnnnnnnnn")

tb6_panel_d <- as.data.frame(matrix(nrow = (ncol(s_quality10)-1), ncol = 1), row.names = paste0("D", 1:10)) %>% set_names("Dn")
for (i in 1:nrow(tb6_panel_d)) {
  tb6_panel_d[i, 1] <- estat_jkm(d_quality10[,i+1, drop = TRUE], s_quality10[,i+1, drop = TRUE], index$IBX, factors$Risk_free)
}

rm(s_quality10, d_quality10, i)

##################################################################
##                           Figure 2                           ##
##################################################################

# Trading Strategy Figure

trad_strat <- read_csv("Portfolios\\trad_strat.csv", col_types = "Dnnn")

index_tb7 <- index %>% dplyr::filter(Date > '2003-12-31')
rf_tb7 <- rf %>% dplyr::filter(Date > '2003-12-31')

## Figure 2
trad_strat_acumul <- apply(trad_strat[,-1], 2, function(x) cumprod(1 + x) - 1)
trad_strat_acumul <- data.frame(Date = trad_strat$Date, trad_strat_acumul, Risk_free = rf_tb7$Risk_free)

# create a png plot
png("fig2.png", height=1600, width=2400, res=250, pointsize=8)

ggplot() + geom_line(data = trad_strat_acumul, aes(x = Date, y = Beta, colour = "Pure Beta")) + 
  geom_line(data = trad_strat_acumul, aes(x = Date, y = BetaBlume, colour = "Adjusted Beta")) +
  geom_line(data = trad_strat_acumul, aes(x = Date, y = OrigLS, colour = "Simple LS")) +
  geom_line(data = trad_strat_acumul, aes(x = Date, y = Risk_free, colour = "Risk Free")) + theme_classic() + 
  xlab("") + ylab('') + scale_color_manual(values = c("Pure Beta" = "darkblue", "Adjusted Beta" = "dark green", "Simple LS" = "dark red", "Risk Free" = "black")) +
  theme(legend.position="bottom") + theme(legend.title=element_blank())

dev.off()

#################################################################
##                           Table 7                           ##
#################################################################

stat_beta <- data.frame(stat_ret(trad_strat$Beta, index_tb7$IBX, rf_tb7$Risk_free),
                        stat_ret(trad_strat$BetaBlume, index_tb7$IBX, rf_tb7$Risk_free),
                        stat_ret(trad_strat$OrigLS, index_tb7$IBX, rf_tb7$Risk_free)) %>% set_names('Pure Beta', 'Beta Blume', 'Simple LS')

trad_strat_xts <- xts(trad_strat[,-1], trad_strat$Date)

max_drawd <- data.frame(round(maxDrawdown(trad_strat_xts$Beta)*100, 2), round(maxDrawdown(trad_strat_xts$BetaBlume)*100, 2), round(maxDrawdown(trad_strat_xts$OrigLS)*100, 2),row.names = 'Max. Drawdown') %>% set_names('Pure Beta', 'Beta Blume', 'Simple LS')
 
tb7 <- rbind(stat_beta, max_drawd)

rm(stat_beta, index_tb7, max_drawd, rf_tb7, trad_strat, trad_strat_acumul, trad_strat_xts)

rm(data, factors, index, rf)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###                             EXPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

# http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r

# create a new workbook for outputs
wb <- createWorkbook(type="xlsx")

# Define some cell styles
#++++++++++++++++++++
# Title and sub title styles
TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, 
                                   color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE <- CellStyle(wb) + 
  Font(wb,  heightInPoints=14, 
       isItalic=FALSE, isBold=TRUE)
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("BOTTOM"), 
         pen=c("BORDER_THIN")) 


#++++++++++++++++++++++++
# Helper function to add titles
#++++++++++++++++++++++++
# - sheet : sheet object to contain the title
# - rowIndex : numeric value indicating the row to 
#contain the title
# - title : the text to use as title
# - titleStyle : style object to use for title
xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

# Change column width
setColumnWidth(sheet, colIndex=c(1:ncol(tb1_panel_a)), colWidth=11)

add_excel_sheet <- function(sheetName, list_dfs, title_vector){
  # Create a new sheet in the workbook
  sheet <- createSheet(wb, sheetName = sheetName)
  
  cont_rows <- 0
  for (i in seq_along(list_dfs)) {
    cont_rows <- cont_rows + 1
    # Add sub title
    xlsx.addTitle(sheet, rowIndex=cont_rows, 
                  title=title_vector[i],
                  titleStyle = SUB_TITLE_STYLE)
    
    cont_rows <- cont_rows + 1
    
    # Center align every columns
    TABLE_COL_STYLE <- CellStyle(wb) + Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER")
    TABLE_COL_STYLE <- rep(list(TABLE_COL_STYLE), ncol(list_dfs[[i]]))
    names(TABLE_COL_STYLE) <- 1:ncol(list_dfs[[i]])
    
    # Add a table
    addDataFrame(list_dfs[[i]], sheet, startRow=cont_rows, startColumn=1, 
                 colnamesStyle = TABLE_COLNAMES_STYLE,
                 rownamesStyle = TABLE_ROWNAMES_STYLE,
                 colStyle = TABLE_COL_STYLE)
    
    cont_rows <- cont_rows + nrow(list_dfs[[i]]) + 1
    
  }
}

# Create a new sheet to contain the plot
sheet <-createSheet(wb, sheetName = "F1")
# Add the plot created previously
addPicture("fig1.png", sheet, scale = 1, startRow = 3,
           startColumn = 1)
# Remove the plot from the disk
res<-file.remove("fig1.png")

list_dfs <- list(tb1_panel_a, tb1_panel_b, tb1_panel_c, tb1_panel_d)
title_vector <- c('Panel A', 'Panel B', 'Panel C', 'Panel D')
add_excel_sheet('T1', list_dfs, title_vector)

list_dfs <- list(tb2_panel_a, tb2_panel_b, tb2_panel_c)
title_vector <- c('Panel A: 2003 - 2008', 'Panel B: 2009', 'Panel C: 2010 - 2021')
add_excel_sheet('T2', list_dfs, title_vector)

list_dfs <- list(tb3_panel_a, tb3_panel_b)
title_vector <- c('Panel A: Alpha', 'Panel B: t-value')
add_excel_sheet('T3', list_dfs, title_vector)

list_dfs <- list(tb4_panel_a, tb4_panel_b, tb4_panel_c, tb4_panel_d)
title_vector <- c('Panel A: Size', 'Panel B: Value', 'Panel C: Momentum', 'Panel D: Quality')
add_excel_sheet('T4', list_dfs, title_vector)

list_dfs <- list(tb5_panel_a, tb5_panel_b, tb5_panel_c, tb5_panel_d)
title_vector <- c('Panel A: Size', 'Panel B: Value', 'Panel C: Momentum', 'Panel D: Quality')
add_excel_sheet('T5', list_dfs, title_vector)

list_dfs <- list(tb6_panel_a, tb6_panel_b, tb6_panel_c, tb6_panel_d)
title_vector <- c('Panel A: Size', 'Panel B: Value', 'Panel C: Momentum', 'Panel D: Quality')
add_excel_sheet('T6', list_dfs, title_vector)

list_dfs <- list(tb7)
title_vector <- c('Panel A')
add_excel_sheet('T7', list_dfs, title_vector)

# Create a new sheet to contain the plot
sheet <-createSheet(wb, sheetName = "F2")
# Add the plot created previously
addPicture("fig2.png", sheet, scale = 1, startRow = 3,
           startColumn = 1)
# Remove the plot from the disk
res<-file.remove("fig2.png")

# Save the workbook to a file
saveWorkbook(wb, "graphs_tables.xlsx")
