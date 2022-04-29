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
###                 DEALING WITH INDEX COMPOSITION DATA                 ###
###                                                                     ###
###########################################################################
###########################################################################

## Import the index composition data extracted from Bloomberg
comp_raw <- read_xlsx("raw_data\\index_comp_raw.xlsx", sheet = 1)

## The odd columns have the Bloomberg Ticker; the even columns have the company name.
### We want only the Bloomberg Ticker
odd <- seq(1, ncol(comp_raw), 2)
## Rows 1, 2 and 3 have no useful information too
comp <- comp_raw[-1:-3, odd]

## The columns' names are dates but are in a numeric format. We fix that
colnames(comp) <- as.Date(as.numeric(colnames(comp)), origin = "1899-12-30")

## Create a column with all assets that is a member or once were a member of the IBX Index
assets <- data.frame(Assets = unique(unlist(comp))) %>% drop_na()

## Create an indicative matrix. If an asset is 1 in a given month, it means that that asset
### was in the index composition that month
dates <- colnames(comp)
for (i in 1:ncol(comp)) {
  new_df <- comp %>%
    dplyr::select(i) %>%
    drop_na() %>%
    mutate(1) %>%
    set_names("Assets", dates[i])

  assets <- merge(assets, new_df, all.x = TRUE)
}

## If an asset is NA in a given month, it means that that asset 
### wasn't in the index composition that month. So we replace NA for 0.
assets[is.na(assets)] <- 0

## Eliminate useless variables
rm(comp_raw, comp, new_df, dates, i, odd)

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 2:                             ###
###                       DEALING WITH PRICE DATA                       ###
###                                                                     ###
###########################################################################
###########################################################################

#################################################################
##                Import and clean asset prices                ##
#################################################################

## Import the data extracted from Bloomberg
price_raw <- read_xlsx("raw_data\\price_raw.xlsx", sheet = 2)

## The first row carries no information
price_raw <- price_raw[-1, ]
colnames(price_raw)[1] <- "Date"

## When we import the data from Excel, the date column comes as numeric.
### We transform it into a date object
price_raw$Date <- as.Date(as.numeric(price_raw$Date), origin = "1899-12-30")

## We order the data frame based on the date, starting with the oldest observation
price_raw <- price_raw[order(price_raw$Date), ]

## The price data is import as character. We transform it to numeric
price_raw <- data.frame(Data = price_raw$Date, sapply(price_raw[, -1], as.numeric)) %>% set_names(colnames(price_raw))

#################################################################
##                Import and clean index prices                ##
#################################################################

## Repeat the process used for the asset prices to the index prices
ind <- read_xlsx("raw_data\\price_raw.xlsx", sheet = 3)
ind <- ind[-1:-3, ]
colnames(ind)[1] <- "Date"
ind$Date <- as.Date(as.numeric(ind$Date), origin = "1899-12-30")
ind <- ind[order(ind$Date), ]
ind$`IBX Index` <- as.numeric(ind$`IBX Index`)
ind <- ind %>%
  drop_na() %>%
  dplyr::filter(Date > "2000-12-31" & Date <= "2021-12-31")

#################################################################
##              Import and clean risk free prices              ##
#################################################################

## The risk free data was extracted using the Brazilian Central Bank API

rf <- read_xlsx("raw_data\\cdi_raw.xlsx") %>%
  dplyr::select(Date, cdi) %>%
  set_names("Date", "Risk_free")

rf$Date <- as.Date(rf$Date)
rf <- rf[order(rf$Date), ]

##################################################################
##               Merge data and calculate returns               ##
##################################################################

## Select price data only when there is market index data
prices <- merge(ind[, -2], price_raw, by = "Date", all.x = TRUE)
## If an asset hasn't any price data, we eliminate it from our database
prices <- prices[, colSums(is.na(prices)) != nrow(prices)]

## We iterate to fill the NAs in the middle of the sample
prices_locf <- data.frame(matrix(ncol = ncol(prices) - 1, nrow = nrow(prices)))
for (i in 2:ncol(prices)) {
  ind_date <- ind[, 1]
  # Select only the date column and the asset in position i
  suport1 <- prices[, c(1, i)]
  # Create a vector that informs the position of the non NA observations
  NonNAindex <- which(!is.na(suport1[, 2]))
  # Filter so we can work only with the dates before an asset, possibly, delists.
  # This will avoid problems with the na.locf function
  suport2 <- suport1[1:max(NonNAindex), ]
  # Use the na.locf function to replace NAs with the last available information
  suport3 <- na.locf(suport2)
  # Add the asset prices to the data frame using it's date column
  prices_locf[[i - 1]] <- merge(ind_date, suport3, by = "Date", all.x = TRUE)[, 2]
}

## Calculate assets returns from the price data
returns <- as.data.frame(lapply(prices_locf, function(x) diff(x) / x[-length(x)])) %>%
  set_names(colnames(prices)[-1]) %>%
  dplyr::mutate(Date = prices$Date[-1], .before = 1)

## Delete useless variables
rm(ind_date, price_raw, prices, prices_locf, suport1, suport2, suport3, i, NonNAindex)

# Select risk free data only when there is market index data
rf <- merge(ind[, 1], rf, by = "Date", all.x = TRUE)

# Calculate the risk free returns
rf$Risk_free <- na.locf(rf$Risk_free)
rf <- data.frame(Date = rf$Date[-1], Risk_free = diff(rf$Risk_free) / rf$Risk_free[-length(rf$Risk_free)])

# Calculate the market index returns
ind[, 2] <- append(diff(ind[, 2, drop = TRUE]) / ind[, 2, drop = TRUE][-length(ind[, 2, drop = TRUE])], NA, after = 0)
ind <- ind[-1, ]

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 3:                             ###
###                   DEALING WITH BALANCE SHEET DATA                   ###
###                                                                     ###
###########################################################################
###########################################################################

## We have Price to Book (value), Gross Profit to Total Assets (profitability) and
## Market Capitalization (size) data for the assets.
indic_name <- c("value", "profitability", "size")

indic_data <- vector("list", length = length(indic_name))
# Clean the financial indicators data
for (i in seq_along(indic_name)) {
  indic <- read_excel("raw_data\\indicators_raw.xlsx", sheet = i + 1)

  colnames(indic)[1] <- "Date"
  indic <- indic[-1, ]

  # Size has one more useless row
  if (indic_name[i] == "size") {
    indic <- indic[-1, ]
  }

  indic$Date <- as.Date(as.numeric(indic$Date), origin = "1899-12-30")

  indic <- data.frame(Date = indic$Date, apply(indic[, -1], 2, as.numeric)) %>% set_names(colnames(indic))

  indic <- indic[order(indic$Date), ]

  # We make sure that there is no empity column or row
  indic_data[[i]] <- indic[rowSums(is.na(indic[, -1])) != ncol(indic[, -1]), colSums(is.na(indic)) != nrow(indic)]
}

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 4:                             ###
###                          EXPORT CLEAN DATA                          ###
###                                                                     ###
###########################################################################
###########################################################################

folder_name <- paste0(getwd(), "/brazil")

# If you need to create the folder ('/brazil'), uncomment the line below
dir.create(folder_name)


write.csv(assets, paste0(folder_name, "\\comp.csv"), row.names = FALSE)

write.csv(rf, paste0(folder_name, "\\risk_free_returns.csv"), row.names = FALSE)
write.csv(returns, paste0(folder_name, "\\asset_returns.csv"), row.names = FALSE)
write.csv(ind, paste0(folder_name, "\\index_returns.csv"), row.names = FALSE)

write.csv(indic_data[[1]], paste0(folder_name, "\\value.csv"), row.names = FALSE)
write.csv(indic_data[[2]], paste0(folder_name, "\\profitability.csv"), row.names = FALSE)
write.csv(indic_data[[3]], paste0(folder_name, "\\size.csv"), row.names = FALSE)

# Clean all variables so we can run the next code (backtests.R)
rm(list=ls()) 
