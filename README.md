# Identification


* Title: Low Volatility Asset Valuation in Brazilian Stock Market: Lower Risk with Higher Returns

* Authors:

    * Luciano Boudjoukian França 

    * Mario Candido de Avelar Fernandes Filho

    * Pedro Paulo Portella Teles

* Date: April 2022

# Folder Contents

* Data Cleaning/cdi_raw.xlsx: data about the risk-free rate; extracted from the Brazilian Central Bank API.

* Data Cleaning/index_comp_raw.xlsx: IBX composition by month; extracted from the Bloomberg Terminal.

* Data Cleaning/indicators_raw.xslx: financial indicators of all companies that are or were part of the IBX index; extracted from the Bloomberg Terminal.

* Data Cleaning/price_raw.xlsx: price of all companies that are or were part of the IBX index; extracted from the Bloomberg Terminal.

* o_setup.R: script to install and load all the required packages (latest  version).

* clean_data.R: takes the raw data and clean it. This cleaned data will be stored as .csv files in a folder called "Brazil".

* functions.R: set of functions that are going to be used to run the backtests. It will be sourced from the other scripts.

* backtests.R: takes the clean data, source the "functions.R" file and runs the backtests. The portfolios will be stored as .csv files in a folder called "Portfolios".

* main_code.R: take the clean data, the portfolio returns, and returns a .xlsx file containing all the tables used in the article.

# Data Availability Statement

All the Excel files used to suport the findings of this study have been deposited in the "raw_data" folder.

# Computational Requirements

R 4.1.1 [64-bit]

* tidyverse (1.3.1)
* xts (0.12.1)
* lubridate (1.8.0)
* PerformanceAnalytics (2.0.4)
* parallel (4.1.1)
* furrr (0.2.3)
* readxl (1.4.0)
* writexl (1.4.0)
* xlsx (0.6.5)


### Comentary about the xlsx package

The xlsx package loads the rJava package. This package requires Java to be installed in the local machine. If you don't have Java installed, follow the [stackoverflow](https://stackoverflow.com/questions/37735108/r-error-onload-failed-in-loadnamespace-for-rjava) tutorial. A good answer is offered by stevec in Dec 20, 2021 (this can change).

# Instructions

The code should be run in the folowing order:

1- o_setup.R;

2- clean_data.R;

3- backtests.R;

4- main_code.R.