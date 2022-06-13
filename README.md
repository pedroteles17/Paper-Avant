# Identification

* Title: Low Volatility Asset Valuation in Brazilian Stock Market: Lower Risk with Higher Returns

* Authors:

    * Luciano Boudjoukian França 

    * Mario Candido de Avelar Fernandes Filho

    * Pedro Paulo Portella Teles

* Date: June 2022

# Folder Contents

* raw_data/cdi_raw.xlsx: data about the risk-free rate; extracted from the Brazilian Central Bank.

* raw_data/index_comp_raw.xlsx: IBX composition by month; extracted from the Bloomberg Terminal.

* raw_data/indicators_raw.xslx: financial indicators of all companies that are or were part of the IBX index; extracted from the Bloomberg Terminal.

* raw_data/price_raw.xlsx: price of all companies that are or were part of the IBX index; extracted from the Bloomberg Terminal.

* o_setup.R: script to install and load all the required packages (latest  version).

* clean_data.R: takes the raw data and clean it. This cleaned data will be stored as .csv files in a folder called "Brazil".

* functions.R: set of functions that are going to be used to run the backtests. It will be sourced from the other scripts.

* backtests.R: takes the clean data, source the "functions.R" file and runs the backtests. The portfolios will be stored as .csv files in a folder called "Portfolios".

* main_code.R: take the clean data, the portfolio returns, and returns a .xlsx file containing all the tables used in the article.

* draft_manuscript.pdf: Draft Manuscript

# Data Availability Statement

All the Excel files used to suport the findings of this study have been deposited in the "raw_data" folder.

# Computational Requirements

Java 1.8.0_331

R 4.1.1 [64-bit]

The file "00_setup.R" will install all dependencies (latest version), and should be run once prior to running other programs .R. For more information, check the "Instructions" section. 

* pacman (0.5.1)
* tidyverse (1.3.1)
* xts (0.12.1)
* lubridate (1.8.0)
* PerformanceAnalytics (2.0.4)
* parallel (4.1.1)
* furrr (0.2.3)
* readxl (1.4.0)
* writexl (1.4.0)
* xlsx (0.6.5)

The code was last run on a 4 core 11th Gen Intel Core i7-1165G7 laptop, with Windows version 11, 16 GB of RAM, and 512GB of SSD. Computation took 10 minutes.

### Comentary about the xlsx package

The xlsx package loads the rJava package. This package requires Java to be installed in the local machine. If you don't have Java installed, follow the [stackoverflow](https://stackoverflow.com/questions/37735108/r-error-onload-failed-in-loadnamespace-for-rjava) tutorial. A good answer is offered by stevec in Dec 20, 2021 (this can change).

# Instructions

First, make sure that you have both R and Java installed.

Then, the code should be run in the folowing order:

1- 00_setup.R: install and load all dependencies (latest version);

2- 01_clean_data.R: create folder 'brazil' containing the cleaned data;

3- 02_backtests.R: create folder 'portfolios' containing the portfolios returns that will be used to generate the figures and tables;

4- 03_main_code.R: create a 'graphs_tables.xlsx' file containing all the tables and figures used in the article.

No further action is needed on the replicator's part.

Following the procedure described above generates an Excel file that contains all the figures and tables used in the article ('graphs_tables.xlsx'). Each table and figure is stored in a separate tab. The name of each tab indicates wheter it refers to a figure ("F") or a table ("T") and the number of the figure or table. In this case, "T3" means "Table 3" and "F2" means "Figure 2", for example.

If the replicator can't access the Excel file, he can, nonetheless, access all the figures and tables using R through the variables that store this data. The name of each variable indicates if it refers to a figure ("fig") or a table ("tb") and the number of the figure or table. If it's a table, it also indicates which panel ("panel_"). In this case, "tb5_panel_d" means "Table 5 - Panel D" and "fig1" means "Figure 1", for example.