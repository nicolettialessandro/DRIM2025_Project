# Load necessary libraries
library(dplyr)
library(readxl)

# Define the path for all files
data_path <- "C:/Users/Gaia/Documents/magistrale/AMUniversite/credit risk/DRIM2025_Project0/01_data_clean/macro/"

# Load the U.S. data files
unrate_usa <- read_excel(paste0(data_path, "UNRATE_USA.xlsx"))
inflation_usa <- read_excel(paste0(data_path, "inflation_USA.xlsx"))
gdp_usa <- read_excel(paste0(data_path, "GDP_USA.xlsx"))
gpr_usa <- read_excel(paste0(data_path, "gpr_USA.xlsx"))
credit_spread_usa <- read_excel(paste0(data_path, "credit_spread_USA.xlsx"))
bond_yield_usa <- read_excel(paste0(data_path, "yield_USA.xlsx"))