
# A file designed to work through all the monthly extracts that are required for monitoring

# Import tidyverse as the base
library(tidyverse)

#Import the custom function package
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Regular Runs.R')

# Select which working directory you want, with or without PINS
getwd()
#setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")
# Set working directory for normal
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All")

# Get list of CSV files
file_list <- list.files(pattern = '*.csv')


# ------------------------------------------------------------------------------

# Nurse Monthly Comparisons ----------------------------------------------------

# Read in the final 2 files
Raw_Data_y1 <- read_csv(file_list[length(file_list)-1])
Raw_Data_y2 <- read_csv(file_list[length(file_list)])

# Call in the function to run all the extracts
process_monthly_extracts()

# Nurse Yearly Comparisons ----------------------------------------------------

# Read in the final 2 files
Raw_Data_y1 <- read_csv(file_list[length(file_list)-12])
Raw_Data_y2 <- read_csv(file_list[length(file_list)])

# Call in the function to run all the extracts
process_annual_extracts()


# Nurse Cumulative Comparisons ----------------------------------------------------

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All")

# Read in the final 2 files
Raw_Data_y1 <- read_csv('Staff in Post - Workforce 202403 extracted May 24.csv')
Raw_Data_y2 <- read_csv(file_list[length(file_list)])

# Call in the function to run all the extracts
process_cumulative_extracts()

