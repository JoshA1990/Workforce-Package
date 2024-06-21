

# Import tidyverse as the base
library(tidyverse)

# Import here to help with source navigation
library(here)

#Import the custom function package
#source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Main Processing.R')
source(paste0(here("Functions"),"/Main Processing.R"))

# Select which working directory you want, with or without PINS
getwd()

# Set working directory for normal
setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All"))

# Set working directory for pins
#setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")


# Get list of CSV files
file_list <- list.files(pattern = '*.csv')

# Loop over each pair of consecutive files
for (i in seq_along(file_list)) {
  if (i < length(file_list)) {  # Ensure there is a next file to compare
    setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All"))
    
    # Read raw data for the current and next files
    Raw_Data_y1 <- read_csv(file_list[i])
    Raw_Data_y2 <- read_csv(file_list[i + 12])
    
    # Process data
    summary <- process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                            raw_data_y2 = Raw_Data_y2,
                                            staff_group_code = midwife_staff_codes,
                                            staff_group_name = 'Midwife',
                                            summarization_variable = 'Nationality_grouping')

    # Write summary to CSV
    write_csv(summary, paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/',
                              substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],
                              '.csv'))
    
  }
}


file_list <- file_list[-1:-36]


for (i in seq_along(file_list)) {
  if (i < length(file_list)) {  # Ensure there is a next file to compare
    # Read raw data for the current and next files
    setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All"))
    
    Raw_Data_y1 <- read_csv(file_list[i])
    
    # Process data
    summary <- process_stock(raw_data = Raw_Data_y1,
                             staff_group_code = midwife_staff_codes,
                             staff_group_name = 'Midwife',
                             summarization_variable = 'Nationality_grouping')
                             

    
    # Write summary to CSV
    write_csv(summary, paste0('C:/Users/',
                              Sys.getenv("USERNAME"),
                              '/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/',
                              ,substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
    
  }
}
