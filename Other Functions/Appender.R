
#                                  APPENDER


###################################Install and load the packages###################################
#(note only need to install packages once, but need to reload library each time)


#data wrangling/ analysis package
library(tidyverse)


#First set the working directory
getwd()
setwd('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/R Outputs/Midwife/Tracking/Joiners Leavers Yearly FTE')



#######################################################

# List all files in the directory 
files <- list.files(pattern = "*.csv") 

# Define a function to convert month abbreviations to numeric format 
convert_month <- function(month) { 
  month_num <- match(tolower(month), tolower(month.abb)) 
  if (is.na(month_num)) { 
    stop("Invalid month abbreviation: ", month) 
  } 
  return(formatC(month_num, width = 2, format = "d", flag = "0")) 
} 


# Rename files based on the specified format 
for (file in files) { 
  # Extract year and month from the file name 
  file_info <- strsplit(file, "-|\\.csv")[[1]] 
  year <- file_info[1] 
  month <- file_info[2] 
  # Convert month abbreviation to numeric format 
  month_num <- convert_month(month) 
  # Create the new file name 
  new_file <- paste0(year, "-", month_num, ".csv") 
  # Rename the file 
  file.rename(file, new_file) 
} 


###############################################################################


# List all CSV files in the directory 
csv_files <- list.files(pattern = "*.csv", full.names = TRUE) 

# Initialize an empty data frame to store the joined data 
joined_data <- data.frame() 


# Loop through each CSV file and perform the join 
for (file in csv_files) { 
  # Read the CSV file 
  df <- read.csv(file) 
  
  # Perform the join 
  if (nrow(joined_data) == 0) { 
    # If it's the first file, assign it to the joined_data 
    joined_data <- df 
  } else { 
    # If not the first file, perform the join based on common columns
    joined_data <- left_join(joined_data, df, by = c("Nationality_grouping","name")) 
  } 
} 
# Only if needed ---------------------------------------------------------------

#Replace column names as will always prefix and X in the beginning
names(joined_data) <- sapply(str_remove_all(colnames(joined_data), "X"), "[")

#Swap the . for a _ in column names as a personal preference.
names(joined_data) <- gsub(x = names(joined_data), pattern = "\\.", replacement = "_")

# Define order which matches previous extracts
order_nationality <- c("EU", "ROW", "UK", "Unknown", "All", "Domestic", "IR")

# Reorder the column to the correct order
joined_data <- joined_data %>% 
  mutate(Nationality_grouping = factor(Nationality_grouping, levels = order_nationality))%>%
  arrange(Nationality_grouping)



#Export to area
write_csv(joined_data, 'C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/R Outputs/Midwife/Tracking/Joiners Leavers Yearly FTE/Joined.csv')

