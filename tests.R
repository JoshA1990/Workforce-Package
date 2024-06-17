
# Import tidyverse as the base
library(tidyverse)

#Import the custom function package
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Joiner Leaver Functions.R')
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/workforce_staff_codes.R')

#reading in the base data
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Data")
nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")

# Reading in pin csv files
getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")

Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202307 with PINS.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202308 with PINS.csv")



raw_data_y1 <- staff_filter(Raw_Data_y1,
                            suffix = '_y1',
                            staff_group_code = nurse_staff_codes,
                            staff_group = 'Nurse',
                            summarization_variable = 'Nationality_grouping',
                            headcount = TRUE)

raw_data_y2 <- staff_filter(Raw_Data_y2,
                            suffix = '_y2',
                            staff_group_code = nurse_staff_codes,
                            staff_group = 'Nurse',
                            summarization_variable = 'Nationality_grouping',
                            headcount = TRUE)


#Rename unique identifier to remove the suffix so it's easier to merge datasets later on
raw_data_y1 <- rename(raw_data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
raw_data_y2 <- rename(raw_data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)

#ordering data by fte and then staff group
raw_data_y1 <- raw_data_y1[order(raw_data_y1$Staff_group_y1,-raw_data_y1$Contracted_Wte_y1),]
raw_data_y2 <- raw_data_y2[order(raw_data_y2$Staff_group_y2,-raw_data_y2$Contracted_Wte_y2),]

#removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
raw_data_y1_dedup <- raw_data_y1[ !duplicated(raw_data_y1$Unique_Nhs_Identifier), ]
raw_data_y2_dedup <- raw_data_y2[ !duplicated(raw_data_y2$Unique_Nhs_Identifier), ]

#Join datasets
data <- full_join(raw_data_y1_dedup, raw_data_y2_dedup, by = "Unique_Nhs_Identifier")

# Remove unused data
rm(raw_data_y1_dedup)

data <- within(data, rm('Age_In_Years_y1', 'Age_In_Years_y2'))

#Call function to clean the joined data
data <- clean_joined_data(data, summarization_variable = 'Nationality_grouping')

#Call function to create joiner leaver flags, 1 parameter of Staff type
data <- joiner_leaver_flags(data, staff_group = "Nurse", summarization_variable = 'Nationality_grouping')

# Join datasets to get a full FTE later on
raw_data_y1 <- merge(raw_data_y1, raw_data_y2_dedup, by='Unique_Nhs_Identifier', all.x=TRUE)

#get rid of unsused data
rm(raw_data_y2_dedup)

raw_data_y1 <- overwrite_y1_nationality(raw_data_y1)

source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/PIN Functions.R')


data <- nmc_pin_cleaning(data)

# Define the columns to remove
columns_to_remove <- c('User_Person_Type_y1', 'User_Person_Type_y1', 'registration_letter', 
                       'FTE_change', 'nhs_provider_joiner', 'nhs_provider_leaver',
                       'other_leaver', 'other_joiner', 'active_to_non_active', 'non_active_to_active')

# Find the columns that exist in both the data frame and the columns to remove
existing_columns_to_remove <- intersect(columns_to_remove, names(data))

# Remove the existing columns
test <- data[, !names(data) %in% existing_columns_to_remove]


summary_test <- pin_data_summary(test, pin_summary_variable = 'age')

write_csv(summary_test, paste0('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/R Outputs/Pin test 2',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))


# 
# test <- process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
#                                      raw_data_y2 = Raw_Data_y2,
#                                      staff_group_code = nurse_staff_codes,
#                                      staff_group_name = 'Nurse',
#                                      summarization_variable = 'Nationality_grouping',
#                                      headcount = TRUE)






# write.csv(test, paste("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data/R Outputs/Headcount test 2.csv"))

# Raw_Data_y2 <- Raw_Data_y1
# 
# test <- process_stock(Raw_Data_y1,
#                       staff_group_code = midwife_staff_codes,
#                       staff_group_name = 'Midwife',
#                       summarization_variable = 'Organisation_Type')


################################################################################
raw_data_y1 <- staff_filter(Raw_Data_y1,
                            suffix = '_y1',
                            staff_group_code = nurse_staff_codes,
                            staff_group = 'Nurse',
                            summarization_variable = 'region')

raw_data_y2 <- staff_filter(Raw_Data_y2,
                            suffix = '_y2',
                            staff_group_code = nurse_staff_codes,
                            staff_group = 'Nurse',
                            summarization_variable = 'region')

#Rename unique identifier to remove the suffix so it's easier to merge datasets later on
raw_data_y1 <- rename(raw_data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
raw_data_y2 <- rename(raw_data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)

#ordering data by fte and then staff group
raw_data_y1 <- raw_data_y1[order(raw_data_y1$Staff_group_y1,-raw_data_y1$Contracted_Wte_y1),]
raw_data_y2 <- raw_data_y2[order(raw_data_y2$Staff_group_y2,-raw_data_y2$Contracted_Wte_y2),]

#removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
raw_data_y1_dedup <- raw_data_y1[ !duplicated(raw_data_y1$Unique_Nhs_Identifier), ]
raw_data_y2_dedup <- raw_data_y2[ !duplicated(raw_data_y2$Unique_Nhs_Identifier), ]

#Join datasets
data <- full_join(raw_data_y1_dedup, raw_data_y2_dedup, by = "Unique_Nhs_Identifier")

# Remove unused data
rm(raw_data_y1_dedup)

#Call function to clean the joined data
data <- clean_joined_data(data, summarization_variable = 'region')

#Call function to create joiner leaver flags, 1 parameter of Staff type
data <- joiner_leaver_flags(data, staff_group = "Nurse", summarization_variable = 'region')

# Join datasets to get a full FTE later on
raw_data_y1 <- merge(raw_data_y1, raw_data_y2_dedup, by='Unique_Nhs_Identifier', all.x=TRUE)

#get rid of unsused data
rm(raw_data_y2_dedup)

raw_data_y1 <- overwrite_y1_nationality(raw_data_y1)

summary <- create_joiner_leaver_summary(data, summarization_variable = 'region')

fte_y1 <- fte_y1_function(raw_data_y1, staff_group = "Nurse", summarization_variable = 'region')
fte_y2 <- fte_y2_function(raw_data_y2, staff_group = "Nurse", summarization_variable = 'region')


pivot <- summary_fte_combine(summary, fte_y1, fte_y2,summarization_variable = 'region', data)



# ------------------------------------------------------------------------------

