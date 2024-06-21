
# Import tidyverse as the base
library(tidyverse)

library(here)

source(paste0(here("Variables"),"/workforce_staff_codes.R"))
source(paste0(here("Functions"), "/Joiner Leaver Functions.R"))

#Import the custom function package
#source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Joiner Leaver Functions.R')
#source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/workforce_staff_codes.R')

#reading in the base data
setwd(here("Data"))
nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")

# Reading in pin csv files
getwd()
setwd(paste0("C:/Users/",Sys.getenv("USERNAME"),"/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/All"))

file_list <- list.files(patter = "*.csv")

Raw_Data_y1 <- read_csv(file_list[length(file_list)-12])
Raw_Data_y2 <- read_csv(file_list[length(file_list)])


################################################################################
raw_data_y1 <- staff_filter(Raw_Data_y1,
                            suffix = '_y1',
                            staff_group_code = nurse_staff_codes,
                            staff_group = 'Nurse',
                            summarization_variable = 'trust')

raw_data_y2 <- staff_filter(Raw_Data_y2,
                            suffix = '_y2',
                            staff_group_code = nurse_staff_codes,
                            staff_group = 'Nurse',
                            summarization_variable = 'trust')

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
data <- clean_joined_data(data, summarization_variable = 'trust')

#Call function to create joiner leaver flags, 1 parameter of Staff type
data <- joiner_leaver_flags(data, staff_group = "Nurse", summarization_variable = 'trust')

# Join datasets to get a full FTE later on
raw_data_y1 <- merge(raw_data_y1, raw_data_y2_dedup, by='Unique_Nhs_Identifier', all.x=TRUE)

#get rid of unsused data
rm(raw_data_y2_dedup)

raw_data_y1 <- overwrite_y1_nationality(raw_data_y1)

test <- data %>% 
  mutate(trust = if_else(is.na(trust), if_else(is.na(trust_y1), trust_y2, trust_y1), trust))%>%
  filter(is.na(trust))

summary <- create_joiner_leaver_summary(data, summarization_variable = 'trust')

fte_y1 <- fte_y1_function(raw_data_y1, staff_group = "Nurse", summarization_variable = 'trust')
fte_y2 <- fte_y2_function(raw_data_y2, staff_group = "Nurse", summarization_variable = 'trust')


pivot <- summary_fte_combine(summary, fte_y1, fte_y2,summarization_variable = 'trust', data)

write_csv(pivot, paste0('C:/Users/',
                        Sys.getenv("USERNAME"),
                        '/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/test.csv'))
# ------------------------------------------------------------------------------

