
library(tidyverse)
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions.R')

getwd()
setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/NMC PINS/ESR records with PINS")

Raw_Data_y1 <- read_csv("Staff in Post - Workforce 202307 with PINS.csv")
Raw_Data_y2 <- read_csv("Staff in Post - Workforce 202308 with PINS.csv")
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

convert_to_number <- function(letter) {
  if (toupper(letter) %in% LETTERS[1:12]) {
    return(match(toupper(letter), LETTERS[1:12]))
  } else {
    return(sample(1:12,1))
  }
}

pin_data <- data %>% filter(is.na(Registration_Number_y2) == FALSE)%>%
  #Enforce the length of registration number has to equal 8
  filter(str_length(Registration_Number_y2) == 8)%>%
  #Filter only the registration numbers who begin with two numbers.
  filter(substr(Registration_Number_y2, 1,1) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
  filter(substr(Registration_Number_y2, 2,2) %in% c(1,2,3,4,5,6,7,8,9,0))%>%
  #Filter only the registration numbers that have a legitimate country code
  filter(toupper(substr(Registration_Number_y2, 8,8)) %in% c("A", "C", "D", "E", "N", "O", "S", "W", 0))%>%
  #Filter only the registration numbers that have the correct month format
  filter(toupper(substr(Registration_Number_y2, 3,3)) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "Y", "U"))%>%
  #Recode all the Reg numbers for easier groupings.
  dplyr::mutate(country_of_training = str_sub(Registration_Number_y2,-1)) %>% 
  dplyr::mutate(country_of_training = recode(toupper(country_of_training), 
                                             A = "UK", 
                                             C = "EU",
                                             D = "UK",
                                             E = "England",
                                             N = "Northern Ireland",
                                             O = "Overseas (non EU)",
                                             '0' = 'Overseas (non EU)',
                                             S = "Scotland",
                                             W = "Wales"
  )
  )%>%
  #Recode the month and year 
  dplyr::mutate(registration_year = str_sub(Registration_Number_y2, 1,2))%>%
  dplyr::mutate(registration_month = str_sub(Registration_Number_y2, 3,3))%>%
  dplyr::mutate(registration_letter = registration_month)

pin_data$registration_month <- sapply(pin_data$registration_month, convert_to_number)

pin_data <- pin_data %>%
  dplyr::mutate(registration_date = if_else(registration_year < 25, paste0(20,registration_year,"-",registration_month,"-01"),
                                            paste0(19,registration_year,"-",registration_month,"-01")))%>%
  select(-c(registration_month, registration_year))%>%
  filter(joiner > 0 | occ_joiner > 0)%>%
  #Recode the current ESR month to a numeric number
  mutate(current_month = paste0(as.numeric(substr(Tm_Year_Month_y2,1,4)),"-",ifelse(substr(Tm_Year_Month_y2,6,8) == "JAN", 01,
                                                                                    ifelse(substr(Tm_Year_Month_y2,6,8) == "FEB", 02,
                                                                                           ifelse(substr(Tm_Year_Month_y2,6,8) == "MAR", 03,
                                                                                                  ifelse(substr(Tm_Year_Month_y2,6,8) == "APR", 04,
                                                                                                         ifelse(substr(Tm_Year_Month_y2,6,8) == "MAY", 05,
                                                                                                                ifelse(substr(Tm_Year_Month_y2,6,8) == "JUN", 06,
                                                                                                                       ifelse(substr(Tm_Year_Month_y2,6,8) == "JUL", 07,
                                                                                                                              ifelse(substr(Tm_Year_Month_y2,6,8) == "AUG", 08,
                                                                                                                                     ifelse(substr(Tm_Year_Month_y2,6,8) == "SEP", 09,
                                                                                                                                            ifelse(substr(Tm_Year_Month_y2,6,8) == "OCT", 10,
                                                                                                                                                   ifelse(substr(Tm_Year_Month_y2,6,8) == "NOV", 11, 12
                                                                                                                                                   ))))))))))),"-01")
         
  )


#Convert registration date, date of birth and current month to datetime
pin_data$registration_date <- ymd(pin_data$registration_date)
pin_data$Date_Of_Birth_y2 <- ymd(pin_data$Date_Of_Birth_y2)
pin_data$current_month <- ymd(pin_data$current_month)

#Add a column to calculate age at date of registration
pin_data$age_check <- as.numeric(difftime(pin_data$registration_date,
                                          pin_data$Date_Of_Birth_y2,
                                          unit = "weeks"))/52.25

#Filter out the registration numbers that are before the date of birth and before the current month
#And people under the age of 16
Data_birth_filter <- pin_data %>% filter(Date_Of_Birth_y2 < registration_date)%>%
  filter(registration_date <= current_month)%>%
  filter(age_check > 16)
























