



###############################   MAIN RUNS    #################################
# Code for comparing two time periods and joiners and leavers ------------------

#' Process Data for Joiners and Leavers
#'
#' This function processes raw data for joiners and leavers, performing various data manipulations and generating summaries.
#'
#' @param raw_data_y1 A data frame containing the raw data for Year 1.
#' @param raw_data_y2 A data frame containing the raw data for Year 2.
#' @param staff_group_code A character string specifying the staff group code.
#' @param staff_group_name A character string specifying the staff group name.
#' @param summarization_variable A character string indicating the variable used for summarization. 
#' Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#' @param headcount A logical value indicating whether to include headcount data (default is FALSE).
#'
#' @return A melted data frame with combined summary groups, including leaver rates and occupational leaver rates.
#'
#' @details
#' The function performs the following operations:
#' - Reads and preprocesses auxiliary data files, including nationality groupings and NHS organization codes.
#' - Filters raw data based on staff group code, staff group name, and summarization variable.
#' - Renames Unique_Nhs_Identifier columns and removes duplicates.
#' - Joins Year 1 and Year 2 datasets, fills unknowns for nationality, and creates joiner and leaver flags.
#' - Starts summaries and calculates Full-Time Equivalent (FTE) values for Year 1 and Year 2.
#' - Combines leavers, joiners, and FTE values into a single data frame.
#' - Removes unnecessary objects.
#'
#' @examples
#' \dontrun{
#' processed_data <- process_data_joiners_leavers(raw_data_y1, raw_data_y2, staff_group_code, staff_group_name, summarization_variable = 'Nationality_grouping')
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @importFrom utils setwd
#' @export


process_data_joiners_leavers <- function(raw_data_y1,
                                         raw_data_y2,
                                         staff_group_code,
                                         staff_group_name,
                                         summarization_variable,
                                         headcount = FALSE) {
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Joiner Leaver Functions.R')
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/workforce_staff_codes.R')
  
  # Step 1: Start the data manipulation
  # Rename columns in nationality
  setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Data")
  nationality <- read_csv("Nationality groupings.csv")
  NHS_orgs <- read_csv("ORG Codes NHS Digital.csv")
  colnames(nationality) <- gsub("[ ]", "_", colnames(nationality), perl=TRUE)
  
  # Call each base file and run the wrangling on them
  raw_data_y1 <- staff_filter(raw_data_y1, '_y1', NHS_orgs, nationality, staff_group_code, staff_group_name, summarization_variable, headcount)
  raw_data_y2 <- staff_filter(raw_data_y2, '_y2', NHS_orgs, nationality, staff_group_code, staff_group_name, summarization_variable, headcount)
  
  # Rename Unique_Nhs_Identifier columns
  names(raw_data_y1)[names(raw_data_y1) == 'Unique_Nhs_Identifier_y1'] <- 'Unique_Nhs_Identifier'
  names(raw_data_y2)[names(raw_data_y2) == 'Unique_Nhs_Identifier_y2'] <- 'Unique_Nhs_Identifier'
  
  # Find duplicates and keep the first entry of each
  raw_data_y1_dedup <- raw_data_y1[!duplicated(raw_data_y1$Unique_Nhs_Identifier), ]
  raw_data_y2_dedup <- raw_data_y2[!duplicated(raw_data_y2$Unique_Nhs_Identifier), ]
  
  # Join datasets
  data <- merge(raw_data_y1_dedup, raw_data_y2_dedup, by="Unique_Nhs_Identifier", all=TRUE)
  
  # Remove unique datasets
  rm(raw_data_y1_dedup)
  
  # Fill in unknowns for nationality
  data <- clean_joined_data(data, summarization_variable)
  
  # Create joiner and leaver flags
  data <- joiner_leaver_flags(data, staff_group_name, summarization_variable)
  
  # Join datasets to get total FTE later on
  raw_data_y1 <- merge(raw_data_y1, raw_data_y2_dedup, by='Unique_Nhs_Identifier', all.x=TRUE)
  raw_data_y1 <- overwrite_y1_nationality(raw_data_y1)
  
  # Start summaries
  summary <- create_joiner_leaver_summary(data, summarization_variable)
  
  # Get FTE for Y1 and Y2
  fte_y1 <- fte_y1_function(raw_data_y1, staff_group_name, summarization_variable)
  fte_y2 <- fte_y2_function(raw_data_y2, staff_group_name, summarization_variable)
  
  # Combine leavers joiners with FTE
  pivot <- summary_fte_combine(summary, fte_y1, fte_y2, summarization_variable, data)
  
  # Remove unnecessary objects
  rm(fte_y1, fte_y2, raw_data_y2_dedup)
  
  return(pivot)
}

# Function to process stock ----------------------------------------------------

#' Process Stock Data
#'
#' This function processes raw stock data, performing various data manipulations and generating summaries.
#'
#' @param raw_data A data frame containing the raw stock data.
#' @param staff_group_code A character string specifying the staff group code.
#' @param staff_group_name A character string specifying the staff group name.
#' @param summarization_variable A character string indicating the variable used for summarization. 
#' Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#' @param headcount A logical value indicating whether to include headcount data (default is FALSE).
#' @param stock A logical value indicating whether the data is related to stock (default is TRUE).
#'
#' @return A summarized data frame based on the specified summarization variable.
#'
#' @details
#' The function performs the following operations:
#' - Reads and preprocesses auxiliary data files, including nationality groupings and NHS organization codes.
#' - Calls the staff_filter function to filter raw data based on staff group code, staff group name, and summarization variable.
#' - Cleans the filtered stock data.
#' - Generates a summary of the stock data based on the specified summarization variable.
#'
#' @examples
#' \dontrun{
#' processed_stock_data <- process_stock(raw_stock_data, staff_group_code, staff_group_name, summarization_variable = 'Nationality_grouping')
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @importFrom utils setwd
#' @export


process_stock <- function(raw_data, staff_group_code, staff_group_name, summarization_variable, headcount = FALSE, stock = TRUE) {
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Joiner Leaver Functions.R')
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Stock Functions.R')
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/workforce_staff_codes.R')
  
  
  # Step 1: Start the data manipulation
  # Load and preprocess nationality data
  setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Data")
  nationality <- read_csv("Nationality groupings.csv")
  NHS_orgs <- read_csv("Org Codes NHS Digital.csv")
  colnames(nationality) <- gsub("[ ]", "_", colnames(nationality), perl=TRUE)
  
  # Call staff_filter with the loaded nationality data

  data <- staff_filter(raw_data,
                       suffix = 'none',  # Set the default suffix
                       NHS_orgs_data = NHS_orgs,
                       nationality_data = nationality,  # Pass the loaded nationality data
                       staff_group_code = staff_group_code,
                       staff_group_name = staff_group_name,
                       summarization_variable = summarization_variable,
                       stock = TRUE,
                       headcount = headcount) 
  
  
  
  # Other processing steps
  print('starting clean stock data')
  data <- clean_stock_data(data, staff_group_name, summarization_variable)
  
  print('starting stock summary')
  data <- create_stock_summary(data, summarization_variable)
  
  return(data)
}

# Function to process PIN joiners

#' Process Data for Pin joiners
#'
#' This function processes raw data for joiners with PINS, performing various data manipulations and generating summaries.
#'
#' @param raw_data_y1 A data frame containing the raw data for Year 1.
#' @param raw_data_y2 A data frame containing the raw data for Year 2.
#' @param staff_group_code A character string specifying the staff group code.
#' @param staff_group_name A character string specifying the staff group name.
#' @param summarization_variable A character string indicating the variable used for summarization for normal extracts. Fixed as 'Nationality_grouping' for PINS
#' @param headcount A logical value indicating whether to include headcount data (keep as TRUE for PINS).
#' @param pin_summary_variable A character string indicating the variable to summarise the PINS on. Can be 'age', 'age_group', 'NMC_to_ESR' and 'country_of_training'.
#'
#' @return A melted data frame with combined summary groups, including leaver rates and occupational leaver rates.
#'
#' @details
#' The function performs the following operations:
#' - Reads and preprocesses auxiliary data files, including nationality groupings and NHS organization codes.
#' - Filters raw data based on staff group code, staff group name, and summarization variable.
#' - Renames Unique_Nhs_Identifier columns and removes duplicates.
#' - Joins Year 1 and Year 2 datasets, fills unknowns for nationality, and creates joiner and leaver flags.
#' - Cleans the PINS within the data, removing entries with incorrect formatting.
#' - Summarises on the variable required.
#'
#' @examples
#' \dontrun{
#' processed_data <- process_pin_joiners(raw_data_y1, raw_data_y2, staff_group_code = nurse_staff_codes, staff_group_name = 'Nurse', summarization_variable = 'Nationality_grouping', headcount = TRUE, pin_summary_variable = 'age')
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import readr
#' @importFrom utils setwd
#' @export


process_pin_joiners <- function(raw_data_y1, raw_data_y2, staff_group_code, staff_group_name, summarization_variable = 'Nationality_grouping', headcount = TRUE, pin_summary_variable = NULL, uk_nqn = FALSE) {
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Joiner Leaver Functions.R')
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/PIN Functions.R')
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/workforce_staff_codes.R')
  
  # Check if 'staff_group_code' is within the predefined sets of codes
  check_staff_group_code <- function(staff_group_code_name) {
    if (!(staff_group_code_name %in% c("nurse_staff_codes", "midwife_staff_codes"))) {
      stop("Invalid staff group. PIN processing can only be done with Nurse (nurse_staff_codes) or Midwives (midwife_staff_codes).")
    }
  }
  # Get the name of the staff_group_code variable
  staff_group_code_name <- deparse(substitute(staff_group_code))
  
  # Check if the staff_group_code is valid
  check_staff_group_code(staff_group_code_name)
  
  if (summarization_variable != 'Nationality_grouping') {
    stop("Invalid summarization variable. For PIN data only use 'Nationality_grouping'.")
  }
  
  if (headcount == FALSE) {
    stop("Invalid argument. Headcount must be TRUE for Pin analysis.")
  }
  
  if (is.null(pin_summary_variable)) {
    stop("Invalid pin_summary_variable. Choose from 'age', 'age_group', 'NMC_to_ESR' or 'country_of_training'.")
  }
  
  
  # Step 1: Start the data manipulation
  # Rename columns in nationality
  setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Data")
  nationality <- read_csv("Nationality groupings.csv")
  NHS_orgs <- read_csv("ORG Codes NHS Digital.csv")
  colnames(nationality) <- gsub("[ ]", "_", colnames(nationality), perl=TRUE)
  
  # Call each base file and run the wrangling on them
  raw_data_y1 <- staff_filter(raw_data_y1, '_y1', NHS_orgs, nationality, staff_group_code, staff_group_name, summarization_variable, headcount)
  raw_data_y2 <- staff_filter(raw_data_y2, '_y2', NHS_orgs, nationality, staff_group_code, staff_group_name, summarization_variable, headcount)
  
  # Rename Unique_Nhs_Identifier columns
  names(raw_data_y1)[names(raw_data_y1) == 'Unique_Nhs_Identifier_y1'] <- 'Unique_Nhs_Identifier'
  names(raw_data_y2)[names(raw_data_y2) == 'Unique_Nhs_Identifier_y2'] <- 'Unique_Nhs_Identifier'
  
  # Find duplicates and keep the first entry of each
  raw_data_y1_dedup <- raw_data_y1[!duplicated(raw_data_y1$Unique_Nhs_Identifier), ]
  raw_data_y2_dedup <- raw_data_y2[!duplicated(raw_data_y2$Unique_Nhs_Identifier), ]
  
  # Join datasets
  data <- merge(raw_data_y1_dedup, raw_data_y2_dedup, by="Unique_Nhs_Identifier", all=TRUE)
  
  # Remove unique datasets
  rm(raw_data_y1_dedup)
  
  # Fill in unknowns for nationality
  data <- clean_joined_data(data, summarization_variable)
  
  # Create joiner and leaver flags
  data <- joiner_leaver_flags(data, staff_group_name, summarization_variable)
  
  
  
  # Run the cleaning function
  pin_data <- nmc_pin_cleaning(data)
  
  # Define the columns to remove
  columns_to_remove <- c('User_Person_Type_y1', 'User_Person_Type_y1', 'registration_letter', 
                         'FTE_change', 'nhs_provider_joiner', 'nhs_provider_leaver',
                         'other_leaver', 'other_joiner', 'active_to_non_active', 'non_active_to_active')
  
  # Find the columns that exist in both the data frame and the columns to remove
  existing_columns_to_remove <- intersect(columns_to_remove, names(pin_data))
  
  # Remove the existing columns
  pin_data <- pin_data[, !names(pin_data) %in% existing_columns_to_remove]
  
  
  summary <- pin_data_summary(pin_data, pin_summary_variable, uk_nqn)
  
  return(summary)
}


process_nurse_trust <- function(staff_group_code, staff_group_name) {
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/workforce_staff_codes.R')
  #reading in the base data
  setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Data")
  nationality <- read_csv('Nationality groupings.csv')
  NHS_orgs <- read_csv('Org Codes NHS Digital.csv')
  
  
  #Amend column names so the dots are replaced with spaces
  colnames(Raw_Data_y1) <- str_replace_all(colnames(Raw_Data_y1), " ", "_")
  colnames(Raw_Data_y2) <- str_replace_all(colnames(Raw_Data_y2), " ", "_")
  colnames(nationality) <- str_replace_all(colnames(nationality), " ", "_")
  
  #join NHS organisation codes and nationality
  Raw_Data_y1 <- full_join(Raw_Data_y1,NHS_orgs)
  Raw_Data_y1 <- full_join(Raw_Data_y1,nationality)
  Raw_Data_y2 <- full_join(Raw_Data_y2,NHS_orgs)
  Raw_Data_y2 <- full_join(Raw_Data_y2,nationality)
  
  #Adding sufixes to variables to separate variable names between datasets
  colnames(Raw_Data_y1) <- paste(colnames(Raw_Data_y1), "y1", sep = "_")
  colnames(Raw_Data_y2) <- paste(colnames(Raw_Data_y2), "y2", sep = "_")
  
  #Rename unique identifier to remove the suffix so it's easier to merge datasets later on
  Raw_Data_y1 <- rename(Raw_Data_y1, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y1)
  Raw_Data_y2 <- rename(Raw_Data_y2, Unique_Nhs_Identifier = Unique_Nhs_Identifier_y2)
  
  #create flags for active nurses, on both datasets, will be important depending on whether looking at joiners or leavers later
  Raw_Data_y1 <- Raw_Data_y1 %>% 
    mutate(Staff_group_y1 = if_else(Occupation_Code_y1 %in% staff_group_code, staff_group_name, 'x.Other'))%>%
    mutate(Status_y1 = if_else(Status_y1 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y1 = if_else(Asg_Type_Of_Contract_y1 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y1=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y1 =if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    mutate (Nationality_grouping_y1_v2 = if_else(Nationality_grouping_y1 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y1 %in% c('UK','Unknown'),'Domestic','Other')))%>%
    group_by(Ocs_Code_y1)
  
  Raw_Data_y2 <- Raw_Data_y2 %>%
    mutate(Staff_group_y2 = if_else(Occupation_Code_y2 %in% staff_group_code, staff_group_name, 'x.Other'))%>%  
    mutate(Status_y2 = if_else(Status_y2 %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract_y2 = if_else(Asg_Type_Of_Contract_y2 %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract_y2=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping_y2 =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    mutate (Nationality_grouping_y2_v2 = if_else(Nationality_grouping_y2 %in% c('ROW','EU'), 'IR',
                                                 if_else(Nationality_grouping_y2 %in% c('UK','Unknown'),'Domestic','Other')))%>%
    group_by(Ocs_Code_y2)
  
  
  #ordering data by fte and then staff group
  Raw_Data_y1 <- Raw_Data_y1[order(Raw_Data_y1$Staff_group_y1,-Raw_Data_y1$Contracted_Wte_y1),]
  Raw_Data_y2 <- Raw_Data_y2[order(Raw_Data_y2$Staff_group_y2,-Raw_Data_y2$Contracted_Wte_y2),]
  
  #removing all duplications in Person_Unique_Nhs_Identifier so there's only one entry for each
  Raw_Data_y1_dedup <- Raw_Data_y1[ !duplicated(Raw_Data_y1$Unique_Nhs_Identifier), ]
  Raw_Data_y2_dedup <- Raw_Data_y2[ !duplicated(Raw_Data_y2$Unique_Nhs_Identifier), ]
  
  #Join datasets
  Data <- full_join(Raw_Data_y1_dedup, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #merge nationality into a single field and override NA nationality with Unknowns and NA NHS providers with 0s
  Data <- Data %>%
    mutate (Nationality = if_else(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)) %>%
    mutate (Nationality_grouping =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)) %>%
    mutate (Nationality_grouping = if_else(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                              if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))) %>%
    mutate (NHSD_trust_or_CCG_y1 = if_else(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)) %>% 
    mutate (NHSD_trust_or_CCG_y2 = if_else(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0))
  
  #joiner/ leaver flags
  Data <- Data %>%
    #joiner flags
    mutate(joiner = if_else(is.na(Staff_group_y1) == TRUE & Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(occ_joiner = if_else(Staff_group_y1 != staff_group_name & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == c(staff_group_name) & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(non_active_to_active = if_else(Staff_group_y1 == staff_group_name & Status_y1 != "Active" & NHSD_trust_or_CCG_y1 == "1" & Staff_group_y2 == staff_group_name & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(nhs_provider_joiner = if_else(Staff_group_y1 == staff_group_name & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == staff_group_name & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    mutate(other_joiner = if_else(Staff_group_y1 != staff_group_name & NHSD_trust_or_CCG_y1 == "0" & Staff_group_y2 == staff_group_name & Status_y2 == "Active" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2, 0)) %>%
    #leaver flags
    mutate(leaver = if_else(is.na(Staff_group_y2) == TRUE & Staff_group_y1 %in% c(staff_group_name) & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(occ_leaver = if_else(Staff_group_y2 != staff_group_name & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c(staff_group_name) & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(active_to_non_active = if_else(Staff_group_y2 == staff_group_name & Status_y2 != "Active" & NHSD_trust_or_CCG_y2 == "1" & Staff_group_y1 == c(staff_group_name) & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(nhs_provider_leaver = if_else(Staff_group_y2 == staff_group_name & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == staff_group_name & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    mutate(other_leaver = if_else(Staff_group_y2 != staff_group_name & NHSD_trust_or_CCG_y2 == "0" & Staff_group_y1 == staff_group_name & Status_y1 == "Active" & NHSD_trust_or_CCG_y1 == "1", Contracted_Wte_y1*-1, 0)) %>%
    #FTE change
    mutate(FTE_change = if_else(Staff_group_y2 == staff_group_name & Staff_group_y1 == staff_group_name & Status_y1 == "Active" & Status_y2 == "Active" & NHSD_trust_or_CCG_y1 == "1" & NHSD_trust_or_CCG_y2 == "1", Contracted_Wte_y2-Contracted_Wte_y1, 0))
  
  #override NAs in joiner/ leaver flags
  Data <- Data %>%
    mutate (nhs_provider_joiner = if_else(is.na(nhs_provider_joiner)==FALSE,nhs_provider_joiner,0)) %>%
    mutate (other_joiner = if_else(is.na(other_joiner)==FALSE,other_joiner,0)) %>%
    mutate (nhs_provider_leaver = if_else(is.na(nhs_provider_leaver)==FALSE,nhs_provider_leaver,0)) %>%
    mutate (other_leaver = if_else(is.na(other_leaver)==FALSE,other_leaver,0))
  
  #duplication check
  Data$Unique_Nhs_Identifier[duplicated(Data$Unique_Nhs_Identifier)]
  
  #create new organisation code taking the latest one first, if y2 missing, take y1
  Data <- Data %>%
    mutate (Ocs_Code = if_else(is.na(Ocs_Code_y2)==TRUE, Ocs_Code_y1, Ocs_Code_y2))
  
  #Join datasets to overwrite Y1 nationality
  Raw_Data_y1 <- left_join(Raw_Data_y1, Raw_Data_y2_dedup, by = "Unique_Nhs_Identifier")
  
  #get rid of unsused data
  rm(Raw_Data_y2_dedup)
  
  #overwrite Y1 nationality
  Raw_Data_y1<- Raw_Data_y1 %>%
    mutate (Nationality_grouping_y1_v2_2=if_else(is.na(Nationality_grouping_y2_v2)==FALSE,Nationality_grouping_y2_v2,Nationality_grouping_y1_v2)) %>%
    mutate (Nationality_grouping_y1_v2=if_else(is.na(Nationality_grouping_y2)==FALSE,Nationality_grouping_y2,Nationality_grouping_y1))
  
  ###################################joiner/ leaver summaries###################################
  #Total joiners/ leavers
  summary <- Data %>%
    group_by(Ocs_Code) %>%

    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  #insert nationality column
  summary <- summary %>% mutate (Nationality_grouping="All") %>% select (13,1:12)
  
  #Split by nationality
  summary_nat <- Data %>%
    group_by(Ocs_Code, Nationality_grouping) %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  summary_nat_group <- Data %>%
    group_by(Ocs_Code, Nationality_grouping_v2) %>%
    summarise (joiner=sum(joiner),
               occ_joiner=sum(occ_joiner),
               non_active_to_active=sum(non_active_to_active),
               nhs_provider_joiner=sum(nhs_provider_joiner),
               other_joiner=sum(other_joiner),
               leaver=sum(leaver),
               occ_leaver=sum(occ_leaver),
               active_to_non_active=sum(active_to_non_active),
               nhs_provider_leaver=sum(nhs_provider_leaver),
               other_leaver=sum(other_leaver),
               FTE_change=sum(FTE_change)
    )
  
  #rename nationality grouping to match above summaries
  summary_nat_group <- rename (summary_nat_group, Nationality_grouping=Nationality_grouping_v2)
  
  #combine total with nat split
  summary <- bind_rows(summary_nat,summary,summary_nat_group) 
  
  #remove helper tables
  rm(summary_nat,summary_nat_group)
  
  
  ###################################FTE summaries###################################
  #FTE - year 1
  #total
  FTE_y1_1 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% staff_group_name & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_1 <- FTE_y1_1 %>% mutate (Nationality_grouping='All')
  
  #nationality split 1
  FTE_y1_2 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% staff_group_name & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    mutate (Nationality_grouping_y1 = if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
    group_by(Ocs_Code_y1, Nationality_grouping_y1_v2) %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_2 <- rename (FTE_y1_2, Nationality_grouping=Nationality_grouping_y1_v2)
  
  #nationality split 2
  FTE_y1_3 <- Raw_Data_y1 %>%
    filter(Staff_group_y1 %in% staff_group_name & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
    group_by(Ocs_Code_y1, Nationality_grouping_y1_v2_2) %>%
    summarise (FTE_y1 = sum(Contracted_Wte_y1))
  
  FTE_y1_3 <- rename (FTE_y1_3, Nationality_grouping=Nationality_grouping_y1_v2_2)
  
  #combine all FTE
  FTE_y1 <- bind_rows(FTE_y1_2,FTE_y1_1,FTE_y1_3) 
  FTE_y1 <- rename (FTE_y1, Ocs_Code=Ocs_Code_y1)
  
  #FTE - year 2
  #total
  FTE_y2_1 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_1 <- FTE_y2_1 %>% mutate (Nationality_grouping='All')
  
  #nationality split 1
  FTE_y2_2 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    mutate (Nationality_grouping_y2 = if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
    group_by(Ocs_Code_y2, Nationality_grouping_y2) %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_2 <- rename (FTE_y2_2, Nationality_grouping=Nationality_grouping_y2)
  
  #nationality split 2
  FTE_y2_3 <- Raw_Data_y2 %>%
    filter(Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
    group_by(Ocs_Code_y2, Nationality_grouping_y2_v2) %>%
    summarise (FTE_y2 = sum(Contracted_Wte_y2))
  
  FTE_y2_3 <- rename (FTE_y2_3, Nationality_grouping=Nationality_grouping_y2_v2)
  
  #combine all FTE
  FTE_y2 <- bind_rows(FTE_y2_2,FTE_y2_1,FTE_y2_3) 
  FTE_y2 <- rename (FTE_y2, Ocs_Code=Ocs_Code_y2)
  
  FTE <- full_join(FTE_y1,FTE_y2)
  
  ###################################Final combined summary output###################################
  #combine joiners/ leavers with FTE
  summary <- full_join(summary,FTE)
  
  #remove helper tables
  rm(FTE_y1_1,FTE_y1_2,FTE_y1_3,FTE_y2_1,FTE_y2_2,FTE_y2_3,FTE_y1,FTE_y2)
  
  #leaver rates
  summary <- summary %>%
    mutate(leaver_rate = as.numeric(leaver)/as.numeric(FTE_y1)) %>%
    mutate(leaver_rate_occ = (as.numeric(occ_leaver))/as.numeric(FTE_y1))
  
  #pivot data into long format
  pivot <- pivot_longer(summary, c(3:17))
  
  #######Pull joiner/ leaver period name#######
  #extract joiner/ leaver period name
  pivot_final <- pivot
  colnames(pivot_final) <- c("Ocs_Code","Nationality_grouping", "name", paste(substr(Data$Tm_Year_Month_y1,1,8)[1],"to",substr(Data$Tm_Year_Month_y2,1,8)[1]))
  

  
  return(pivot_final)
}

