
#Staff Filter ------------------------------------------------------------------

#' Staff Filter
#'
#' This function cleans the input data by filtering for the required staff type and performs additional actions depending on the summarization variable.
#' @param raw_data A data frame containing the raw staff data.
#' @param suffix The year suffix to add. If identifying joiners and leavers, use '_y1' or '_y2'.
#' @param NHS_orgs_data A data frame containing all current trust organization codes, used to check if the staff member is working inside or outside of trusts.
#' @param nationality_data A data frame containing the area in which each nationality is located. For example, India will be returned as Overseas.
#' @param staff_group_code A vector of staff codes to filter for. Possible values are nurse_staff_codes or midwife_staff_codes.
#' @param staff_group_name A character string for the staff group, referenced in later functions. Possible values are 'Nurse', 'Midwife', 'Medical', etc.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'. Default is 'Nationality_grouping'.
#' @param headcount Logical. Default is FALSE. If TRUE, will convert the FTE of the data frame to 1 if above 0 to get a headcount rather than Full Time Equivalent.
#' @param stock Logical. Default is FALSE. If TRUE, gets a snapshot of stock for a time period and does not add the suffix.
#'
#' @return A filtered data frame based on user input. Will append a suffix if identifying joiners and leavers.
#' 
#' @details
#' The function performs the following operations:
#' - Checks if a valid suffix is being used, only accepting '_y1' or '_y2' as it will impact later functions.
#' - Replaces spaces in column names with '_' and joins the main dataset with NHS_orgs_data and nationality_data.
#' - Creates a new column of Staff_group based on the provided staff_group_code. Filters to only the active staff.
#' - Depending on the value of \code{summarization_variable}, performs additional transformations:
#'   - If 'age' or 'age_group', calculates age based on the date of birth column and the current month (Tm_Month_Year).
#'   - If 'age_group', creates a new column based on age groups.
#'   - If 'region', creates a new column by grouped regions.
#'   - If 'trust', creates a new column by the grouped trusts.
#'   - If 'Ocs_Code', fills missing values in the 'Ocs_Code' column.
#' - Adds a suffix to each column name if required.
#' 
#' @examples
#' \dontrun{
#' data_cleaned <- staff_filter(raw_data, suffix = '_y1', NHS_orgs_data = NHS_orgs, nationality_data = nationality, 
#'                              staff_group_code = nurse_staff_codes, staff_group_name = 'Nurse', 
#'                              summarization_variable = 'age', headcount = FALSE, stock = FALSE)
#' }
#'
#' @import dplyr
#' @export
staff_filter <- function(raw_data,
                         suffix = 'none',
                         NHS_orgs_data = NHS_orgs,
                         nationality_data = nationality,
                         staff_group_code,
                         staff_group_name,
                         summarization_variable = 'Nationality_grouping',
                         headcount = FALSE,
                         stock = FALSE) {
  print('Starting staff filter')
  
  # Check if suffix is 'none' or a valid suffix
  if (stock == FALSE && suffix != "none" && !(suffix %in% c('_y1' , '_y2'))) {
    stop("Invalid suffix. Please use '_y1' or '_y2'.")
  }  
  
  # Change loose spaces in date to be replaced with underscore, easier for manip
  colnames(raw_data) <- str_replace_all(colnames(raw_data), " ", "_")
  colnames(nationality_data) <- str_replace_all(colnames(nationality_data), " ", "_")
  
  #Join datasets together
  raw_data <- full_join(raw_data,NHS_orgs_data)
  raw_data <- full_join(raw_data,nationality_data)
  
  raw_data <- raw_data %>% 
    mutate(Staff_group = if_else(Occupation_Code %in% staff_group_code, staff_group_name, 'x.Other'))%>%
    mutate(Status_orig = Status) %>%
    mutate(Status = if_else(Status %in% c("Active Assignment", "Internal Secondment", "Acting Up"),"Active","Not active"))%>%
    mutate(Asg_Type_Of_Contract = if_else(Asg_Type_Of_Contract %in% c("Locum", "Fixed Term Temp", "Permanent"),"Permanent/fixed term/locum","Other")) %>%
    filter (Asg_Type_Of_Contract=='Permanent/fixed term/locum') %>%
    mutate (Nationality_grouping =if_else(is.na(Nationality_grouping) == FALSE, Nationality_grouping, 'Unknown')) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                              if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other')))
  
  if (summarization_variable %in% c('age', 'age_group')){
    # Convert 'Date' and 'Date_Of_Birth' columns to Date objects if they are not already
    raw_data <- raw_data %>%
      mutate(Date = as.Date(paste0(Tm_Year_Month,"-01"), tryFormats = c("%Y-%B-%d")))%>%
      mutate(Date_Of_Birth = as.Date(Date_Of_Birth, tryFormats = c("%Y-%B-%d")))
    
    # Calculate the age difference in days
    age_difference_days <- difftime(raw_data$Date, raw_data$Date_Of_Birth, units = "days")
    
    # Convert age difference from days to years
    age_years <- as.numeric(age_difference_days) / 365.25
    
    # Assign the age in years to the 'Age' column
    raw_data$age <- round(age_years)
    
    if (summarization_variable == 'age_group') {
      raw_data <- raw_data %>% mutate(age_group = case_when(is.na(age)~ 'Unknown',
                                                            age < 25 ~ 'Under 25',
                                                            age < 35 ~ '25 to 34',
                                                            age < 45 ~ '35 to 44',
                                                            age < 55 ~ '45 to 54',
                                                            age < 65 ~ '55 to 64',
                                                            TRUE ~ 'Over 65'))
      
    }
  }
  if (summarization_variable == 'region' & stock == FALSE){
    source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/org_region_codes.R')
    
    raw_data <- raw_data %>%
      mutate(region = case_when(is.na(Ocs_Code) ~ "Unknown",
                                Ocs_Code %in% orgs_region_east_england ~ "East England",
                                Ocs_Code %in% orgs_region_london ~ "London",
                                Ocs_Code %in% orgs_region_midlands ~ "Midlands",
                                Ocs_Code %in% orgs_region_north_east_and_yorkshire ~ "North East and Yorkshire",
                                Ocs_Code %in% orgs_region_north_west ~ "North West",
                                Ocs_Code %in% orgs_region_south_west ~ "South West",
                                Ocs_Code %in% orgs_region_southeast ~ "South East",
                                TRUE ~ 'NA'))
  }
  
  if (summarization_variable == 'trust' & stock == FALSE) {
    source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/org_trust_codes.R')
    
    raw_data <- raw_data %>%
      mutate(trust = case_when(is.na(Ocs_Code) ~ 'Unknown',
                                       Ocs_Code %in% orgs_bath_somerset_swindon_and_wiltshire ~ 'Bath, Somerset, Swindon and Wiltshire',
                                       Ocs_Code %in% orgs_bedfordshire_luton_and_milton_keynes ~ 'Bedfordshire, Luton and Milton Keynes',
                                       Ocs_Code %in% orgs_birmingham ~ 'Birmingham',
                                       Ocs_Code %in% orgs_black_country ~ 'Black Country',
                                       Ocs_Code %in% orgs_bristol_north_somerset_and_south_gloucestershire ~ 'Bristol, North Somerset and South Gloucestershire',
                                       Ocs_Code %in% orgs_buckinghamshire_oxfordshire_and_berkshire_west ~ 'Buckinghamshire, Oxfordshire and Berkshire West',
                                       Ocs_Code %in% orgs_cambridgeshire_and_peterborough ~ 'Cambridgeshire and Peterborough',
                                       Ocs_Code %in% orgs_cheshire_and_merseyside ~ 'Cheshire and Merseyside',
                                       Ocs_Code %in% orgs_coventry_and_warwickshire ~ 'Coventry and Warwickshre',
                                       Ocs_Code %in% orgs_derbyshire ~ 'Derbyshire',
                                       Ocs_Code %in% orgs_devon ~ 'Devon',
                                       Ocs_Code %in% orgs_dorset ~ 'Dorset',
                                       Ocs_Code %in% orgs_frimley ~ 'Frimley',
                                       Ocs_Code %in% orgs_gloucestershire ~ 'Gloucestershire',
                                       Ocs_Code %in% orgs_greater_manchester ~ 'Greater Manchester',
                                       Ocs_Code %in% orgs_hampshire_and_isle_of_wight ~ 'Hampshire and the Isle of Wight',
                                       Ocs_Code %in% orgs_herefordshire_and_worcestershire ~ 'Herefordshire and Worcestershire',
                                       Ocs_Code %in% orgs_hertfordshire_and_west_essex ~ 'Hertfordshire and West Essex',
                                       Ocs_Code %in% orgs_humber_and_north_yorkshire ~ 'Humber and North Yorkshire',
                                       Ocs_Code %in% orgs_kent_and_medway ~ 'Kent and Medway',
                                       Ocs_Code %in% orgs_lancashire_and_south_cumbria ~ 'Lancashire and South Cumbria',
                                       Ocs_Code %in% orgs_leicestershire_and_rutland ~ 'Leicestershire and Rutland',
                                       Ocs_Code %in% orgs_lincolnshire ~ 'Lincolnshire',
                                       Ocs_Code %in% orgs_mid_and_south_essex ~ 'Mid and South Essex',
                                       Ocs_Code %in% orgs_norfolk_and_waveney ~ 'Norfolk and Waveney',
                                       Ocs_Code %in% orgs_north_central_london ~ 'North Central London',
                                       Ocs_Code %in% orgs_north_east_and_north_cumbria ~ 'North East and North Cumbria',
                                       Ocs_Code %in% orgs_north_east_london ~ 'North East London',
                                       Ocs_Code %in% orgs_north_west_london ~ 'North West London',
                                       Ocs_Code %in% orgs_northamptonshire ~ 'Northamptonshire',
                                       Ocs_Code %in% orgs_nottinghamshire ~ 'Nottinghamshire',
                                       Ocs_Code %in% orgs_shropshire_telford_and_wrekin ~ 'Shropshire, Telford and Wrekin',
                                       Ocs_Code %in% orgs_somerset ~ 'Somerset',
                                       Ocs_Code %in% orgs_south_east_london ~ 'South East London',
                                       Ocs_Code %in% orgs_south_west_london ~ 'South West London',
                                       Ocs_Code %in% orgs_south_yorkshire ~ 'South Yorkshire',
                                       Ocs_Code %in% orgs_staffordshire_and_stoke_on_trent ~ 'Staffordshire and Stoke on Trent',
                                       Ocs_Code %in% orgs_suffolk_and_north_east_essex ~ 'Suffolk and North East Essex',
                                       Ocs_Code %in% orgs_surrey_heartlands ~ 'Surrey Heartlands',
                                       Ocs_Code %in% orgs_sussex ~ 'Sussex',
                                       Ocs_Code %in% orgs_west_yorkshire ~ 'West Yorkshire',
                                       TRUE ~ 'NA'))
  }
  
  
  if (headcount == TRUE) {
    raw_data <- raw_data %>%
      mutate(Contracted_Wte = if_else(Contracted_Wte > 0, 1, Contracted_Wte))
  }
  if (stock == TRUE) {
    return(raw_data)
  }
  
  raw_data <- raw_data %>% rename_with(~ paste0(., suffix))
  
  
  return(raw_data)
}




# Clean joined data ------------------------------------------------------------

#' Clean Joined Data
#'
#' This function cleans the joined data by filling in missing values, combining nationality information, and performing other transformations based on a summarization variable.
#'
#' @param data A data frame containing the joined data.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'. Default is 'Nationality_grouping'.
#'
#' @return A cleaned data frame with transformed variables.
#' 
#' @details
#' The function performs the following operations:
#' - Fills and combines nationality information.
#' - Creates a new variable 'Nationality_grouping_v2' based on 'Nationality_grouping'.
#' - Fills missing values for 'NHSD_trust_or_CCG_y1' and 'NHSD_trust_or_CCG_y2'.
#' - Depending on the value of \code{summarization_variable}, performs additional transformations:
#'   - If 'age', assigns the age in years to the 'age' column and removes unnecessary columns.
#'   - If 'age_group', assigns the age group and removes unnecessary columns.
#'   - If 'region', fills missing values in the 'region' column.
#'   - If 'trust', fills missing values in the 'trust' column.
#'   - If 'Ocs_Code', fills missing values in the 'Ocs_Code' column.
#'
#' @examples
#' \dontrun{
#' data_cleaned <- clean_joined_data(data, summarization_variable = 'age')
#' }
#'
#' @import dplyr
#' @export

clean_joined_data <- function(data, summarization_variable = 'Nationality_grouping') {
  print('Cleaning joined data')
  
  #Fill and combine nationality
  data <- data %>%
    mutate (Nationality = if_else(is.na(Nationality_y2) == FALSE, Nationality_y2, Nationality_y1)) %>%
    mutate (Nationality_grouping =if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, Nationality_grouping_y1)) %>%
    mutate (Nationality_grouping = if_else(is.na(Nationality_grouping) == TRUE, 'Unknown',Nationality_grouping)) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('ROW','EU'), 'IR',
                                              if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic','Other'))) %>%
    mutate (NHSD_trust_or_CCG_y1 = if_else(is.na(NHSD_trust_or_CCG_y1) == FALSE, NHSD_trust_or_CCG_y1,0)) %>% 
    mutate (NHSD_trust_or_CCG_y2 = if_else(is.na(NHSD_trust_or_CCG_y2) == FALSE, NHSD_trust_or_CCG_y2,0))
  
  if (summarization_variable == "age") {
    #Assign the age in years to the Age column
    data <- data %>% mutate(age = if_else(is.na(age_y1) == TRUE, age_y2, age_y1))%>%
      select(-c(Date_y1, Date_y2, age_y1, age_y2))
  }
  
  if (summarization_variable == "age_group") {
    #Assign the age in years to the Age column
    data <- data %>% mutate(age_group = if_else(is.na(age_group_y1) == TRUE, age_group_y2, age_group_y1))%>%
      select(-c(Date_y1, Date_y2, age_y1, age_y2, age_group_y1, age_group_y2))
  }
  
  if (summarization_variable == 'region'){
    #override NA's in region data, assigning joining region, leaving region and non movers
    data <- data %>% mutate(region = if_else(is.na(region_y2) == TRUE, region_y1, region_y2))
  }
  
  if (summarization_variable == 'trust'){
    data <- data %>% mutate(trust = if_else(is.na(trust_y2) == TRUE, trust_y1, trust_y2))
    
  }
  
  if (summarization_variable == 'Ocs_Code'){
    data <- data %>% mutate(Ocs_Code = if_else(is.na(Ocs_Code_y2) == TRUE, Ocs_Code_y1, Ocs_Code_y2))
    
  }
  
  return(data)
}





# Joiner and leaver flags ------------------------------------------------------

#' Joiner and Leaver Flags
#'
#' This function creates flags for joiners and leavers in the provided dataset. It identifies various categories of joiners and leavers based on the status and type of contract.
#' 
#' @param data A data frame that contains staff data with required columns such as Staff_group_y1, Staff_group_y2, Status_y1, Status_y2, NHSD_trust_or_CCG_y1, NHSD_trust_or_CCG_y2, and Contracted_Wte_y1, Contracted_Wte_y2.
#' @param staff_group_name A character string indicating the staff group name to filter for. Possible values are 'Nurse', 'Midwife', 'Medical', etc.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'. Default is 'Nationality_grouping'.
#'
#' @return A data frame with additional columns for joiner and leaver flags, and FTE changes.
#'
#' @details
#' The function performs the following operations:
#' - Creates flags for various types of joiners and leavers based on the conditions of staff group, status, and organization type.
#' - Calculates FTE changes for staff who remained active within the same staff group and organization type.
#' - Overrides NA values in joiner and leaver flags with 0.
#'
#' The joiner flags include:
#' - \code{joiner}: Staff who were not present in y1 but are active in y2 within the specified staff group and organization type.
#' - \code{occ_joiner}: Staff who changed occupation to the specified staff group between y1 and y2.
#' - \code{non_active_to_active}: Staff who became active in y2 within the specified staff group.
#' - \code{nhs_provider_joiner}: Staff who joined from non-NHS provider to NHS provider within the specified staff group.
#' - \code{other_joiner}: Staff who joined from non-specified staff group and non-NHS provider to the specified staff group and NHS provider.
#'
#' The leaver flags include:
#' - \code{leaver}: Staff who were present in y1 but are not in y2 within the specified staff group and organization type.
#' - \code{occ_leaver}: Staff who changed occupation from the specified staff group between y1 and y2.
#' - \code{active_to_non_active}: Staff who became non-active in y2 within the specified staff group.
#' - \code{nhs_provider_leaver}: Staff who left from NHS provider to non-NHS provider within the specified staff group.
#' - \code{other_leaver}: Staff who left from the specified staff group and NHS provider to non-specified staff group and non-NHS provider.
#'
#' The function also calculates the FTE change (\code{FTE_change}) for staff who remained in the same staff group and organization type, and stayed active between y1 and y2.
#'
#' @examples
#' \dontrun{
#' data_with_flags <- joiner_leaver_flags(data, 'Nurse', summarization_variable = 'age')
#' }
#'
#' @import dplyr
#' @export
joiner_leaver_flags <- function(data,
                                staff_group_name,
                                summarization_variable = 'Nationality_grouping') {
  print('Creating joiner leaver flags')
  #joiner/ leaver flags
  data <- data %>%
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
  data <- data %>%
    mutate (nhs_provider_joiner = if_else(is.na(nhs_provider_joiner)==FALSE,nhs_provider_joiner,0)) %>%
    mutate (other_joiner = if_else(is.na(other_joiner)==FALSE,other_joiner,0)) %>%
    mutate (nhs_provider_leaver = if_else(is.na(nhs_provider_leaver)==FALSE,nhs_provider_leaver,0)) %>%
    mutate (other_leaver = if_else(is.na(other_leaver)==FALSE,other_leaver,0))
  
  return(data)
}






#Function for overwriting Nationality to correct it in both time frames --------

#' Overwrite Y1 Nationality
#'
#' This function overwrites the nationality grouping columns in year 1 with the values from year 2 if they are not missing.
#'
#' @param data A data frame containing the columns \code{Nationality_grouping_v2_y1}, \code{Nationality_grouping_v2_y2}, \code{Nationality_grouping_y1}, and \code{Nationality_grouping_y2}.
#'
#' @return A data frame with updated \code{Nationality_grouping_v2_y1_2} and \code{Nationality_grouping_v2_y1} columns.
#'
#' @details
#' The function performs the following operations:
#' - Creates a new column \code{Nationality_grouping_v2_y1_2} which takes the value of \code{Nationality_grouping_v2_y2} if it is not missing; otherwise, it retains the value from \code{Nationality_grouping_v2_y1}.
#' - Updates the existing column \code{Nationality_grouping_v2_y1} with the value of \code{Nationality_grouping_y2} if it is not missing; otherwise, it retains the value from \code{Nationality_grouping_y1}.
#'
#' @examples
#' \dontrun{
#' updated_data <- overwrite_y1_nationality(data)
#' }
#'
#' @import dplyr
#' @export

overwrite_y1_nationality <- function(data) {
  print('Overwriting nationality')
  data$Nationality_grouping_v2_y1_2 <- ifelse(is.na(data$Nationality_grouping_v2_y2), 
                                              data$Nationality_grouping_v2_y1, 
                                              data$Nationality_grouping_v2_y2)
  
  data$Nationality_grouping_v2_y1 <- ifelse(is.na(data$Nationality_grouping_y2), 
                                            data$Nationality_grouping_y1, 
                                            data$Nationality_grouping_y2)
  
  return(data)
}





# Function for creating summaries based on 'summarization_variable' argument -------------

#' Create Joiner Leaver Summary
#'
#' This function generates summaries based on the specified summarization variable, aggregating joiner and leaver data.
#'
#' @param data A data frame containing joiner and leaver flags along with the summarization variable.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#'
#' @return A summarized data frame with joiner and leaver statistics aggregated based on the specified summarization variable.
#'
#' @details
#' The function performs the following operations:
#' - Calculates total joiners, leavers, and FTE changes across all categories.
#' - Splits the data by the summarization variable and calculates joiners, leavers, and FTE changes for each category.
#' - If the summarization variable is 'Nationality_grouping', it further generates summaries for nationality smaller groups.
#' - Converts the 'age' variable to character type if the summarization variable is 'age'.
#'
#' @examples
#' \dontrun{
#' summary_data <- create_joiner_leaver_summary(data, summarization_variable = 'Nationality_grouping')
#' }
#'
#' @import dplyr
#' @export

create_joiner_leaver_summary <- function(data, summarization_variable) {
  print('Creating summary')
  # Total summaries
  #Total joiners/ leavers
  summary <- data %>%
    summarise (
      joiner = sum(joiner),
      occ_joiner = sum(occ_joiner),
      non_active_to_active = sum(non_active_to_active),
      nhs_provider_joiner = sum(nhs_provider_joiner),
      other_joiner = sum(other_joiner),
      leaver = sum(leaver),
      occ_leaver = sum(occ_leaver),
      active_to_non_active = sum(active_to_non_active),
      nhs_provider_leaver = sum(nhs_provider_leaver),
      other_leaver = sum(other_leaver),
      FTE_change = sum(FTE_change)
    )
  
  # Insert summarization_variable column
  summary <- summary %>%
    mutate(!!sym(summarization_variable) := 'All')
  
  # Split by summarization_variable
  summary_group <- data %>%
    group_by_at(vars(summarization_variable)) %>%
    summarise (
      joiner = sum(joiner),
      occ_joiner = sum(occ_joiner),
      non_active_to_active = sum(non_active_to_active),
      nhs_provider_joiner = sum(nhs_provider_joiner),
      other_joiner = sum(other_joiner),
      leaver = sum(leaver),
      occ_leaver = sum(occ_leaver),
      active_to_non_active = sum(active_to_non_active),
      nhs_provider_leaver = sum(nhs_provider_leaver),
      other_leaver = sum(other_leaver),
      FTE_change = sum(FTE_change)
    )
  
  if (summarization_variable == 'Nationality_grouping') {
    # Summary for nationality smaller groups
    summary_nat_group <- data %>%
      group_by(Nationality_grouping_v2) %>%
      summarise (
        joiner = sum(joiner),
        occ_joiner = sum(occ_joiner),
        non_active_to_active = sum(non_active_to_active),
        nhs_provider_joiner = sum(nhs_provider_joiner),
        other_joiner = sum(other_joiner),
        leaver = sum(leaver),
        occ_leaver = sum(occ_leaver),
        active_to_non_active = sum(active_to_non_active),
        nhs_provider_leaver = sum(nhs_provider_leaver),
        other_leaver = sum(other_leaver),
        FTE_change = sum(FTE_change)
      ) %>%
      rename(Nationality_grouping = Nationality_grouping_v2)
    
    summary <- bind_rows(summary_group, summary, summary_nat_group)
    return(summary)
  } 
  
  
  if (summarization_variable == 'age') {
    summary$age <- as.character(summary$age)
    summary_group$age <- as.character(summary_group$age)
  }
  

  summary <- bind_rows(summary_group, summary)
  
  return(summary)
}








# Function for the FTE of y1 ---------------------------------------------------
#' FTE of Year 1
#'
#' This function calculates the Full-Time Equivalent (FTE) for Year 1 based on the specified summarization variable.
#'
#' @param data A data frame containing the relevant data.
#' @param staff_group_name A character string indicating the staff group name.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#'
#' @return A data frame with FTE values for Year 1, categorized by the specified summarization variable.
#'
#' @details
#' The function performs the following operations:
#' - If the summarization variable is 'Nationality_grouping', it generates three sets of summaries:
#'   - Total FTE for Year 1 for all nationalities.
#'   - FTE by refined nationality grouping for Year 1.
#'   - FTE by nationality groups for Year 1.
#' - If the summarization variable is not 'Nationality_grouping', it calculates the total FTE for Year 1 for each category.
#' - If the summarization variable is 'age', it renames the summarization variable to 'age' from 'age_y1'.
#'
#' @examples
#' \dontrun{
#' fte_y1 <- fte_y1_function(data, staff_group_name = 'example_staff_group', summarization_variable = 'Nationality_grouping')
#' }
#'
#' @import dplyr
#' @export


fte_y1_function <- function(data, staff_group_name, summarization_variable){
  print('Getting FTE')
  #FTE - year 1
  #total
  if (summarization_variable == 'Nationality_grouping'){
    FTE_y1_1 <- data %>%
      filter(Staff_group_y1 %in% staff_group_name & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
      summarise (FTE_y1 = sum(Contracted_Wte_y1))
    
    FTE_y1_1 <- FTE_y1_1 %>% mutate (Nationality_grouping='All')
    
    #nationality split 1
    FTE_y1_2 <- data %>%
      filter(Staff_group_y1 %in% c(staff_group_name) & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
      mutate (Nationality_grouping_y1 = if_else(is.na(Nationality_grouping_y1) == FALSE, Nationality_grouping_y1, 'Unknown')) %>%
      group_by(Nationality_grouping_v2_y1) %>%
      summarise (FTE_y1 = sum(Contracted_Wte_y1))
    
    FTE_y1_2 <- rename (FTE_y1_2, Nationality_grouping=Nationality_grouping_v2_y1)
    
    #nationality split 2
    FTE_y1_3 <- data %>%
      filter(Staff_group_y1 %in% c(staff_group_name) & Status_y1 %in% c("Active") & NHSD_trust_or_CCG_y1 == "1") %>%
      group_by(Nationality_grouping_v2_y1_2) %>%
      summarise (FTE_y1 = sum(Contracted_Wte_y1))
    
    FTE_y1_3 <- rename (FTE_y1_3, Nationality_grouping=Nationality_grouping_v2_y1_2)
    
    #combine all FTE
    FTE_y1 <- bind_rows(FTE_y1_2,FTE_y1_1,FTE_y1_3)
    
    return(FTE_y1)
  } else {
    summarization_variable <- paste0(summarization_variable, "_y1")
    
    # Total
    FTE_y1_1 <- data %>%
      filter(Staff_group_y1 == staff_group_name, Status_y1 == "Active", NHSD_trust_or_CCG_y1 == "1") %>%
      summarise(FTE_y1 = sum(Contracted_Wte_y1)) %>%
      mutate(!!summarization_variable := 'All')
    
    # Summary split 1
    FTE_y1_2 <- data %>%
      filter(Staff_group_y1 == staff_group_name, Status_y1 == "Active", NHSD_trust_or_CCG_y1 == "1") %>%
      group_by(!!sym(summarization_variable)) %>%
      summarise(FTE_y1 = sum(Contracted_Wte_y1))
    
    # Combine all FTE
    FTE_y1 <- rbind(FTE_y1_2, FTE_y1_1)
    
    summarization_variable <- sub("_y1$", "", summarization_variable)
    
    colnames(FTE_y1) <- c(summarization_variable, 'FTE_y1')
    
    return(FTE_y1)
  }
}





# Function for the FTE of Y2 ---------------------------------------------------

#' FTE of Year 2
#'
#' This function calculates the Full-Time Equivalent (FTE) for Year 2 based on the specified summarization variable.
#'
#' @param data A data frame containing the relevant data.
#' @param staff_group_name A character string indicating the staff group name.
#' @param summarization_variable A character string indicating the variable used for summarization. 
#' Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#'
#' @return A data frame with FTE values for Year 2, categorized by the specified summarization variable.
#'
#' @details
#' The function performs the following operations:
#' - If the summarization variable is 'Nationality_grouping', it generates three sets of summaries:
#'   - Total FTE for Year 2 for all nationalities.
#'   - FTE by refined nationality grouping for Year 2.
#'   - FTE by nationality groups for Year 2.
#' - If the summarization variable is not 'Nationality_grouping', it calculates the total FTE for Year 2 for each category.
#' - If the summarization variable is 'age', it renames the summarization variable to 'age' from 'age_y2'.
#'
#' @examples
#' \dontrun{
#' fte_y2 <- fte_y2_function(data, staff_group_name = 'example_staff_group', summarization_variable = 'Nationality_grouping')
#' }
#'
#' @import dplyr
#' @export

fte_y2_function <- function(data, staff_group_name, summarization_variable){
  print('Getting FTE')
  if (summarization_variable == 'Nationality_grouping'){
    #FTE - year 2
    #total
    FTE_y2_1 <- data %>%
      filter(Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
      summarise (FTE_y2 = sum(Contracted_Wte_y2))
    
    FTE_y2_1 <- FTE_y2_1 %>% mutate (Nationality_grouping='All')
    
    #nationality split 1
    FTE_y2_2 <- data %>%
      filter(Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
      mutate (Nationality_grouping_y2 = if_else(is.na(Nationality_grouping_y2) == FALSE, Nationality_grouping_y2, 'Unknown')) %>%
      group_by(Nationality_grouping_y2) %>%
      summarise (FTE_y2 = sum(Contracted_Wte_y2))
    
    FTE_y2_2 <- rename (FTE_y2_2, Nationality_grouping=Nationality_grouping_y2)
    
    #nationality split 2
    FTE_y2_3 <- data %>%
      filter(Staff_group_y2 %in% staff_group_name & Status_y2 %in% c("Active") & NHSD_trust_or_CCG_y2 == "1") %>%
      group_by(Nationality_grouping_v2_y2) %>%
      summarise (FTE_y2 = sum(Contracted_Wte_y2))
    
    FTE_y2_3 <- rename (FTE_y2_3, Nationality_grouping=Nationality_grouping_v2_y2)
    
    #combine all FTE
    FTE_y2 <- bind_rows(FTE_y2_2,FTE_y2_1,FTE_y2_3) 
    
    return(FTE_y2)
    
  } else {
    summarization_variable <- paste0(summarization_variable, "_y2")
    
    # Total
    FTE_y2_1 <- data %>%
      filter(Staff_group_y2 == staff_group_name, Status_y2 == "Active", NHSD_trust_or_CCG_y2 == "1") %>%
      summarise(FTE_y2 = sum(Contracted_Wte_y2)) %>%
      mutate(!!summarization_variable := 'All')
    
    # Summary split 1
    FTE_y2_2 <- data %>%
      filter(Staff_group_y2 == staff_group_name, Status_y2 == "Active", NHSD_trust_or_CCG_y2 == "1") %>%
      group_by(!!sym(summarization_variable)) %>%
      summarise(FTE_y2 = sum(Contracted_Wte_y2))
    
    # Combine all FTE
    FTE_y2 <- rbind(FTE_y2_2, FTE_y2_1)
    
    summarization_variable <- sub("_y2$", "", summarization_variable)
    
    colnames(FTE_y2) <- c(summarization_variable, 'FTE_y2')
    
    return(FTE_y2)
  }
}

# Function for combining summary groups ----------------------------------------

#' Combine Summary Groups
#'
#' This function combines summary groups and calculates leaver rates and occupational leaver rates.
#'
#' @param summary A data frame containing the summary data.
#' @param fte_y1 A data frame containing the Full-Time Equivalent (FTE) values for Year 1.
#' @param fte_y2 A data frame containing the FTE values for Year 2.
#' @param summarization_variable A character string indicating the variable used for summarization. 
#' Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#' @param data A data frame containing the relevant data.
#'
#' @return A melted data frame with combined summary groups, including leaver rates and occupational leaver rates.
#'
#' @details
#' The function performs the following operations:
#' - Merges FTE values for Year 1 and Year 2 into the summary data frame based on the specified summarization variable.
#' - Calculates leaver rates and occupational leaver rates.
#' - Melts the summary data frame to long format for easy visualization.
#' - Extracts year-month strings from the data frame.
#' - Renames columns of the pivot data frame.
#'
#' @examples
#' \dontrun{
#' combined_summary <- summary_fte_combine(summary, fte_y1, fte_y2, summarization_variable = 'Nationality_grouping', data)
#' }
#'
#' @import dplyr
#' @import tidyr
#' @export

summary_fte_combine <- function(summary, fte_y1, fte_y2, summarization_variable, data) {
  print('Combining summary')
  # Merge fte_y1 into summary
  summary <- merge(summary, fte_y1, by = summarization_variable)
  
  # Merge fte_y2 into summary
  summary <- merge(summary, fte_y2, by = summarization_variable)
  
  # Calculate leaver_rate and occ_leaver_rate
  summary$leaver_rate <- summary$leaver / summary$FTE_y1
  summary$occ_leaver_rate <- summary$occ_leaver / summary$FTE_y1
  
  # Melt the summary data frame
  pivot <- pivot_longer(summary, cols = -c(summarization_variable))
  
  # Extract year-month strings from data
  date_y1 <- substr(data$Tm_Year_Month_y1, 1, 8)
  date_y2 <- substr(data$Tm_Year_Month_y2, 1, 8)
  
  # Define new column names
  new_colnames <- c(summarization_variable, "name", paste(date_y1, "to", date_y2))
  
  # Rename columns of the pivot data frame
  colnames(pivot) <- new_colnames
  
  return(pivot)
}


