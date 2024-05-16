# Script containing the majority of functions used

library(tidyverse)
source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables.R')


# Import Nationality and Org documents to be used in main analysis

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")
nationality <- read_csv("Nationality groupings.csv")
NHS_orgs <- read_csv("Org Codes NHS Digital.csv")



# Filter and flagging of desired staff group -----------------------------------
staff_filter <- function(raw_data,
                         suffix = 'none',
                         NHS_orgs_data = NHS_orgs,
                         nationality_data = nationality,
                         staff_group_code,
                         staff_group_name,
                         summarization_variable = 'Nationality_grouping',
                         headcount = FALSE,
                         stock = FALSE) {

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
    if (summarization_variable == 'region'){
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
    
    if (summarization_variable == 'trust') {
      raw_data <- raw_data %>%
        mutate(trust_joiners = case_when(is.na(Ocs_Code) ~ 'Unknown',
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






#Function for cleaning the joined data -----------------------------------------

clean_joined_data <- function(data, summarization_variable = 'Nationality_grouping') {
  
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







#Function for creating joiner and leaver flags ---------------------------------

joiner_leaver_flags <- function(data,
                                staff_group_name,
                                summarization_variable = 'Nationality_grouping') {
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







# Function for cleaning stock data -----------------------------------

clean_stock_data <- function(data, staff_code_name, summarization_variable){
  
  data <- data %>%  filter(Status == 'Active')%>%
    filter(Staff_group %in% staff_code_name)
  
  data <- data %>% filter(NHSD_trust_or_CCG==1 & is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate(Nationality_grouping = if_else(is.na(Nationality_grouping)==TRUE, 'Unknown',Nationality_grouping)) %>%
    filter(is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic',
                                              if_else(Nationality_grouping %in% c('EU','ROW'),'IR','Other')))
  if (summarization_variable == 'region') {
    data <- data %>%
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
  
  if (summarization_variable == 'trust') {
    data <- data %>%
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
  
  if (summarization_variable == 'age_group') {
    data <- data %>% mutate(age_group = case_when(is.na(age)~ 'Unknown',
                                                  age < 25 ~ 'Under 25',
                                                  age < 35 ~ '25 to 34',
                                                  age < 45 ~ '35 to 44',
                                                  age < 55 ~ '45 to 54',
                                                  age < 65 ~ '55 to 64',
                                                  TRUE ~ 'Over 65'))
    
  }
  return(data)

}







#Function for overwriting Nationality to correct it in both time frames --------

overwrite_y1_nationality <- function(data) {
  data$Nationality_grouping_v2_y1_2 <- ifelse(is.na(data$Nationality_grouping_v2_y2), 
                                              data$Nationality_grouping_v2_y1, 
                                              data$Nationality_grouping_v2_y2)
  
  data$Nationality_grouping_v2_y1 <- ifelse(is.na(data$Nationality_grouping_y2), 
                                            data$Nationality_grouping_y1, 
                                            data$Nationality_grouping_y2)
  
  return(data)
}





# Function for creating summaries based on 'summarization_variable' argument -------------

create_joiner_leaver_summary <- function(data, summarization_variable) {
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







# Function for creating summary of stock ---------------------------------------
create_stock_summary <- function(data, summarization_variable){
  if (summarization_variable == 'Nationality_grouping') {
    
    #nationality split
    summary_nat <- data %>% group_by(Tm_Year_Month,Nationality_grouping) %>%
      summarise (FTE=sum(Contracted_Wte)) %>% 
      arrange(Tm_Year_Month,Nationality_grouping)
    
    #FTE by nat summary
    summary_nat_2 <- data %>% group_by(Tm_Year_Month,Nationality_grouping_v2) %>%
      summarise (FTE=sum(Contracted_Wte)) %>% 
      arrange(Tm_Year_Month,Nationality_grouping_v2)
    
    #FTE summary
    summary <- data %>% group_by(Tm_Year_Month) %>%
      summarise (FTE=sum(Contracted_Wte))
    
    #align columns for FTE summary to FTE by nat
    summary <- summary %>% mutate (Nationality_grouping="All")
    
    summary_nat_2 <- rename (summary_nat_2, Nationality_grouping=Nationality_grouping_v2)
    
    #bind grouping split and total and add date
    summary <- bind_rows(summary, summary_nat,summary_nat_2) %>%
      mutate(Date=as.Date(str_replace_all(str_replace_all(paste(Tm_Year_Month,"01")," ",""),"-",""),"%Y%B%d")) %>%
      arrange(Date,Nationality_grouping)
    
    return(summary)
  }
  
  # Get total of fte
  summary <- data %>%
    summarise(FTE = sum(Contracted_Wte))%>%
    mutate(!!sym(summarization_variable) := 'All')
  
  summary_group <- data %>%
    group_by_at(vars(summarization_variable))%>%
    summarise(FTE = sum(Contracted_Wte))
  
  if (summarization_variable == 'age') {
    summary$age <- as.character(summary$age)
    summary_group$age <- as.character(summary_group$age)
  }
  
  summary <- bind_rows(summary_group, summary)
  
  return(summary)
  
}





  
  
  
# Function for the FTE of y1 ---------------------------------------------------

fte_y1_function <- function(data, staff_group_name, summarization_variable){
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
fte_y2_function <- function(data, staff_group_name, summarization_variable){
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

summary_fte_combine <- function(summary, fte_y1, fte_y2, summarization_variable, data) {
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






############################### PIN SPECIFIC  ##################################
# Function for adding pins

nmc_pin_cleaning <- function(data) {
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
  
}







###############################   MAIN RUNS    #################################
# Code for comparing two time periods and joiners and leavers ------------------

process_data_joiners_leavers <- function(raw_data_y1, raw_data_y2, staff_group_code, staff_group_name, summarization_variable, headcount = FALSE) {
  # Step 1: Start the data manipulation
  # Rename columns in nationality
  setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")
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

process_stock <- function(raw_data, staff_group_code, staff_group_name, summarization_variable, headcount = FALSE, stock = TRUE) {
  # Step 1: Start the data manipulation
  # Load and preprocess nationality data
  setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Nurse Data")
  nationality <- read_csv("Nationality groupings.csv")
  NHS_orgs <- read_csv("Org Codes NHS Digital.csv")
  colnames(nationality) <- gsub("[ ]", "_", colnames(nationality), perl=TRUE)
  
  # Call staff_filter with the loaded nationality data
  print('Starting staff_filter')
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

