

# Function for cleaning stock data -----------------------------------

#' Clean Stock Data
#'
#' This function cleans the provided staff data based on specified conditions and adds necessary grouping variables for summarization.
#'
#' @param data A data frame containing the staff data with necessary columns like Status, Staff_group, NHSD_trust_or_CCG, Unique_Nhs_Identifier, Nationality_grouping, and Ocs_Code.
#' @param staff_code_name A character vector specifying the staff group names to filter for.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'region', 'trust', and 'age_group'.
#'
#' @return A cleaned data frame with additional columns for summarization based on the specified variable.
#'
#' @details
#' The function performs the following operations:
#' - Filters the data to include only active staff within the specified staff groups.
#' - Further filters the data to include only rows where the organization is an NHS trust or CCG and the Unique NHS Identifier is not missing.
#' - Replaces missing values in the Nationality_grouping column with 'Unknown'.
#' - Creates a new column \code{Nationality_grouping_v2} with values 'Domestic', 'IR', or 'Other' based on the original Nationality_grouping.
#' - Adds a new summarization variable based on the specified input. The possible summarization variables are:
#'   - \code{region}: Groups data into different regions based on Ocs_Code.
#'   - \code{trust}: Groups data into different trusts based on Ocs_Code.
#'   - \code{age_group}: Groups data into age categories based on the age column.
#'
#' @examples
#' \dontrun{
#' cleaned_data <- clean_stock_data(data, c('Nurse', 'Midwife'), summarization_variable = 'region')
#' }
#'
#' @import dplyr
#' @export

clean_stock_data <- function(data, staff_code_name, summarization_variable){
  print('Cleaning data')
  
  data <- data %>%  filter(Status == 'Active')%>%
    filter(Staff_group %in% staff_code_name)
  
  data <- data %>% filter(NHSD_trust_or_CCG==1 & is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate(Nationality_grouping = if_else(is.na(Nationality_grouping)==TRUE, 'Unknown',Nationality_grouping)) %>%
    filter(is.na(Unique_Nhs_Identifier)==FALSE) %>%
    mutate (Nationality_grouping_v2 = if_else(Nationality_grouping %in% c('UK','Unknown'),'Domestic',
                                              if_else(Nationality_grouping %in% c('EU','ROW'),'IR','Other')))
  if (summarization_variable == 'region') {
    source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/org_region_codes.R')
    
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
    source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables/org_trust_codes.R')
    
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



# Function for creating summary of stock ---------------------------------------
#' Create Stock Summary
#'
#' This function generates a summary of stock based on the specified summarization variable.
#'
#' @param data A data frame containing stock data.
#' @param summarization_variable A character string indicating the variable used for summarization. Possible values are 'Nationality_grouping', 'age', 'age_group', 'region', 'trust', and 'Ocs_Code'.
#'
#' @return A summarized data frame with total Full-Time Equivalent (FTE) and, if applicable, FTE breakdown by the specified summarization variable.
#'
#' @details
#' The function performs the following operations:
#' - If the summarization variable is 'Nationality_grouping', it generates three sets of summaries: 
#'   - FTE by nationality groups for each year and month.
#'   - FTE by the refined nationality grouping for each year and month.
#'   - Total FTE for each year and month, with an additional row representing the total FTE for all nationalities.
#' - If the summarization variable is not 'Nationality_grouping', it calculates the total FTE for each category.
#' - If the summarization variable is 'age', it converts the 'age' variable to a character type.
#'
#' @examples
#' \dontrun{
#' stock_summary <- create_stock_summary(data, summarization_variable = 'Nationality_grouping')
#' }
#'
#' @import dplyr
#' @export


create_stock_summary <- function(data, summarization_variable){
  print('Creating summary')
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
