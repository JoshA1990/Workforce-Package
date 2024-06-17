############################### PIN SPECIFIC  ##################################
# Function for adding pins


#' NMC PIN Cleaning
#'
#' This function cleans the dataset by removing ineligible NMC PINs based on specific criteria.
#' @param data A data frame containing the cleaned staff data with joiners and leavers.
#'
#' @return A cleaned data frame, containing only accurate NMC PIN formats.
#' 
#' @details
#' The function performs the following operations:
#' - Defines a helper function to assign month numbers when the letters 'Y' or 'U' are present in the PIN.
#' - Filters the data to keep only valid PIN entries:
#'     - Removes rows without a PIN number for year 2.
#'     - Selects PINs that are 8 characters long.
#'     - Ensures the first and second characters are digits.
#'     - Ensures the 8th character is one of 'A, C, D, E, N, S, O, W' or '0'.
#'     - Ensures the 3rd character is one of 'A, B, C, D, E, F, G, H, I, J, K, Y, U'.
#' - Recodes the 8th character to the matching country of training.
#' - Extracts the year and month from the first 3 characters, or assigns a random month if 'Y' or 'U'.
#' - Converts the year to a full date, using 2000s for years less than 25, otherwise 1900s.
#' - Converts to a date format and checks against the date of birth, filtering out entries where the person would be under 16 when receiving the PIN.
#' 
#' @examples
#' \dontrun{
#' cleaned_pins <- nmc_pin_cleaning(data)
#' }
#'
#' @import dplyr
#' @import lubridate
#' @export

nmc_pin_cleaning <- function(data) {
  print('Starting PIN cleaning')
  
  # Helper function to assign month numbers for letters Y or U
  convert_to_number <- function(letter) {
    if (toupper(letter) %in% LETTERS[1:12]) {
      return(match(toupper(letter), LETTERS[1:12]))
    } else {
      return(sample(1:12, 1))
    }
  }
  
  # Filter and clean the data based on PIN criteria
  pin_data <- data %>%
    filter(!is.na(Registration_Number_y2)) %>%
    filter(str_length(Registration_Number_y2) == 8) %>%
    filter(str_detect(Registration_Number_y2, "^[0-9]{2}")) %>%
    filter(str_sub(Registration_Number_y2, 8, 8) %in% c("A", "C", "D", "E", "N", "O", "S", "W", "0")) %>%
    filter(str_sub(Registration_Number_y2, 3, 3) %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "Y", "U")) %>%
    mutate(
      country_of_training = recode(toupper(str_sub(Registration_Number_y2, 8, 8)), 
                                   A = "UK", 
                                   C = "EU",
                                   D = "UK",
                                   E = "England",
                                   N = "Northern Ireland",
                                   O = "Overseas (non EU)",
                                   '0' = 'Overseas (non EU)',
                                   S = "Scotland",
                                   W = "Wales"),
      registration_year = str_sub(Registration_Number_y2, 1, 2),
      registration_month = str_sub(Registration_Number_y2, 3, 3),
      registration_month = sapply(registration_month, convert_to_number),
      registration_date = if_else(as.numeric(registration_year) < 25, 
                                  paste0("20", registration_year, "-", registration_month, "-01"), 
                                  paste0("19", registration_year, "-", registration_month, "-01"))
    ) %>%
    mutate(
      Date_Of_Birth_y2 = ymd(Date_Of_Birth_y2),
      current_month = as.Date(paste0(as.numeric(substr(Tm_Year_Month_y2, 1, 4)), "-", 
                                     recode(substr(Tm_Year_Month_y2, 6, 8), 
                                            JAN = 01, FEB = 02, MAR = 03, APR = 04, 
                                            MAY = 05, JUN = 06, JUL = 07, AUG = 08, 
                                            SEP = 09, OCT = 10, NOV = 11, DEC = 12), 
                                     "-01")),
      registration_date = ymd(registration_date),
      age_check = as.numeric(difftime(registration_date, Date_Of_Birth_y2, units = "weeks")) / 52.25
    ) %>%
    filter(Date_Of_Birth_y2 < registration_date, registration_date <= current_month, age_check > 16)
  
  
  # Create time periods for transition
  current_month <- max(pin_data$current_month, na.rm = TRUE)
  months_5 <- current_month %m-% months(5)
  months_11 <- current_month %m-% months(11)
  months_17 <- current_month %m-% months(17)
  months_23 <- current_month %m-% months(23)
  months_59 <- current_month %m-% months(59)
  months_119 <- current_month %m-% months(119)
  
  # Categorize data based on registration date
  pin_data <- pin_data %>%
    mutate(NMC_to_ESR = case_when(
      is.na(registration_date) ~ "Unknown",
      registration_date == current_month ~ "Immediate Joiner",
      registration_date >= months_5 ~ "6 months",
      registration_date >= months_11 ~ "12 months",
      registration_date >= months_17 ~ "18 months",
      registration_date >= months_23 ~ "24 months",
      registration_date >= months_59 ~ "5 years",
      registration_date >= months_119 ~ "10 years",
      TRUE ~ "Over 10 years"
    ))
  
  
  return(pin_data)
}

# PIN summary ------------------------------------------------------------------

#' PIN Data Summary
#'
#' This function generates summaries based on the specified summary variable, focusing on joiners and occ_joiners.
#' 
#' @param data A data frame containing the cleaned PIN data of staff.
#' @param pin_summary_variable A character string indicating the variable used for summarization. Possible values are 'country_of_training', 'age', 'age_group', and 'NMC_to_ESR'.
#'
#' @return A summarized data frame with joiner statistics aggregated based on the specified summarization variable.
#' 
#' @details
#' The function performs the following operations:
#' - Calculates total joiners and occ_joiners based on the pin_summary_variable.
#'    - If the pin_summary_variable is 'age' or 'age_group', it calculates age and inserts the column to be summarized upon.
#'    - If the pin_summary_variable is 'NMC_to_ESR', it creates a difftime based on months and groups into the time taken to transition to ESR.
#'
#' @examples
#' \dontrun{
#' summary_data <- pin_data_summary(data, pin_summary_variable = 'country_of_training')
#' }
#'
#' @import dplyr
#' @import lubridate
#' @export


pin_data_summary <- function(data, pin_summary_variable = NULL, uk_nqn = FALSE) {
  print('Starting PIN summary')
  
  if (uk_nqn == TRUE) {
    data <- data %>%
      filter(country_of_training %in% c('UK', 'Scotland', 'Wales', 'England', 'Northern Ireland'))%>%
      filter(NMC_to_ESR %in% c('Immediate Joiner', '6 months', '12 months'))%>%
      filter(Afc_Spinal_Point_y2 == 16.1 | 16)
  }
  
  if (pin_summary_variable == 'country_of_training'){
    summary <- data %>% group_by(country_of_training)%>%
      summarise(joiner = sum(joiner),
                occ_joiner = sum(occ_joiner))%>%
      pivot_longer(2:3)
  }
  
  if (pin_summary_variable == 'NMC_to_ESR') {
    date_order <- c(
      "Immediate Joiner",
      "6 months",
      "12 months",
      "18 months",
      "24 months",
      "5 years",
      "10 years",
      "Over 10 years",
      "Unknown"
    )
    
    # Create a data frame to ensure all periods are included in the summary
    all_periods <- tibble(NMC_to_ESR = factor(date_order, levels = date_order))
    
    summary <- data %>%
      group_by(NMC_to_ESR) %>%
      summarise(joiner = sum(joiner, na.rm = TRUE),
                occ_joiner = sum(occ_joiner, na.rm = TRUE)) %>%
      pivot_longer(cols = c(joiner, occ_joiner), names_to = "name", values_to = "value") %>%
      right_join(all_periods, by = "NMC_to_ESR") %>%
      replace_na(list(value = 0)) %>%
      mutate(NMC_to_ESR = factor(NMC_to_ESR, levels = date_order)) %>%
      arrange(NMC_to_ESR)
  } 
  
  if (pin_summary_variable == 'age'){
    data <- data %>%
      mutate(
        Date = as.Date(paste0(Tm_Year_Month_y2, "-01"), tryFormats = c("%Y-%m-%d", "%Y-%B-%d")),
        Date_Of_Birth = as.Date(Date_Of_Birth_y2, tryFormats = c("%Y-%m-%d", "%Y-%B-%d"))
      )
    
    # Calculate the age difference in days
    data <- data %>%
      mutate(
        age_difference_days = difftime(Date, Date_Of_Birth, units = "days"),
        age_years = as.numeric(age_difference_days) / 365.25,
        age = round(age_years)
      )
    
    # Create summary using date 
    summary <- data %>% group_by(age)%>%
      summarise(joiner = sum(joiner),
                occ_joiner = sum(occ_joiner))%>%
      pivot_longer(cols = c(joiner, occ_joiner), names_to = "name", values_to = "value")
  }
  
  
  if (pin_summary_variable == 'age_group'){
    data <- data %>%
      mutate(
        Date = as.Date(paste0(Tm_Year_Month_y2, "-01"), tryFormats = c("%Y-%m-%d", "%Y-%B-%d")),
        Date_Of_Birth = as.Date(Date_Of_Birth_y2, tryFormats = c("%Y-%m-%d", "%Y-%B-%d"))
      )
    
    # Calculate the age difference in days
    data <- data %>%
      mutate(
        age_difference_days = difftime(Date, Date_Of_Birth, units = "days"),
        age_years = as.numeric(age_difference_days) / 365.25,
        age = round(age_years)
      )
    
    # Assign the age groups
    data <- data %>%
      mutate(
        age_group = case_when(
          is.na(age) ~ 'Unknown',
          age < 25 ~ 'Under 25',
          age < 35 ~ '25 to 34',
          age < 45 ~ '35 to 44',
          age < 55 ~ '45 to 54',
          age < 65 ~ '55 to 64',
          TRUE ~ 'Over 65'
        )
      )
    
    # Define the order of age groups
    age_order <- c('Under 25', '25 to 34', '35 to 44', '45 to 54', '55 to 64', 'Over 65', 'Unknown')
    
    # Create a data frame to ensure all age groups are included in the summary
    all_age_groups <- tibble(age_group = factor(age_order, levels = age_order))
    
    # Create summary using date and ensure all age groups are present
    summary <- data %>%
      group_by(age_group) %>%
      summarise(
        joiner = sum(joiner, na.rm = TRUE),
        occ_joiner = sum(occ_joiner, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(joiner, occ_joiner), names_to = "name", values_to = "value") %>%
      right_join(all_age_groups, by = "age_group") %>%
      replace_na(list(value = 0)) %>%
      mutate(age_group = factor(age_group, levels = age_order)) %>%
      arrange(age_group)
    
  }
  
  return(summary)
}

