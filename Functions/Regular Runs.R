
process_monthly_extracts <- function(){
  source(paste0(here("Functions"),"Main Processing.R"))
  # Stocks by FTE
  write_csv(process_stock(raw_data = Raw_Data_y2,
                          staff_group_code = nurse_staff_codes,
                          staff_group_name = 'Nurse',
                          summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Stocks FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  #Stocks by Headcount
  write_csv(process_stock(raw_data = Raw_Data_y2,
                          staff_group_code = nurse_staff_codes,
                          staff_group_name = 'Nurse',
                          summarization_variable = 'Nationality_grouping',
                          headcount = TRUE),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Stocks HC/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = nurse_staff_codes,
                                         staff_group_name = 'Nurse',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Monthly FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  # Joiners Leavers by Nationality HC
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = nurse_staff_codes,
                                         staff_group_name = 'Nurse',
                                         summarization_variable = 'Nationality_grouping',
                                         headcount = TRUE),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Monthly HC/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  #Joiners Leavers by Trust
  write_csv(process_nurse_trust(staff_group_code = nurse_staff_codes,
                                staff_group_name ='Nurse'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Monthly Trust/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  ### Midwife --------------------------------------------------------------------
  
  # Stocks by FTE
  write_csv(process_stock(raw_data = Raw_Data_y2,
                          staff_group_code = midwife_staff_codes,
                          staff_group_name = 'Midwife',
                          summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Stocks FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  #Stocks by Headcount
  write_csv(process_stock(raw_data = Raw_Data_y2,
                          staff_group_code = midwife_staff_codes,
                          staff_group_name = 'Midwife',
                          summarization_variable = 'Nationality_grouping',
                          headcount = TRUE),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Stocks HC/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = midwife_staff_codes,
                                         staff_group_name = 'Midwife',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Joiners Leavers Monthly FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  # Joiners Leavers by Nationality HC
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = midwife_staff_codes,
                                         staff_group_name = 'Midwife',
                                         summarization_variable = 'Nationality_grouping',
                                         headcount = TRUE),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Joiners Leavers Monthly HC/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  #Joiners Leavers by Trust
  write_csv(process_nurse_trust(staff_group_code = midwife_staff_codes,
                                staff_group_name ='Midwife'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Joiners Leavers Monthly Trust/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  ### Nursing Associate ----------------------------------------------------------
  
  # Stocks by FTE
  write_csv(process_stock(raw_data = Raw_Data_y2,
                          staff_group_code = nursing_associate_codes,
                          staff_group_name = 'Nursing Associate',
                          summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Stocks FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  #Stocks by Headcount
  write_csv(process_stock(raw_data = Raw_Data_y2,
                          staff_group_code = nursing_associate_codes,
                          staff_group_name = 'Nursing Associate',
                          summarization_variable = 'Nationality_grouping',
                          headcount = TRUE),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nursing Associate/Tracking/Stocks HC/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = nursing_associate_codes,
                                         staff_group_name = 'Nursing Associate',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nursing Associate/Tracking/Joiners Leavers Monthly FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
}

process_annual_extracts <- function(){
  source(paste0(here("Functions"),"Main Processing.R"))
  
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = nurse_staff_codes,
                                         staff_group_name = 'Nurse',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Yearly FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  

  #Joiners Leavers by Trust
  write_csv(process_nurse_trust(staff_group_code = nurse_staff_codes,
                                staff_group_name ='Nurse'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Yearly Trust/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  ### Midwife --------------------------------------------------------------------
  
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = midwife_staff_codes,
                                         staff_group_name = 'Midwife',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Joiners Leavers Yearly FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  ### Nursing Associate ----------------------------------------------------------

  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = nursing_associate_codes,
                                         staff_group_name = 'Nursing Associate',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nursing Associate/Tracking/Joiners Leavers Yearly FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
}

process_cumulative_extracts <- function(){
  source('C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Functions/Main Processing.R')
  source(paste0(here("Functions"),"/Main Processing.R"))
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = nurse_staff_codes,
                                         staff_group_name = 'Nurse',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Cumulative FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  
  #Joiners Leavers by Trust
  write_csv(process_nurse_trust(staff_group_code = nurse_staff_codes,
                                staff_group_name ='Nurse'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Nurses/Tracking/Joiners Leavers Cumulative Trust/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))
  
  ### Midwife --------------------------------------------------------------------
  
  # Joiners Leavers by Nationality FTE
  write_csv(process_data_joiners_leavers(raw_data_y1 = Raw_Data_y1,
                                         raw_data_y2 = Raw_Data_y2,
                                         staff_group_code = midwife_staff_codes,
                                         staff_group_name = 'Midwife',
                                         summarization_variable = 'Nationality_grouping'),
            paste0('C:/Users/',Sys.getenv("USERNAME"),'/OneDrive - Department of Health and Social Care/wf/Cross-cutting work/Brexit/Nursing/LTP/PMIU work - 2019 elections/Tracking 50k/ESR Runs/Outputs/Midwife/Tracking/Joiners Leavers Cumulative FTE/',substr(Raw_Data_y1$`Tm Year Month`,1,8)[1],'.csv'))

}