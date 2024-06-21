
# Script that takes the staff codes contained the in excel file and converts each column to its own variable, to be used.

# Allows for easier amending in excel, while preserving flow.

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables")

library(readxl)

workforce_staff_codes <- read_excel('Workforce Staff Codes.xlsx')

admin_estates_staff_codes <- rev(na.omit(rev(workforce_staff_codes$admin_estates_staff_codes)))
allied_health_staff_codes <- rev(na.omit(rev(workforce_staff_codes$allied_health_staff_codes)))
ambulance_staff_codes <-rev(na.omit(rev( workforce_staff_codes$ambulance_staff_codes)))
ambulance_support_staff_codes <- rev(na.omit(rev(workforce_staff_codes$ambulance_support_staff_codes)))
clinical_science_staff_codes <- rev(na.omit(rev(workforce_staff_codes$clinical_science_staff_codes)))
clinical_science_support_codes <- rev(na.omit(rev(workforce_staff_codes$clinical_science_support_codes)))
general_payments_staff_codes <- rev(na.omit(rev(workforce_staff_codes$general_payments_staff_codes)))
hca_support_codes <- rev(na.omit(rev(workforce_staff_codes$hca_support_codes)))
life_science_staff_codes <-rev(na.omit(rev(workforce_staff_codes$life_science_staff_codes)))
life_science_support_codes<- rev(na.omit(rev(workforce_staff_codes$life_science_support_codes)))
medical_staff_gp_codes <-rev(na.omit(rev(workforce_staff_codes$medical_staff_gp_codes)))
medical_staff_hospital_codes <-rev(na.omit(rev(workforce_staff_codes$medical_staff_hospital_codes)))
midwife_staff_codes <-rev(na.omit(rev(workforce_staff_codes$midwife_staff_codes)))
nurse_health_visitor_codes<- rev(na.omit(rev(workforce_staff_codes$nurse_health_visitor_codes)))
nurse_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_staff_codes)))
nurse_support_codes <- rev(na.omit(rev(workforce_staff_codes$nurse_support_codes)))
nursing_associate_codes <-rev(na.omit(rev(workforce_staff_codes$nursing_associate_codes)))
physical_science_staff_codes <-rev(na.omit(rev(workforce_staff_codes$physical_science_staff_codes)))
physical_science_support_codes<- rev(na.omit(rev(workforce_staff_codes$physical_science_support_codes)))
physiological_science_staff_codes <- rev(na.omit(rev(workforce_staff_codes$physiological_science_staff_codes)))
physiological_science_support_codes<- rev(na.omit(rev(workforce_staff_codes$physiological_science_support_codes)))
public_science_staff_codes <- rev(na.omit(rev(workforce_staff_codes$public_science_staff_codes)))
public_science_support_codes <- rev(na.omit(rev(workforce_staff_codes$public_science_support_codes)))
scientific_staff_codes <- rev(na.omit(rev(workforce_staff_codes$scientific_staff_codes)))
scientific_support_staff_codes<- rev(na.omit(rev(workforce_staff_codes$scientific_support_staff_codes)))
support_allied_staff_codes <-rev(na.omit(rev(workforce_staff_codes$support_allied_staff_codes)))
nurse_adult_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_adult_staff_codes)))
nurse_children_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_children_staff_codes)))
nurse_maternity_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_maternity_staff_codes)))
nurse_community_mental_health_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_community_mental_health_staff_codes)))
nurse_other_mental_health_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_other_mental_health_staff_codes)))
nurse_community_learning_difficulties_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_community_learning_difficulties_staff_codes)))
nurse_other_learning_difficulties_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_other_learning_difficulties_staff_codes)))
nurse_community_service_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_community_service_staff_codes)))
nurse_education_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_education_staff_codes)))
nurse_school_nursing_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_school_nursing_staff_codes)))
nurse_neonatal_staff_codes <-rev(na.omit(rev(workforce_staff_codes$nurse_neonatal_staff_codes)))
nurse_learner_staff_codes <- rev(na.omit(rev(workforce_staff_codes$nurse_learner_staff_codes)))







