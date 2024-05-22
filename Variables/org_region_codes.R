
# Script that takes the org codes contained the in excel file and converts each column to its own variable, to be used.

# Allows for easier amending in excel, while preserving flow.

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables")

library(readxl)


# Org region codes 

workforce_org_regions <- read_excel('Workforce Org Regions.xlsx')

orgs_region_east_england <- rev(na.omit(rev(workforce_org_regions$orgs_region_east_england)))
orgs_region_london <- rev(na.omit(rev(workforce_org_regions$orgs_region_london)))
orgs_region_midlands <-rev(na.omit(rev(workforce_org_regions$orgs_region_midlands)))
orgs_region_north_east_and_yorkshire <- rev(na.omit(rev(workforce_org_regions$orgs_region_north_east_and_yorkshire)))
orgs_region_north_west <- rev(na.omit(rev(workforce_org_regions$orgs_region_north_west)))
orgs_region_south_west <- rev(na.omit(rev(workforce_org_regions$orgs_region_south_west)))
orgs_region_southeast <- rev(na.omit(rev(workforce_org_regions$orgs_region_southeast)))

