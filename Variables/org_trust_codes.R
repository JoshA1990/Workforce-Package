# Script that takes the org codes contained the in excel file and converts each column to its own variable, to be used.

# Allows for easier amending in excel, while preserving flow.

setwd("C:/Users/Josh.Andrews/OneDrive - Department of Health and Social Care/Documents/R Codes/Workforce-Package/Variables")

library(readxl)


# Org trust codes 

workforce_org_regions <- read_excel('Workforce Org Regions.xlsx')

orgs_bath_somerset_swindon_and_wiltshire <- rev(na.omit(rev(workforce_org_regions$orgs_bath_somerset_swindon_and_wiltshire)))
orgs_bedfordshire_luton_and_milton_keynes <- rev(na.omit(rev(workforce_org_regions$orgs_bedfordshire_luton_and_milton_keynes)))
orgs_birmingham <- rev(na.omit(rev(workforce_org_regions$orgs_birmingham)))
orgs_black_country <- rev(na.omit(rev(workforce_org_regions$orgs_black_country)))
orgs_bristol_north_somerset_and_south_gloucestershire <- rev(na.omit(rev(workforce_org_regions$orgs_bristol_north_somerset_and_south_gloucestershire)))
orgs_buckinghamshire_oxfordshire_and_berkshire_west <- rev(na.omit(rev(workforce_org_regions$orgs_buckinghamshire_oxfordshire_and_berkshire_west)))
orgs_cambridgeshire_and_peterborough <- rev(na.omit(rev(workforce_org_regions$orgs_cambridgeshire_and_peterborough)))
orgs_cheshire_and_merseyside <- rev(na.omit(rev(workforce_org_regions$orgs_cheshire_and_merseyside)))
orgs_coventry_and_warwickshire <- rev(na.omit(rev(workforce_org_regions$orgs_coventry_and_warwickshire)))
orgs_derbyshire <- rev(na.omit(rev(workforce_org_regions$orgs_derbyshire)))
orgs_devon <-rev(na.omit(rev(workforce_org_regions$orgs_devon)))
orgs_dorset <- rev(na.omit(rev(workforce_org_regions$orgs_dorset)))
orgs_frimley <- rev(na.omit(rev(workforce_org_regions$orgs_frimley)))
orgs_gloucestershire <- rev(na.omit(rev(workforce_org_regions$orgs_gloucestershire)))
orgs_greater_manchester <- rev(na.omit(rev(workforce_org_regions$orgs_greater_manchester)))
orgs_hampshire_and_isle_of_wight <- rev(na.omit(rev(workforce_org_regions$orgs_hampshire_and_isle_of_wight)))
orgs_herefordshire_and_worcestershire<- rev(na.omit(rev(workforce_org_regions$orgs_herefordshire_and_worcestershire)))
orgs_hertfordshire_and_west_essex <- rev(na.omit(rev(workforce_org_regions$orgs_hertfordshire_and_west_essex)))
orgs_humber_and_north_yorkshire <- rev(na.omit(rev(workforce_org_regions$orgs_humber_and_north_yorkshire)))
orgs_kent_and_medway <- rev(na.omit(rev(workforce_org_regions$orgs_kent_and_medway)))
orgs_lancashire_and_south_cumbria <- rev(na.omit(rev(workforce_org_regions$orgs_lancashire_and_south_cumbria)))
orgs_leicestershire_and_rutland <-rev(na.omit(rev(workforce_org_regions$orgs_leicestershire_and_rutland)))
orgs_lincolnshire <- rev(na.omit(rev(workforce_org_regions$orgs_lincolnshire)))
orgs_mid_and_south_essex <- rev(na.omit(rev(workforce_org_regions$orgs_mid_and_south_essex)))
orgs_norfolk_and_waveney <- rev(na.omit(rev(workforce_org_regions$orgs_norfolk_and_waveney)))
orgs_north_central_london <-rev(na.omit(rev(workforce_org_regions$orgs_north_central_london)))
orgs_north_east_and_north_cumbria <-rev(na.omit(rev(workforce_org_regions$orgs_north_east_and_north_cumbria)))
orgs_north_east_london <- rev(na.omit(rev(workforce_org_regions$orgs_north_east_london)))
orgs_north_west_london <- rev(na.omit(rev(workforce_org_regions$orgs_north_west_london)))
orgs_northamptonshire <- rev(na.omit(rev(workforce_org_regions$orgs_northamptonshire)))
orgs_nottinghamshire <- rev(na.omit(rev(workforce_org_regions$orgs_nottinghamshire)))
orgs_shropshire_telford_and_wrekin <-rev(na.omit(rev(workforce_org_regions$orgs_shropshire_telford_and_wrekin)))
orgs_somerset <-rev(na.omit(rev(workforce_org_regions$orgs_somerset)))
orgs_south_east_london <- rev(na.omit(rev(workforce_org_regions$orgs_south_east_london)))
orgs_south_west_london <- rev(na.omit(rev(workforce_org_regions$orgs_south_west_london)))
orgs_south_yorkshire <- rev(na.omit(rev(workforce_org_regions$orgs_south_yorkshire)))
orgs_staffordshire_and_stoke_on_trent <- rev(na.omit(rev(workforce_org_regions$orgs_staffordshire_and_stoke_on_trent)))
orgs_suffolk_and_north_east_essex <- rev(na.omit(rev(workforce_org_regions$orgs_suffolk_and_north_east_essex)))
orgs_surrey_heartlands <- rev(na.omit(rev(workforce_org_regions$orgs_surrey_heartlands)))
orgs_sussex <- rev(na.omit(rev(workforce_org_regions$orgs_sussex)))
orgs_west_yorkshire <- rev(na.omit(rev(workforce_org_regions$orgs_west_yorkshire)))