

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/WTO/data/BRA")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)


################################################################################
# get either specific to 

################################################################################

#specific to Brazil
library(readr)
dta <- read_csv("BRA_tariffs.csv")
names(dta)
head(dta)
table(dta$'Tariff Year')
table(dta$DutyType)

################################################################################



dta <- dta %>% rename( ReporterISO3 = 'Reporter Name',
                       PartnerI = 'Partner Name',
                       ProductCode = Product ,
                       Year = 'Tariff Year')



#get isocde instead
library(countrycode)
custom_mappings <- c("World" = WLD)

# Assuming 'dta' is your dataframe
dta <- dta %>%
  mutate(PartnerISO3 = countrycode(PartnerI, origin = "country.name", destination = "iso3c"))


dta <- dta %>%
  mutate(PartnerISO3 = countrycode(PartnerI, origin = "country.name", destination = "iso3c"),
         PartnerISO3 = ifelse(PartnerI == "World", "WLD", PartnerISO3))

na_observations <- dta %>%   filter(is.na(PartnerISO3))
table(na_observations$PartnerI)
dta <- dta %>% filter(!is.na(PartnerISO3))


dta <- dta %>% mutate(ReporterISO3 = countrycode(ReporterISO3, origin = "country.name", destination = "iso3c"))


################################################################################
table(dta$DutyType)
# get MFN
dta <- subset(dta, DutyType == "MFN")

#get applied tariffs 
dta <- subset(dta, DutyType == "AHS")

names(dta)

#use weighted average 
dta <- dta %>% rename( MFN_tariff = 'Simple Average')
dta <- dta %>% rename( AHS_tariff = 'Simple Average')



################################################################################
# selected the required variables 
names(dta)
clean_dta <- select(dta, Year, ReporterISO3, ProductCode, PartnerISO3, MFN_tariff)
clean_dta <- select(dta, Year, ReporterISO3, ProductCode, PartnerISO3, AHS_tariff)

colSums(is.na(clean_dta))

################################################################################
# export dta 

write.csv(clean_dta, "/home/sikeme/TRADE/WTO/data/BRA/clean_BRA_tariff.csv", row.names = FALSE)

write.csv(clean_dta, "/data/sikeme/TRADE/WTO/data/BRA/clean_BRA_tariff_AHS.csv", row.names = FALSE)





################################################################################



                          # from WTS directly #


################################################################################

# More info : 
# https://github.com/diegoacastro/witstrainsr
# https://wits.worldbank.org/data/public/WITSAPI_UserGuide.pdf



# get it directly :
library(witstrainsr)


tariffs <- get_tariffs(
  reporter = "076",
  year = "2015"
)

###############################################################################
# get tariffs data 







