




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/WTO/data")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)
library(data.table)
library(stringi)
library(fixest)




# 1) load data, and filter the years of interest:

dta_TBT <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WTO/clean_WTO_TBT.csv")
dta_SPS <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WTO/clean_WTO_SPS.csv")

# checks:
names(dta_SPS)
names(dta_TBT)
table(dta_SPS$ReporterISO3)
table(dta_SPS$PartnerISO3)
table(dta_TBT$PartnerISO3)


################################################################################

# 2) Select country of interest 

countries <- c("BRA", "ARG", "BOL", "COL", "CHL", "ECU", "PRY", "URY", "PER")

dta_SPS1 <- dta_SPS %>% filter(ReporterISO3 %in% countries)
dta_TBT1 <- dta_TBT %>% filter(ReporterISO3 %in% countries)

# rename the count variable
dta_SPS1 <- dta_SPS1 %>% select(-SPS_count) %>% rename(SPS_count = SPS1)
dta_TBT1 <- dta_TBT1 %>% select(- TBT_count) %>% rename(TBT_count = TBT1)

################################################################################

# 3) convert when partner if WLD or EU

# to get trade share data  we need to get WLD representing countries in our data 
trade_share <- fread( "dta_WITS/trade_share/clean_trade_share.csv")
# drop eu (as a group)
trade_share1 <- trade_share %>% filter(ReporterISO3 != "EUN")
length(unique(trade_share1$ReporterISO3))
WLD <- unique(trade_share1$ReporterISO3)

# and for EU:
eu <- c("AUT", "BEL", "BGR", "HRV", "CYP","CZE", "DNK", "EST", "FIN", "FRA", 
                  "DEU", "GRC", "HUN", "IRL", "ITA","LVA", "LTU", "LUX", "MLT", "NLD", 
                  "POL", "PRT", "ROU", "SVK", "SVN","ESP", "SWE")


# Convert EUN and WLD so that can be matched to trade shar
# check how many countries in the data
length(unique(dta_SPS1$PartnerISO3))



# For SPS and WORLD value: 
WLD_partner <- function(WLD, ntm_data) {
  new_partners <- WLD  # Get unique partners that are not "WLD" from trade share data
  wld_rows <- filter(ntm_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(ntm_data %>% filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  combined_data <- combined_data %>% group_by(Year,ProductCode, ReporterISO3,PartnerISO3) %>%
    summarise(SPS_count  = sum(SPS_count))
  return(combined_data)
}

dta_SPS2 <- WLD_partner(WLD,dta_SPS1)


# For Eu
table(dta_SPS1$PartnerISO3)
EUN_partner <- function(eu, ntm_data) {
  new_partners <- eu  
  wld_rows <- filter(ntm_data, PartnerISO3 == "EUN")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(ntm_data %>% filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  combined_data <- combined_data %>% group_by(Year,ProductCode, ReporterISO3,PartnerISO3) %>%
    summarise(SPS_count  = sum(SPS_count))
  return(combined_data)
}

dta_SPS3 <- EUN_partner(eu,dta_SPS2)







# For TBT and WORLD value: 
WLD_partner <- function(WLD, ntm_data) {
  new_partners <- WLD  # Get unique partners that are not "WLD" from trade share data
  # Filter rows where 'PartnerISO3' is 'WLD' from UNCTAD data
  wld_rows <- filter(ntm_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(ntm_data %>% filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  combined_data <- combined_data %>% group_by(Year,ProductCode, ReporterISO3,PartnerISO3) %>%
    summarise(TBT_count  = sum(TBT_count))
  return(combined_data)
}

dta_TBT2 <- WLD_partner(WLD,dta_TBT1)


# For Eu
table(dta_SPS1$PartnerISO3)
EUN_partner <- function(eu, ntm_data) {
  new_partners <- eu  
  wld_rows <- filter(ntm_data, PartnerISO3 == "EUN")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(ntm_data %>% filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  combined_data <- combined_data %>% group_by(Year,ProductCode, ReporterISO3,PartnerISO3) %>%
    summarise(TBT_count  = sum(TBT_count))
  return(combined_data)
}

dta_TBT3 <- EUN_partner(eu,dta_TBT2)


table(dta_SPS2)
colSums(is.na(dta_SPS2))
################################################################################
# Get the trade share data



# a) load the data
trade_share <- fread( "dta_WITS/trade_share/clean_trade_share.csv")
names(trade_share)
length(unique(trade_share$ReporterISO3))
table(trade_share$ReporterISO3)
share <- unique(trade_share$ReporterISO3)

X_vars <- fread("/data/sikeme/TRADE/WTO/data/quant_based/merge2.csv")
length(unique(X_vars$PartnerISO3))
table(X_vars$PartnerISO3)

length(unique(dta_SPS1$PartnerISO3))
table(dta_SPS1$PartnerISO3)

trade <- unique(X_vars$PartnerISO3)


all_elements_present <- all(share %in% trade)

all_elements_present <- all(eu_iso_codes %in% share)

missing_elements <- setdiff(share, trade)

# ROM -> ROU
# SER -> SRB
# ZAR -> COD
# SUD -> SDN
# MNT -> MNE

################################################################################


# b) select neighboring countries as an IV 


#Get the countries of interest 
names(dta_UNCTAD_SPS)
unique(dta_UNCTAD_SPS$Reporter)
unique(dta_UNCTAD_SPS$Partner)
table(dta_UNCTAD_SPS$Reporter, dta_UNCTAD_SPS$Year)


#create a dataset with only countries of interest
country_data <- list()
countries <- c("BRA", "CHL","BOL", "COL","ECU", "PER")
for (country in countries) {
  # Subset the data for the current country
  country_subset <- subset(dta_UNCTAD_SPS, Reporter == country, select = c(Year, Partner, HSCode, ntm_all))
  
  # Rename the ntm_all column to NTM_COUNTRY_SPS
  country_subset <- country_subset %>%
    rename(!!paste0("NTM_", country, "_SPS") := ntm_all)
  
  # Filter data for the specified year
  #country_subset <- filter(country_subset, Year == 2015)
  
  # Group by Year, Partner, and HSCode and summarize
  country_summary <- country_subset %>%
    group_by(Year, Partner, HSCode) %>%
    summarise(!!paste0("total_NTM_", country, "_SPS") := sum(!!rlang::sym(paste0("NTM_", country, "_SPS")), na.rm = TRUE)) %>%
    ungroup() 
  # Store the subset in the country_data list
  country_data[[country]] <- country_summary
}
# Perform full join on all data frames in the list
merged_UNCTAD_NTM <- Reduce(full_join, country_data)



