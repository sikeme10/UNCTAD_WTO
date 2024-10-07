



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




################################################################################


# Load data


################################################################################

# a) UNCTAD Version SPS

#load data:
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")



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


# put value 0 instead of NA 
colSums(is.na(merged_UNCTAD_NTM))
merged_UNCTAD_NTM <- merged_UNCTAD_NTM %>% mutate_all(~replace_na(., 0))

# Get HS levels variables
merged_UNCTAD_NTM$H1<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 1)
merged_UNCTAD_NTM$H2<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 2)
merged_UNCTAD_NTM$H3<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 3)
merged_UNCTAD_NTM$H4<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 4)
merged_UNCTAD_NTM$H5<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 5)



# Can aggregate NTM counts at HS 5 level:

names(merged_UNCTAD_NTM)
#choose merging level at HS5 level: 
merged_UNCTAD_NTM  <- merged_UNCTAD_NTM %>% group_by(Year, Partner, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum (total_NTM_BRA_SPS),
            total_NTM_CHL_SPS = sum (total_NTM_CHL_SPS),
            total_NTM_BOL_SPS = sum (total_NTM_BOL_SPS),
            total_NTM_COL_SPS = sum (total_NTM_COL_SPS),
            total_NTM_ECU_SPS = sum (total_NTM_ECU_SPS),
            total_NTM_PER_SPS = sum (total_NTM_PER_SPS))


merged_UNCTAD_NTM  <-merged_UNCTAD_NTM  %>% rename(PartnerISO3 =Partner)
# length(unique(merged_UNCTAD_NTM $PartnerISO3))


# get years from 2012 to 2016
merged_UNCTAD_NTM  <- subset(merged_UNCTAD_NTM , Year< 2017)

# need to get rid of WLD as partner:
table(merged_UNCTAD_NTM $PartnerISO3)



BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge.csv")

WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- c(unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]), 
                    unique(unctad_data$PartnerISO3[unctad_data$PartnerISO3 != "WLD"]))  # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'PartnerISO3' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
} #By hand at the bottom 

# Call the function with the provided dataframes
SPS_UNCTAD <- WLD_partner(BRA_trade_Merge, merged_UNCTAD_NTM )
names(SPS_UNCTAD)

#calculate counts of NTM by partners and HS code 
SPS_UNCTAD <- SPS_UNCTAD %>%  group_by(Year, PartnerISO3, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum(total_NTM_BRA_SPS, na.rm = TRUE),
            total_NTM_CHL_SPS = sum(total_NTM_CHL_SPS, na.rm = TRUE),
            total_NTM_BOL_SPS = sum(total_NTM_BOL_SPS, na.rm = TRUE),
            total_NTM_COL_SPS = sum(total_NTM_COL_SPS, na.rm = TRUE),
            total_NTM_ECU_SPS = sum(total_NTM_ECU_SPS, na.rm = TRUE),
            total_NTM_PER_SPS = sum(total_NTM_PER_SPS, na.rm = TRUE))


colSums(is.na(SPS_UNCTAD))
#  summary(BRA_UNCTAD_SPS_count$total_NTM_BRA_SPS)
#  # check if duplicates
#  duplicates <- BRA_UNCTAD_SPS_count[duplicated(BRA_UNCTAD_SPS_count), ]



################################################################################

# Multiply by weights 



Weighting_trade <-  read_csv("BRA/IVs/Weighting_trade.csv")
names(Weighting_trade)
names(merged_UNCTAD_NTM )
class(Weighting_trade$H6)


class(merged_UNCTAD_NTM$HSCode)
weighted_NTM  <- left_join(merged_UNCTAD_NTM , Weighting_trade, join_by(PartnerISO3 == PartnerISO3, HSCode == H6))
colSums(is.na(weighted_NTM))





weighted_NTM  <- weighted_NTM %>% mutate()



################################################################################






