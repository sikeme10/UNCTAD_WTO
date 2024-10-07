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


################################################################################
# Load the trade data 

library(readr)
BOL <- read_csv("dta_WITS/Bolivia_2011_2019.csv")
CHL <- read_csv("dta_WITS/Chile_2011_2019.csv")
COL <- read_csv("dta_WITS/Colombia_2011_2019.csv")
ECD <- read_csv("dta_WITS/Ecuador_2011_2019.csv")
PER <- read_csv("dta_WITS/Peru_2011_2019.csv")
BRA <- read_csv("dta_WITS/BRA_2012_2016.csv")

#and NTM data;
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")


################################################################################


# weight function: choose the HS level of aggregation 

weighting <- function(data, hs_level) {
  # Create the HS level columns dynamically
  for (i in 1:6) {
    col_name <- paste0("H", i)
    data[[col_name]] <- substring(data$ProductCode, first = 1, last = i)
  }
  # Rename the TradeValue column
  data <- data %>%
    rename(TradeValue_1000_USD = `TradeValue in 1000 USD`)
  # Filter the data for the year 2012 and "Gross Imp."
  data_2012 <- data %>%
    filter(Year == 2012 & TradeFlowName == "Gross Imp.")
  
  # Dynamically select the HS level column for grouping
  hs_col <- paste0("H", hs_level)
  
  # Group by the specified HS level and summarize the trade values
  data_2012 <- data_2012 %>%
    group_by(Year, ReporterISO3, PartnerISO3, !!sym(hs_col)) %>%
    summarize(TradeValue_1000_USD_HS = sum(TradeValue_1000_USD), .groups = 'drop')
  
  # Calculate the total import values for each HS level
  sum_imports <- data_2012 %>%
    group_by(!!sym(hs_col)) %>%
    summarize(tot_imp_val = sum(TradeValue_1000_USD_HS), .groups = 'drop')
  # Merge the total import values back with the original data
  data_2012 <- left_join(data_2012, sum_imports, by = hs_col)
  
  # Calculate the weight
  data_2012 <- data_2012 %>%
    mutate(weight = TradeValue_1000_USD_HS / tot_imp_val)
  return(data_2012)
}


BOL_weights <- weighting(BOL, 6)
CHL_weights <- weighting(CHL, 6)
COL_weights <- weighting(COL, 6)
ECD_weights <- weighting(ECD, 6)
PER_weights <- weighting(PER, 6)
BRA_weights <- weighting(BRA, 6)

names(BRA_weights)

BOL_weights <-  BOL_weights %>%  select(PartnerISO3, H6, weight) %>% rename(BOL_weights = weight)
CHL_weights <-  CHL_weights %>%  select(PartnerISO3, H6, weight) %>% rename(CHL_weights = weight)
COL_weights <-  COL_weights %>%  select(PartnerISO3, H6, weight) %>% rename(COL_weights = weight)
ECD_weights <-  ECD_weights %>%  select(PartnerISO3, H6, weight) %>% rename(ECD_weights = weight)
PER_weights <-  PER_weights %>%  select(PartnerISO3, H6, weight) %>% rename(PER_weights = weight)
BRA_weights <-  BRA_weights %>%  select(PartnerISO3, H6, weight) %>% rename(BRA_weights = weight)


#Weighting_trade <- full_join(BOL_weights,CHL_weights)
# merge all of them together
Weighting_trade <- BOL_weights %>%
  full_join(CHL_weights, by = c("PartnerISO3", "H6")) %>%
  full_join(COL_weights, by = c("PartnerISO3", "H6")) %>%
  full_join(ECD_weights, by = c("PartnerISO3", "H6")) %>%
  full_join(PER_weights, by = c("PartnerISO3", "H6")) %>%
  full_join(BRA_weights, by = c("PartnerISO3", "H6"))
colSums(is.na(Weighting_trade))

Weighting_trade <- Weighting_trade %>% mutate_all(~replace_na(., 0))

write.csv(Weighting_trade, "BRA/IVs/Weighting_trade.csv")

s#####################################################################
#   
#   # by hand:
names(ECD)
  ECD$H1<- substring(ECD$ProductCode, first = 1, last = 1)
  ECD$H2<- substring(ECD$ProductCode, first = 1, last = 2)
  ECD$H3<- substring(ECD$ProductCode, first = 1, last = 3)
  ECD$H4<- substring(ECD$ProductCode, first = 1, last = 4)
  ECD$H5<- substring(ECD$ProductCode, first = 1, last = 5)
  ECD <- ECD %>%
    rename(TradeValue_1000_USD = 'TradeValue in 1000 USD')
  #select year
  ECD_2012 <- ECD %>%
    filter(Year == 2012 & TradeFlowName == "Gross Imp.")
  #select HS level
  ECD_2012 <- ECD_2012 %>% group_by(Year, ReporterISO3, PartnerISO3, H5) %>%
    summarize (TradeValue_1000_USD_H5 = sum(TradeValue_1000_USD))
  #create the denominator sum of imports value from all country
  sum <- ECD_2012  %>%  group_by(H5) %>%
    summarize(tot_imp_val = sum(TradeValue_1000_USD_H5)) %>% ungroup()
  ECD_2012 <- left_join(ECD_2012,sum)
  #create the weight 
  ECD_2012$weight <- ECD_2012$TradeValue_1000_USD_H5 / ECD_2012$tot_imp_val

