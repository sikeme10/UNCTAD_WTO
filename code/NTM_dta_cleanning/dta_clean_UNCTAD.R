
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
# STEPS of the code

#1) Load the data 

#2) select year of the data, and aggregate NTM type info
#3) select the country of interest, or by type of NTM
#4) HS codes in characters
# 5) look at withdrawal




################################################################################

#1) load the data 

################################################################################

library(readr)
dta <- read_csv("/data/sikeme/TRADE/WTO/data/dta_UNCTAD/NTM_hs6_2010_2022_H4V12.csv")
dta <- fread("dta_UNCTAD/NTM_hs6_2010_2022_H4V12.csv")

#checks
names(dta)
table(dta$Year)
table(dta$ntm_all)
names(dta)
table(dta$Reporter)
table(dta$NTMCode)

################################################################################

#2) Select data 

################################################################################

# get the data from 2010 to 2019 
dta <- subset(dta, Year<2020)
# get the type of NTM (first letter)
dta$NTM <- substr(dta$NTMCode, 1, 1)
table(dta$NTM)


#CHECKS
#country names:
table(dta$Reporter)

# number of countries
country_counts <- dta %>%
  group_by(Year) %>%
  summarise(num_countries = n_distinct(Reporter), reporters = list(unique(Reporter)))
country_counts

# names of countries per year 
summary_table <- dta %>%
  group_by(Year) %>%
  summarize(Country = paste(Reporter, collapse = ", ")) %>%
  ungroup()

# observation per year for EU:
observations_per_year <- dta %>%
  filter(Reporter == "EUN") %>%
  count(Year)



################################################################################

#3) create subset of data for the selected country 

################################################################################

dta_BRA <- subset(dta, Reporter == "BRA")

#for SPS specific: (class A)
dta_EUN <- subset(dta_EUN, NTM == "A")

write.csv(dta_EUN, "dta_EUN_SPS.csv", row.names = FALSE)


################################################################################
# export by type of NTMs

#for only SPS 
dta_SPS<- subset(dta, NTM == "A")
write.csv(dta_SPS, "dta_UNCTAD_SPS.csv", row.names = FALSE)

#for only TBT 
dta_TBT<- subset(dta, NTM == "B")
write.csv(dta_TBT, "dta_UNCTAD_TBT.csv", row.names = FALSE)




################################################################################

# 4) HS code in character

################################################################################

dta_UNCTAD_SPS <- read_csv("dta_UNCTAD/dta_UNCTAD_SPS.csv")
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD/dta_UNCTAD_TBT.csv")

# the data is at HS 2012 revision, so need to change some of the HS codes

# make the variable in character: 
dta_UNCTAD_SPS$HSCode <- stri_pad_left(dta_UNCTAD_SPS$HSCode,6,0)
num_characters <- nchar(dta_UNCTAD_SPS$HSCode )
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)



write.csv(dta_UNCTAD_SPS, "dta_UNCTAD/dta_UNCTAD_SPS.csv", row.names = FALSE)

#for TBT:
dta_UNCTAD_TBT$HSCode <- stri_pad_left(dta_UNCTAD_TBT$HSCode,6,0)
num_characters <- nchar(dta_UNCTAD_TBT$HSCode )
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)

write.csv(dta_UNCTAD_TBT, "dta_UNCTAD/dta_UNCTAD_TBT.csv", row.names = FALSE)





################################################################################

# 5) Aggregate at SPS and TBT level, to get total count 

################################################################################


# For SPS, do the total count of NTM aggregated at the SPS level
dta_UNCTAD_SPS1 <- dta_UNCTAD_SPS %>% group_by(Year, Reporter, Partner, HSCode) %>% 
  summarise(SPS_count = sum(ntm_all, na.rm = TRUE) )


table(dta_UNCTAD_SPS$NTMCode)
table(dta_UNCTAD_SPS1$SPS_count)
sum(dta_UNCTAD_SPS1$SPS_count, na.rm = TRUE)



# For TBT 
dta_UNCTAD_TBT1 <- dta_UNCTAD_TBT %>% group_by(Year, Reporter, Partner, HSCode) %>% 
  summarise(TBT_count = sum(ntm_all, na.rm = TRUE) )


table(dta_UNCTAD_TBT$NTMCode)
table(dta_UNCTAD_TBT1$TBT_count)
sum(dta_UNCTAD_TBT1$TBT_count, na.rm = TRUE)



################################################################################
# Export:


write.csv(dta_UNCTAD_SPS1, "dta_UNCTAD/clean_dta_UNCTAD_SPS.csv", row.names = FALSE)
write.csv(dta_UNCTAD_TBT1, "dta_UNCTAD/clean_dta_UNCTAD_TBT.csv", row.names = FALSE)




