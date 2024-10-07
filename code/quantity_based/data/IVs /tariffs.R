
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


tariff <- fread("dta_WITS/tariff/DataJobID-2696600_2696600_AmandaCountryman (1).csv")
tariff1 <- tariff

names(tariff1)
table(tariff1$'Trade Year')
table(tariff1$'Trade Year', tariff1$`Native Nomen`)


tariffs_2011 <- fread("dta_WITS/tariff/DataJobID1.csv")
tariffs_2012 <- fread("dta_WITS/tariff/DataJobID2.csv")
tariffs_2017 <- fread("dta_WITS/tariff/DataJobID3.csv")

table(tariffs_2011$`Trade Year`)
table(tariffs_2012$`Trade Year`)
table(tariffs_2017$`Trade Year`)

names(tariffs_2011)
table(tariffs_2011$`Native Nomen`)
table(tariffs_2012$`Native Nomen`)
table(tariffs_2012$`Native Nomen`, tariffs_2012$`Trade Year`)
table(tariffs_2017$`Native Nomen`)


################################################################################


##############################################################################
# 

# Select applied tariffs

names(tariff1)
# select variables of interest
tariff1 <- tariff1 %>% filter(DutyType == "AHS") %>%
  select(-c(Reporter, Partner, Product))

# Rename columns
tariff1 <- tariff1 %>% rename(
  Year = `Trade Year`,
  tariff_AHS = `Simple Average`,
  Reporter = `Reporter Name`, 
  Partner = `Partner Name`,
  ProductCode = Product_HS4) %>%
  select(Reporter, Partner, Year, ProductCode, tariff_AHS)


tariff1$PartnerISO3 <- countrycode(tariff1$Partner, origin = "country.name", 
                                   destination = "iso3c")
tariff1 <- tariff1 %>% mutate(PartnerISO3 = if_else(Partner== "World","WLD", PartnerISO3))

tariff1$ReporterISO3 <- countrycode(tariff1$Reporter, origin = "country.name", 
                                    destination = "iso3c")
table(tariff1$ReporterISO3)

colSums(is.na(tariff1))

write.csv(tariff1, "dta_WITS/tariff/tariff_clean1.csv")

rm(list=ls())
tariff1 <-  read_csv("dta_WITS/tariff/tariff_clean1.csv")

#check the NAs
na_values <- tariff1 %>% filter(is.na(PartnerISO3)) %>%
  select(Partner)
unique(tariff1$PartnerISO3)

na_values <- tariff1 %>% filter(is.na(ProductCode)) %>%
  select(Year)
unique(tariff1$Year)
###############################################################################

# # Pivot wider 

names(tariff1)

tariff2 <- tariff1 %>% select(-Reporter) %>%
  pivot_wider(names_from = ReporterISO3, values_from = tariff_AHS, names_prefix = "tariff_")
names(tariff2)

################################################################################















tariff3 <-  read_csv("dta_WITS/tariff/tariff_clean3.csv")
names(tariff3)
table(tariff3$Partner)
table(tariff3$Partner)
summary(tariff3)
length(unique(tariff3$ProductCode))










# do the mean of tariffs as IVs



# try to do the mean
# Filter rows where all specified columns are different from 0
tariff4 <- tariff3 %>%
  filter(!(tariff_BRA == 0 & tariff_CHL == 0 & tariff_BOL == 0 & tariff_COL== 0 & tariff_ECU== 0 
           & tariff_PER == 0))
summary(tariff4)













