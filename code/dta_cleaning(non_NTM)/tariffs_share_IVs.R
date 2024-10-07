

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
library(countrycode)
library(arrow)
###############################################################################


# Structure:

# 1) Load the data
# 2) Convert all HS codes into H4 nomenclature 
# 3) dta cleaning
# 4) Data structure
# 5) export the data
# load import and export share data to merge it with taruff data 


###############################################################################
# 1) Load the data

library(readr)
#most south american countries
tariff <- read_csv("dta_WITS/tariff/DataJobID-2696600_2696600_AmandaCountryman (1).csv")



# # forgot venezuela 
# tariff_VEN <- read_csv("dta_WITS/tariff/VENEZUELA_AmandaCountryman.csv")
# tariff <-rbind(tariff, tariff_VEN) 

table(tariff$`Reporter Name`)
names(tariff)



###############################################################################

# 2) Convert all HS codes into H4 nomenclature 
# we can use concord_hs {concordance} to find HS codes for other classification
library(concordance)

table(tariff$'Native Nomen')
# For HS3
HS_H3 <- tariff %>% filter(`Native Nomen` == "H3") %>% distinct(Product) %>%   rename(Product = Product)
length(unique(HS_H3$Product))
HS_H3$HS3_HS4 <-concord_hs(HS_H3$Product, origin = "HS3", destination = "HS4", dest.digit = 6)
length(unique(HS_H3$HS3_HS4 ))

HS_H4 <- tariff %>% filter(`Native Nomen` == "H4") %>% distinct(Product) %>% pull(Product)
length(unique(HS_H4))

# For HS5
HS_H5 <- tariff %>% filter(`Native Nomen` == "H5") %>% distinct(Product) %>%   rename(Product = Product)
length(unique(HS_H5$v))
HS_H5$HS5_HS4 <-concord_hs(HS_H5$Product, origin = "HS5", destination = "HS4", dest.digit = 6)
length(unique(HS_H5$HS5_HS4))

# join HS concordance
tariff1 <- left_join(tariff, HS_H3)
tariff1 <- left_join(tariff1, HS_H5)

#match with HS codes
tariff1 <- tariff1 %>% 
  mutate(Product_HS4 = case_when(
    `Native Nomen` == "H3" ~ HS3_HS4, 
    `Native Nomen` == "H4" ~ Product, 
    `Native Nomen` == "H5" ~ HS5_HS4))


# checks
length(unique(tariff1$Product_HS4))
colSums(is.na(tariff1))


##############################################################################

# 3) data cleaning



# a) Select applied tariffs

names(tariff1)
# select variables of interest
tariff1 <- tariff1 %>% filter(DutyType == "AHS") %>%
  select(-c(Reporter, Partner, Product))

# b) Rename columns
tariff1 <- tariff1 %>% rename(
    Year = `Trade Year`,
    tariff_AHS = `Simple Average`,
    Reporter = `Reporter Name`, 
    Partner = `Partner Name`,
    ProductCode = Product_HS4) %>%
  select(Reporter, Partner, Year, ProductCode, tariff_AHS)


# c) use Isocode for countries ot match with other dta
tariff1$PartnerISO3 <- countrycode(tariff1$Partner, origin = "country.name", 
                           destination = "iso3c")
#for WLD change it manually
tariff1 <- tariff1 %>% mutate(PartnerISO3 = if_else(Partner== "World","WLD", PartnerISO3))

tariff1$ReporterISO3 <- countrycode(tariff1$Reporter, origin = "country.name", 
                                   destination = "iso3c")
table(tariff1$ReporterISO3)

# check NA to see match of isocodes
colSums(is.na(tariff1))
check_NA <- tariff1 %>% filter(!complete.cases(select(., - tariff_AHS, -ProductCode)))
table(check_NA$Partner)
check <- tariff1 %>%  filter(PartnerISO3 == "ETH")

#pb with Ethiopia
tariff1 <- tariff1 %>%  mutate(PartnerISO3 = ifelse(Partner == "Ethiopia(excludes Eritrea)", "ETH", PartnerISO3))
colSums(is.na(tariff1))

# keep complete cases and drop NA observations
tariff2 <- tariff1 %>%  filter(complete.cases(select(., -tariff_AHS)))

# select year of interest:
tariff2 <- tariff2 %>% filter(Year>2011 &Year<2017)

###############################################################################
# export and checks:

write.csv(tariff2, "dta_WITS/tariff/tariff_clean1.csv")

rm(list=ls())
tariff1 <-  fread("dta_WITS/tariff/tariff_clean1.csv")
tariff1 <- tariff1 %>%   select(-1)
names(tariff1)


#check the NAs
na_values <- tariff1 %>% filter(is.na(PartnerISO3)) %>%  select(Partner)
unique(tariff1$PartnerISO3)
na_values <- tariff1 %>% filter(is.na(ProductCode)) %>%  select(Year)
unique(tariff1$Year)
unique(tariff1$ReporterISO3)

###############################################################################

# 4) Data structure

#check uniqueness of observations:
duplicates <- tariff1 %>%
  group_by(Reporter, Partner, Year, ProductCode) %>%  filter(n() > 1) %>%  ungroup()

# drop duplicates
tariff1 <- tariff1 %>%
  distinct(Reporter, Partner, Year, ProductCode, PartnerISO3, ReporterISO3, .keep_all = TRUE)


# a) use pivot wider 

names(tariff1)
tariff2 <- tariff1 %>% select(-Reporter) %>%
  pivot_wider(names_from = ReporterISO3, values_from = tariff_AHS, names_prefix = "tariff_")
names(tariff2)


# b) take care of NAs and NUll

tariff3 <- tariff2 %>%
  mutate(across(c(tariff_BRA, tariff_ARG, tariff_BOL, tariff_CHL, tariff_COL,
                  tariff_ECU, tariff_MEX, tariff_PRY, tariff_PER, tariff_URY),
                ~ ifelse(is.na(.), 0, .)))
summary(tariff3)



colSums(is.na(tariff3))
names(tariff3)
table(tariff3$tariff_BOL)




# c) create mean value of tariff for instruments

# for each country create a data with only three neighboring countries and get the mean
# Define lists for each country with selected neighbors to create data for IVs
BRA <- c("tariff_BRA", "tariff_ARG", "tariff_PRY", "tariff_URY")
ARG <- c("tariff_ARG", "tariff_BRA", "tariff_CHL", "tariff_URY")
BOL <- c("tariff_BOL", "tariff_PER", "tariff_ARG", "tariff_BRA")
COL <- c("tariff_COL", "tariff_ECU", "tariff_PER", "tariff_BRA")
CHL <- c("tariff_CHL", "tariff_ARG", "tariff_PER", "tariff_BOL")
ECU <- c("tariff_ECU", "tariff_COL", "tariff_PER", "tariff_BRA")
PRY <- c("tariff_PRY", "tariff_BRA", "tariff_ARG", "tariff_URY")
URY <- c("tariff_URY", "tariff_ARG", "tariff_BRA", "tariff_PRY")
PER <- c("tariff_PER", "tariff_ECU", "tariff_BOL", "tariff_COL")


# List of country codes
country_list <- list(BRA = BRA, ARG = ARG, BOL = BOL, COL = COL, CHL = CHL, ECU = ECU, PRY = PRY, URY = URY, PER = PER)
country_data_list <- list()

# Loop over each country to create the data and calculate mean
for (country in names(country_list)) {
  # Create a temporary data frame with the selected country and its neighbors
  selected_columns <- country_list[[country]]
  # Create new variable for each country with neighboring country tariffs
  new_data <- tariff3 %>%
    select(Year, ProductCode, PartnerISO3, Partner, all_of(selected_columns)) %>%
    mutate(mean_tar_IV = rowMeans(select(., all_of(selected_columns[-1])), na.rm = TRUE)) %>%
    mutate(ReporterISO3 = country) %>%  # Assign the country code to ReporterISO3
    rename_with(~ "tariff", all_of(paste0("tariff_", country))) %>%  # Rename the specific country's tariff column to 'tariff'
    select(-starts_with("tariff_")) # Remove the remaining 'tariff_' columns
  # Assign the result to a new variable dynamically
  assign(paste0(country, "_tar"), new_data)
  country_data_list[[country]] = new_data
}

tariff4 <- bind_rows(country_data_list)
tariff4 <- tariff4 %>% arrange(Year,ReporterISO3, ProductCode,PartnerISO3 )
table(tariff4$ReporterISO3)



#  d) Get HS levels variables

tariff4$H1<- substring(tariff4$ProductCode, first = 1, last = 1)
tariff4$H2<- substring(tariff4$ProductCode, first = 1, last = 2)
tariff4$H3<- substring(tariff4$ProductCode, first = 1, last = 3)
tariff4$H4<- substring(tariff4$ProductCode, first = 1, last = 4)

################################################################################

# 5) export the data


write.csv(tariff4, "dta_WITS/tariff/tariff_clean3.csv")



################################################################################

# To load:
rm(list=ls())
tariff3 <-  read_csv("dta_WITS/tariff/tariff_clean3.csv")
names(tariff3)
tariff3 <- tariff3 %>%   select(-1)
tariff3 <- tariff3 %>% arrange(Partner, Year, ProductCode)


################################################################################


# 6) Trade share data: for import shares


################################################################################

# we need to vars: share_ni (importer share in world market) and share_nj 
# we then need to match it with the tariff data 

# a) load the data
trade_share <- fread( "dta_WITS/trade_share/clean_trade_share.csv")

names(trade_share)
table(trade_share$ReporterISO3)
length(unique(trade_share$ProductCode))
table(trade_share$Year)




# covert the ProductCode variable in character: for both data
class(trade_share$ProductCode)
trade_share$ProductCode <- as.character(trade_share$ProductCode)
trade_share$ProductCode <- stri_pad_left(trade_share$ProductCode,6,0)
num_characters <- nchar(trade_share$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)


class(tariff3$ProductCode)
tariff3$ProductCode <- as.character(tariff3$ProductCode)
tariff3$ProductCode <- stri_pad_left(tariff3$ProductCode,6,0)
num_characters <- nchar(tariff3$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)


###################################################################################


# Import share tariff data structuring


###################################################################################


# match import share with thre reporter 
names(trade_share)
names(tariff3)
class(tariff3$ProductCode)


# Reporter import share 
tariff4 <- left_join(tariff3, trade_share, by = c("Year", "ProductCode", "ReporterISO3")) %>%
  select(-export_share, -Nomenclature)
colSums(is.na(tariff4))
#if NA put 0
tariff4[is.na(tariff4)] <- 0

#Partner export share
exp_share <- trade_share %>% select(-import_share )
tariff5 <- left_join(tariff4, exp_share, by = c("Year", "ProductCode", "PartnerISO3" = "ReporterISO3")) %>%
  select(-Nomenclature)
colSums(is.na(tariff5))

NAs <- tariff5 %>% filter(is.na(export_share))
table(NAs$PartnerISO3)
tariff5[is.na(tariff5)] <- 0



class(tariff5$ProductCode)


###################################################################################

# create tariff importer/exporter shares 


names(tariff5)

tariff5 <- tariff5 %>%
  mutate(
    import_tariff = tariff * import_share,
    import_mean_tariff_IV = mean_tar_IV * import_share,
    export_tariff = tariff * export_share,
    export_mean_tariff_IV = mean_tar_IV * export_share
  )

colSums(is.na(tariff5))

###################################################################################


write.csv(tariff5,"/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/share_tariff.csv" )

test <- read_csv("/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/share_tariff.csv")
table(test$PartnerISO3)

