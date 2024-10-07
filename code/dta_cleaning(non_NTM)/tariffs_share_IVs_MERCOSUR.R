

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
# if values NULL then put 0 too
tariff3 <- tariff3 %>%
  mutate(across(c(tariff_BRA, tariff_ARG, tariff_BOL, tariff_CHL, tariff_COL,
                  tariff_ECU, tariff_MEX, tariff_PRY, tariff_PER, tariff_URY),
                ~ ifelse(. == "NULL", 0, .)))

names(tariff3)
table(tariff3$tariff_BOL)


# c) Rename and select variables:

#   # the instruments we use are the following countries: "BRA", "CHL","BOL", "COL","ECU", "PER"
#   # select countries of interest:
#   tariff3 <- tariff3 %>% select(Partner,Year, ProductCode, PartnerISO3, tariff_BRA, 
#                                 tariff_CHL, tariff_BOL,tariff_COL,  tariff_ECU, tariff_PER)
#   unique(nchar(tariff3$ProductCode))


#  d) Get HS levels variables

tariff3$H1<- substring(tariff3$ProductCode, first = 1, last = 1)
tariff3$H2<- substring(tariff3$ProductCode, first = 1, last = 2)
tariff3$H3<- substring(tariff3$ProductCode, first = 1, last = 3)
tariff3$H4<- substring(tariff3$ProductCode, first = 1, last = 4)

################################################################################

# 5) export the data


write.csv(tariff3, "dta_WITS/tariff/tariff_clean3.csv")


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



# b) select country of interest and convert Product code in character
south_american_iso3 <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "PRY", 
                         "PER", "URY", "VEN") # VEN is not in the data 
trade_share <- trade_share %>% filter(ReporterISO3 %in% south_american_iso3)
# covert the ProductCode variable in character: 
class(trade_share$ProductCode)
trade_share$ProductCode <- as.character(trade_share$ProductCode)
trade_share$ProductCode <- stri_pad_left(trade_share$ProductCode,6,0)
num_characters <- nchar(trade_share$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)


###################################################################################


# Import share tariff data structuring


###################################################################################

# a) need to change data structure to match it with tariff data: import share
import_share <- trade_share %>% select (-export_share) %>%
  pivot_wider(names_from = ReporterISO3,                       # Create new columns from ReporterISO3
    values_from = c(import_share),     # Specify the values to spread
    names_glue = "{.value}_{ReporterISO3}" )          # Create new column names
colSums(is.na(import_share)) #drop NAs and replace it with 0
import_share[is.na(import_share)] <- 0

# need to match it with tariff data 
names(tariff3)
table(tariff3$Year)
tariff3 <- tariff3 %>% select(-tariff_MEX) %>% filter(Year>2011 &Year<2017)


# harmonize vars class 
tariff3$ProductCode <- as.character(tariff3$ProductCode)
tariff3$ProductCode <- stri_pad_left(tariff3$ProductCode,6,0)
num_characters <- nchar(tariff3$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)


class(tariff3$ProductCode)
class(import_share$ProductCode)


tariff4 <- left_join(tariff3, import_share, by = c("Year", "ProductCode")) %>%
  arrange(Year, ProductCode) %>% select(-Nomenclature)
colSums(is.na(tariff4))
test <- tariff4 %>%  filter(is.na(import_share_ARG))# very specific to HS codes and year
# put 0 instead of NAs
tariff4[is.na(tariff4)] <- 0


# now multiply each tariffs and trade shares 
names(tariff4)
suffixes <- c("ARG", "BRA", "BOL", "CHL", "COL", "ECU", "PRY", "PER", "URY")
tariff5 <- tariff4
# Create new variables in the data frame using lapply and mutate
for (suffix in suffixes) {
  tariff5 <- tariff5 %>%
    mutate(!!sym(paste0("share_tariff_", suffix)) := !!sym(paste0("tariff_", suffix)) * !!sym(paste0("import_share_", suffix)))
}
names(tariff5)
# only keep vars of interets:
vars_to_keep <- c("Partner", "Year", "ProductCode", "PartnerISO3", "H1", 
                  "H2", "H3", "H4", paste0("share_tariff_", suffixes))

# Select only the desired variables
import_tariff <- tariff5 %>%  select(all_of(vars_to_keep))

write.csv(import_tariff,"/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/import_tariff.csv" )



################################################################################






###################################################################################


# Export share tariff data sctructuring


###################################################################################

# exporter share in world market times tariff of the import


names(trade_share)
names(tariff3)

# a) need to change data structure to match it with tariff data: import share
export_share <- trade_share %>% select (-import_share, -Nomenclature) %>% rename(PartnerISO3 = ReporterISO3)
colSums(is.na(export_share)) #drop NAs and replace it with 0
export_share[is.na(export_share)] <- 0
names(export_share)

# need to match it with tariff data 
names(tariff3)
table(tariff3$Year)
tariff3 <- tariff3 %>% select(-tariff_MEX) %>% filter(Year>2011 & Year<2017)


tariff4 <- left_join(tariff3, export_share, by = c("Year", "ProductCode", "PartnerISO3")) %>%
  arrange(Year, ProductCode, PartnerISO3) 
colSums(is.na(tariff4))
# put 0 instead of NAs
tariff4[is.na(tariff4)] <- 0


# now multiply each tariffs and trade shares 
names(tariff4)
suffixes <- c("ARG", "BRA", "BOL", "CHL", "COL", "ECU", "PRY", "PER", "URY")
tariff5 <- tariff4
# Create new variables in the data frame using lapply and mutate
for (suffix in suffixes) {
  tariff5 <- tariff5 %>%
    mutate(!!sym(paste0("share_tariff_", suffix)) := !!sym(paste0("tariff_", suffix)) * !!sym(paste0("export_share_", suffix)))
}
names(tariff5)
# only keep vars of interets:
vars_to_keep <- c("Partner", "Year", "ProductCode", "PartnerISO3", "H1", 
                  "H2", "H3", "H4", paste0("share_tariff_", suffixes))

# Select only the desired variables
import_tariff <- tariff5 %>%  select(all_of(vars_to_keep))

write.csv(import_tariff,"/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/import_tariff.csv" )

################################################################################



