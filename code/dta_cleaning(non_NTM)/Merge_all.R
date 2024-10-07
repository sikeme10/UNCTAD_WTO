
################################################################################


                          # data MERGE #




################################################################################
# We merge all trade and country bilateral characteristics 



################################################################################
# Load all data 


rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/WTO/data/")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)
library(data.table)


# for quant based:
trade <- fread("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/clean_WITS_trade_quant.csv")
# for price based
trade <- fread("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/clean_WITS_trade_price.csv")


#Other gravity variables:
Gravity <- read_csv("country_char/clean_Gravity.csv")
rta <- read_csv("country_char/clean_rta.csv")
landlock <- read_csv("country_char/clean_landlock.csv")
gdp <- read_csv("country_char/clean_gdp.csv")

class(trade$ProductCode)



################################################################################

# 1) harmonize all data 

names(trade)
names(Gravity)
names(rta)
names(landlock)
names(gdp)
# a) Rename variable 

Gravity <- Gravity %>% rename(ReporterISO3 = iso3_o,
                              PartnerISO3 = iso3_d,
                              Year = year)

rta <- rta %>% rename(ReporterISO3 = importer,
                      PartnerISO3 = exporter,
                      Year = year)

landlock <- landlock %>% rename(ReporterISO3 = iso3_o,
                                PartnerISO3 = iso3_d,
                                landlocked_rep = landlocked_o,
                                landlocked_par = landlocked_d,
                                Year = year)

gdp <- gdp %>% rename(Year = year)


# b)  all same year 
table(Gravity$Year)
table(rta$Year)
table(trade$Year)
rta <- rta %>% filter(Year <2020 & Year >2011)
Gravity <- Gravity %>%  filter(Year <2020 & Year >2011)
landlock <- landlock %>% filter(Year <2020 & Year >2011)
gdp <- gdp %>% filter(Year <2020 & Year >2011)

# c) check for NAs

colSums(is.na(Gravity))
colSums(is.na(rta))
colSums(is.na(landlock))

# if missing take the value at another year 
# (distance between countries and contiguous not supposed to change with time)
Gravity2 <- Gravity %>%  group_by(ReporterISO3, PartnerISO3) %>%
  arrange(Year) %>% fill(dist, contig, comlang_off, .direction = "downup") %>%  # Fills both down and up
  ungroup()
colSums(is.na(Gravity2))


################################################################################
# 2) restructure trade data 


# select country reports of interest to make the data smaller 
# because it is heavy can select the reoprter of interest 
table(trade$ReporterISO3)

# choose country of interest
south_american_iso3 <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY", 
                         "PER", "URY", "VEN")

trade1 <- trade %>% filter(ReporterISO3 %in% south_american_iso3)
table(trade1$PartnerISO3)
table(trade1$ReporterISO3)

trade <- trade1

rm(trade1)

# a) Some isocodes to change in the trade WITS data 

# some IScodes in the WITS are not well updated so cannot match with the other data
# so need to change and update them 
#old -> new
# ROM -> ROU
# SER -> SRB
# ZAR -> COD
# SUD -> SDN
# MNT -> MNE

table(trade$PartnerISO3)
names(Gravity2)

# match with country name 
CountryList <- read_csv("dta_WITS/trade/CountryList.csv")
CountryList <- CountryList %>% rename(Country_name = 'Country Name')
trade <- left_join(trade, CountryList, by = c("PartnerISO3" = "Country ISO3"))
colSums(is.na(trade))
test <- trade %>% filter(is.na(Country_name))

table(test$PartnerISO3)

# check if name of partner isocode match or not
trade_partner <- unique(trade$PartnerISO3)
gravity_partner <- unique(Gravity2$PartnerISO3)
# Find values in trade that are not in gravity
not_in_gravity_dta <- trade_partner[!trade_partner %in% gravity_partner]
not_in_gravity_dta
countries <- trade %>% filter(PartnerISO3 %in% not_in_gravity_dta) %>% select(PartnerISO3, Country_name)

table(countries$PartnerISO3, countries$Country_name)

# for partner
trade <- trade %>%
  mutate(PartnerISO3 = case_when(
    PartnerISO3 == "SER" ~ "SRB",
    PartnerISO3 == "ROM" ~ "ROU",
    PartnerISO3 == "ZAR" ~ "COD",
    PartnerISO3 == "SUD" ~ "SDN",
    PartnerISO3 == "MNT" ~ "MNE",
    TRUE ~ PartnerISO3  # Keep original value if none of the above conditions are met
  ))

# for reporter
trade <- trade %>%
  mutate(ReporterISO3 = case_when(
    ReporterISO3 == "SER" ~ "SRB",
    ReporterISO3 == "ROM" ~ "ROU",
    ReporterISO3 == "ZAR" ~ "COD",
    ReporterISO3 == "SUD" ~ "SDN",
    ReporterISO3 == "MNT" ~ "MNE",
    TRUE ~ ReporterISO3  # Keep original value if none of the above conditions are met
  ))




# drop un necessary partner countries countries (island, antartica mostly)
trade <- trade %>% filter(!PartnerISO3 %in% not_in_gravity_dta)
trade <- trade %>% filter(ReporterISO3 != "EUN")


# b) Rebalance data 
names(trade)
length(unique(trade$Year))
length(unique(trade$ProductCode))
length(unique(trade$PartnerISO3))
length(unique(trade$ReporterISO3))
table((trade$PartnerISO3))


complete_grid <- expand.grid(
  ReporterISO3 = unique(trade$ReporterISO3),
  PartnerISO3 = unique(trade$PartnerISO3),
  ProductCode = unique(trade$ProductCode),
  Year = unique(trade$Year)
)
trade1 <- complete_grid %>%  left_join(trade, by = c("ReporterISO3",
                                                     "PartnerISO3", "ProductCode",
                                                     "Year" ))
colSums(is.na(trade1))


#drop NA cretaed and put 0 instead
trade1 <- trade1%>%  mutate(Quantity = ifelse(is.na(Quantity), 0, Quantity))
trade1 <- trade1%>%  mutate(TradeValue = ifelse(is.na(TradeValue), 0, TradeValue))

colSums(is.na(trade1))

names(trade1)

# rename some of te variables
trade1 <- trade1 %>%
  rename(
    imp_TradeValue = TradeValue,
    imp_Quantity = Quantity) %>%
  select( -TradeFlow)

# drop if importer = exporter
trade2 <- trade1 %>% filter(ReporterISO3 != PartnerISO3)

# separate by year if to much 
trade_2012 <- trade2 %>% filter(Year == 2012)
Gravity2_2012 <- Gravity2 %>% filter(Year == 2012)

################################################################################

# 3) Merge the data (between trade and the rest of the dta)

#when merging need to be careful, some creates NAs

Merge <- left_join(trade2,Gravity2, relationship = "many-to-many")
colSums(is.na(Merge))
check_NA <- Merge %>% filter(!complete.cases(select(., contig ,dist,comlang_off)))
table(check_NA$PartnerISO3)
Merge$comlang_off[is.na(Merge$comlang_off)] <- 0 # we get comlang_off with a lot of NA (after checking manually can just put 0)
table(check_NA$PartnerName, check_NA$PartnerISO3)



Merge <- left_join(Merge,rta, relationship = "many-to-many")
Merge <- left_join(Merge,landlock, relationship = "many-to-many")
colSums(is.na(Merge))


# if missing drop them as often (island and all...)
Merge2 <- Merge %>%  filter(complete.cases(select(., -PartnerName, -QuantityToken)))
colSums(is.na(Merge2))


write.csv(Merge, "/data/sikeme/TRADE/WTO/data/quant_based/merge1.csv", row.names = FALSE)


###############################################################################

rm(list=ls())
library(data.table)
Merge <- fread("/data/sikeme/TRADE/WTO/data/quant_based/merge1.csv")

names(Merge)
# because it is heavy can select the reoprter of interest 
table(Merge$ReporterISO3)


###############################################################################


# 4) GDP values to match with trade  

#create a GDP variable of partner and reporter
names(gdp)
names(Merge2)


gdp1 <- gdp %>% select(iso3c,Year,Gdp_current_val)
table(gdp1$iso3c)

#merge for Reporter gdp
Merge3 <- left_join(Merge, gdp1, by = c("ReporterISO3" = "iso3c", "Year" = "Year"))
Merge3 <- Merge3 %>% rename(Reporter_GDP = Gdp_current_val)

#merge for Partner gdp
Merge3 <- left_join(Merge3, gdp1, by = c("PartnerISO3" = "iso3c", "Year" = "Year"))
Merge3 <- Merge3 %>% rename(Partner_GDP = Gdp_current_val)

colSums(is.na(Merge3))
check_NA <- Merge3 %>% filter(!complete.cases(select(., - QtyToken, -Country_name )))
table(check_NA$PartnerISO3)



# a lot of the missing GDPs are islands (that we can drop)
# only two countries don't have complete GDP values : VEN and SSD (south sudan)
# we can drop all the islands and fill for VEN the GDP of previous years 


# a lot of the missing GDPs are islands (that we can drop)
# only two countries don't have complete GDP values : VEN and SSD (south sudan)
# if missing take the value at another year 

Merge4 <- Merge3 %>% arrange(PartnerISO3, Year) %>%            # Arrange by PartnerISO3 and Year
  group_by(PartnerISO3) %>%                 # Group by PartnerISO3
  fill(Partner_GDP, .direction = "downup") %>%  # Fill NA values, first down then up
  ungroup()     
#And drop the countries that have NAs
check_NA <- Merge4 %>% filter(!complete.cases(select(., - QtyToken, -Country_name )))
table(check_NA$PartnerISO3)
countries_to_drop <- unique(check_NA$PartnerISO3)
Merge4 <- Merge4 %>% filter(!PartnerISO3 %in% countries_to_drop ) #drop the list of countries


colSums(is.na(Merge3))
colSums(is.na(Merge4))

################################################################################


#5) Drop unnecessary vars


names(Merge4)

Merge4 <- Merge4 %>% select(-country_id_o, -country_id_d, -country_o, -country_d )



################################################################################

# Get HS 6 codes in characters;


names(Merge4)

class(Merge4$ProductCode)
length(unique(Merge4$ProductCode))


# make the variable in character: 
Merge4$ProductCode <- as.character(Merge4$ProductCode)
Merge4$ProductCode <- stri_pad_left(Merge4$ProductCode,6,0)
num_characters <- nchar(Merge4$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)





################################################################################

# 6) export



write.csv(Merge3, "/data/sikeme/TRADE/WTO/data/BRA/country_char/BRA_merge.csv", row.names = FALSE)

write.csv(Merge4, "/data/sikeme/TRADE/WTO/data/quant_based/merge2.csv", row.names = FALSE)

Merge4 <- fread("/data/sikeme/TRADE/WTO/data/quant_based/merge2.csv")
class(Merge3$Pro)





