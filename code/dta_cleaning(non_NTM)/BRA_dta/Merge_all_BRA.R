
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

#for brazil price 
trade <- read_csv("/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_price.csv")
#for quantity 
trade <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/clean_WITS_trade.csv")
Gravity <- read_csv("country_char/clean_Gravity.csv")
rta <- read_csv("country_char/clean_rta.csv")
landlock <- read_csv("country_char/clean_landlock.csv")
gdp <- read_csv("country_char/clean_gdp.csv")





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

# a) Some isocodes to change in the trade WITS data 

# some IScodes in the WITS are not well updated so cannot match with the other data
# so need to change and update them 
#old -> new
# ROM -> ROU
# SER -> SRB
# ZAR -> COD
# SUD -> SDN
# MNT -> MNE


trade <- trade %>%
  mutate(PartnerISO3 = case_when(
    PartnerISO3 == "SER" ~ "SRB",
    PartnerISO3 == "ROM" ~ "ROU",
    PartnerISO3 == "ZAR" ~ "COD",
    PartnerISO3 == "SUD" ~ "SDN",
    PartnerISO3 == "MNT" ~ "MNE",
    TRUE ~ PartnerISO3  # Keep original value if none of the above conditions are met
  ))


# b) Rebalance data (for quanitty based)
names(trade)
length(unique(trade$Year))
length(unique(trade$ProductCode))
length(unique(trade$PartnerISO3))
table((trade$PartnerISO3))
table(trade$Nomenclature)

complete_grid <- expand.grid(
  PartnerISO3 = unique(trade$PartnerISO3),
  ProductCode = unique(trade$ProductCode),
  Year = unique(trade$Year)
)
trade1 <- complete_grid %>%  left_join(trade, by = c("PartnerISO3", "ProductCode",
                                                     "Year" ))

# for NA susbtitute
trade1 <- trade1 %>%
  mutate(
    ReporterISO3 = ifelse(is.na(ReporterISO3), unique(ReporterISO3[!is.na(ReporterISO3)]), ReporterISO3),
    Nomenclature = ifelse(is.na(Nomenclature), unique(Nomenclature[!is.na(Nomenclature)]), Nomenclature),
    ReporterName = ifelse(is.na(ReporterName), unique(ReporterName[!is.na(ReporterName)]), ReporterName)
  )


trade1 <- trade1%>%  mutate(Quantity = ifelse(is.na(Quantity), 0, Quantity))
trade1 <- trade1%>%  mutate(TradeValue_1000_USD = ifelse(is.na(TradeValue_1000_USD), 0, TradeValue_1000_USD))

colSums(is.na(trade1))

names(trade1)

# rename some of te variables
trade1 <- trade1 %>%
  rename(
    imp_TradeValue_1000_USD = TradeValue_1000_USD,
    imp_Quantity = Quantity) %>%
  select(-TradeFlowName, -TradeFlowCode)

trade1 <- trade

################################################################################

# 3) Merge the data (between trade and the rest of the dta)

Merge <- left_join(trade1,Gravity2, relationship = "many-to-many")
colSums(is.na(Merge))
check_NA <- Merge %>% filter(!complete.cases(select(., -PartnerName)))
table(check_NA$PartnerISO3)
table(check_NA$PartnerName, check_NA$PartnerISO3)


Merge <- left_join(Merge,rta, relationship = "many-to-many")
Merge <- left_join(Merge,landlock, relationship = "many-to-many")
colSums(is.na(Merge))


# if missing drop them as often (island and all...)
Merge2 <- Merge %>%
  filter(complete.cases(select(., -PartnerName)))
colSums(is.na(Merge2))

###############################################################################


# 4) GDP values to match with trade  

#create a GDP variable of partner and reporter
names(gdp)

gdp1 <- gdp %>% select(iso3c,Year,Gdp_current_val)

#merge for Reporter gdp
Merge3 <- left_join(Merge2, gdp1, by = c("ReporterISO3" = "iso3c", "Year" = "Year"))
Merge3 <- Merge3 %>% rename(Reporter_GDP = Gdp_current_val)

#merge for Partner gdp
Merge3 <- left_join(Merge3, gdp1, by = c("PartnerISO3" = "iso3c", "Year" = "Year"))
Merge3 <- Merge3 %>% rename(Partner_GDP = Gdp_current_val)

colSums(is.na(Merge3))
check_NA <- Merge3 %>% filter(!complete.cases(select(., -PartnerName)))
table(check_NA$PartnerISO3)

################################################################################


#5) Drop unnecessary vars


names(Merge3)

Merge3 <- Merge3 %>% select(-country_id_o, -country_id_d, -country_o, -country_d )



################################################################################

# 6) export



write.csv(Merge3, "/data/sikeme/TRADE/WTO/data/BRA/country_char/BRA_merge.csv", row.names = FALSE)

Merge <- read_csv("country_char/BRA/BRA_merge.csv")

################################################################################
# 6) join it with tariff data  (for applie and MFN tariffs)

tariff <- read_csv("BRA/clean_BRA_tariff.csv")
# or AHS
tariff <- read_csv("/data/sikeme/TRADE/WTO/data/BRA/clean_BRA_tariff_AHS.csv")

names(tariff)
#select matching year to trade 
tariff <- subset(tariff, Year > 2011 & Year <2017 )

# if partner is world then need to change values 
WLD_countries <- unique(Merge$PartnerISO3[Merge$PartnerISO3 != "WLD"])
wld_rows <- filter(tariff, PartnerISO3 == "WLD")

# Create new observations for each country in 'WLD'
new_observations <- lapply(WLD_countries, function(country) {
  mutate(wld_rows, PartnerISO3 = country)
})

# Combine the new observations into a single dataframe
new_tariff <- bind_rows(new_observations)

# Join the new observations with the original 'tariff' dataframe
combined_tariff <- bind_rows(tariff %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where Partner is 'WLD'
                             new_tariff) 
names(combined_tariff)
duplicates <- combined_tariff[duplicated(combined_tariff), ]
combined_tariff <- distinct(combined_tariff) #drop duplicates


#Look fo NAs
colSums(is.na(combined_tariff))
combined_tariff <- combined_tariff %>% filter(!is.na(AHS_tariff))

duplicates <- combined_tariff %>%   group_by(ReporterISO3, ProductCode, PartnerISO3, Year) %>%
  filter(n() > 1)

# merge it with Merge
Merge1 <- left_join(Merge,combined_tariff)
colSums(is.na(Merge1))



################################################################################

# get applied tariffs instead 

tariff3 <-  read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/tariff/tariff_clean3.csv")
table(Merge$Year)
table(tariff3$Year)
names(Merge)

tariff4 <- tariff3 %>% filter(Year > 2011 & Year <2017) %>% select(Year,ProductCode,
                                                                   PartnerISO3, tariff_BRA)
tariff4 <- tariff4 %>% filter(tariff_BRA !=0)
table(tariff4$tariff_BRA)
names(tariff4)

# check for duplicates:
duplicates <- tariff4[duplicated(tariff4[, c("ProductCode", "PartnerISO3", "Year")]) | 
                        duplicated(tariff4[, c("ProductCode", "PartnerISO3", "Year")], fromLast = TRUE), ]
tariff4 <- tariff4[!duplicated(tariff4[, c("ProductCode", "PartnerISO3", "Year","tariff_BRA")]), ]

Merge1 <- left_join(Merge3, tariff4)
colSums(is.na(Merge1))


# if the tariff rate isNA then put 0 instead
Merge1$tariff_BRA[is.na(Merge1$tariff_BRA)] <- 0

################################################################################
# Save the data 

write.csv(Merge1, "/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_Merge2.csv", row.names = FALSE)
Merge <- read_csv("/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_Merge.csv")
table(Merge$Year)
names(Merge)
