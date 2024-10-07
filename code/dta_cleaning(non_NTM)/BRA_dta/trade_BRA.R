

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/WTO/data")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)








###############################################################################
# 0) check the data 

trade <- read_csv("dta_WITS/BRA_2012_2016.csv")

trade<- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/Bra_import.csv")


colSums(is.na(trade))
names(trade)
table(trade$ReporterISO3)
length(unique(trade$ProductCode))
table(trade$Year)
length(unique(trade$ProductCode))
length(unique(trade$PartnerISO3))
length(unique(trade$TradeFlowCode))
table(trade$Nomenclature)


#rename some of the variable 
trade <- trade %>% rename("TradeValue_1000_USD" = "TradeValue in 1000 USD")




################################################################################


# 1) create price variable 

# If interested in price based 
################################################################################

#drop 0 values for trade value
trade <- trade[trade$TradeValue_1000_USD != 0, ]
summary(trade$TradeValue_1000_USD)
# Calculate the 'price' variable
trade$price <- (trade$TradeValue_1000_USD * 1000) / trade$Quantity
summary(trade$price)
#remove if NA or inf
trade <- trade[!is.na(trade$price)&trade$price<Inf,]

#truncate top (and bottom) n of prices ( here 5%)
trade <- trade[trade$price<quantile(trade$price,prob=1-5/100),]
trade <- trade[trade$price>quantile(trade$price,prob=5/100),]
summary(trade$price)


# select only imports
table(trade$TradeFlowName)
trade <- trade %>% filter(TradeFlowName =="Gross Imp." )


################################################################################

# 2) quantity based 


# if interested in quantity based 
################################################################################
# a) look at HS6 code and make it a character

names(trade)
head(trade)
class(trade$ProductCode)
length(unique(trade$ProductCode))
summary(trade$Quantity)


# make the variable in character: 
trade$ProductCode <- stri_pad_left(trade$ProductCode,6,0)
num_characters <- nchar(trade$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)



# b) quantity tokens 
# https://wits.worldbank.org/WITS/WITS/WITSHELP/Content/Codes/Quantity_Tokens.htm 
table(trade$QuantityToken)
obs  <- trade %>% filter(QuantityToken ==1)
summary(obs$Quantity) # check if values are 0

################################################################################
# 4) EU as single countries 


table(trade$PartnerISO3)

# defining EU countries (2015 so pre-Brexit)
EUN <- c('AUT', 'BEL', 'BGR', 'HRV', 'CYP', 'CZE', 'DNK', 'EST', 'FIN', 'FRA', 'DEU', 
         'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT', 'NLD', 'POL', 'PRT',
         'ROM', 'SVK', 'SVN', 'ESP', 'SWE', 'GBR')

################################################################################

names(trade)
table(trade$ReporterISO3)
table(trade$PartnerISO3)
#drop if partner equal to brazil 
table(trade$PartnerISO3)
trade <- trade[trade$PartnerISO3 != "BRA", ]


################################################################################
# Checks

names(trade)


length(unique(trade$ProductCode))
table(trade$Year)
table(trade$ReporterName)
summary(trade$Quantity)




write.csv(trade, "/data/sikeme/TRADE/WTO/data/BRA/BRA_trade.csv", row.names = FALSE)
write.csv(trade, "/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_price.csv", row.names = FALSE)


################################################################################
# 5) merge characteristic vars and trade 

names(trade)
table(trade$ReporterISO3)
table(trade$PartnerISO3)
#drop if partner equal to brazil 
table(trade$PartnerISO3)
trade <- trade[trade$PartnerISO3 != "BRA", ]



table(trade$Year)


merge_X <- read_csv("/home/sikeme/TRADE/WTO/data/BRA/country_char/BRA_merge_X.csv")

Merge <- left_join(trade,merge_X )
table(Merge$ReporterISO3)
table(Merge$Year)
colSums(is.na(Merge))

# bUT A LOT OF MISSING OBSERVATIONS:
library(zoo)
na_obs <- Merge[is.na(Merge$dist), ]




Merge <- Merge[order(Merge$PartnerISO3, Merge$Year), ]

# Fill missing values with the previous year's value within each country
#for sharing the same language
table(Merge$comlang_off)
Merge$comlang_off <- ave(Merge$comlang_off, Merge$PartnerISO3, FUN = function(x) na.locf(x, na.rm = FALSE))
#for the distance: 
table(Merge$dist)
Merge$dist <- ave(Merge$dist, Merge$PartnerISO3, FUN = function(x) na.locf(x, na.rm = FALSE))
#for the sharing a border: 
table(Merge$contig)
Merge$contig <- ave(Merge$contig, Merge$PartnerISO3, FUN = function(x) na.locf(x, na.rm = FALSE))
#for being landlock
table(Merge$landlocked_o)
Merge$contig <- ave(Merge$landlocked_o, Merge$PartnerISO3, FUN = function(x) na.locf(x, na.rm = FALSE))
table(Merge$landlocked_d)
Merge$contig <- ave(Merge$landlocked_d, Merge$PartnerISO3, FUN = function(x) na.locf(x, na.rm = FALSE))



#for Brazil missing gdp: 
brazil_ln_GDP <- gdp_per_capita %>%
  filter(iso3c == "BRA") %>%
  select(year, ln_GDP_Brazil = ln_Gdp_per_cap)
Merge <- subset(Merge, select = -ln_GDP_Brazil )
Merge <- left_join(Merge, brazil_ln_GDP, by = c("Year" = "year"))



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

Merge1 <- left_join(Merge, tariff4)
colSums(is.na(Merge1))


# if the tariff rate isNA then put 0 instead
Merge1$tariff_BRA[is.na(Merge1$tariff_BRA)] <- 0

################################################################################
# Save the data 

write.csv(Merge1, "/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_Merge2.csv", row.names = FALSE)
Merge <- read_csv("/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_Merge.csv")
table(Merge$Year)
names(Merge)
