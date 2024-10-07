

################################################################################

# 2 values to calculate

# we need the value importers share in the world market (by country, HS codes and year)
# importer values of imports / total world imports 

# we need the value exporter share in the world market (by country, HS codes and year)
# exporter values of expors / total world expors 




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




trade_sh <- read_csv("dta_WITS/trade_share/WITS_trade_share.csv")

#Checks
names(trade_sh)
table(trade_sh$Year)
table(trade_sh$Nomenclature)
table(trade_sh$ReporterISO3)
table(trade_sh$PartnerISO3)
length(unique(trade_sh$ProductCode))
table(trade_sh$TradeFlowName)




# drop reporter all countries
trade_sh <- trade_sh %>% filter(ReporterISO3!= "All")

################################################################################


# 1) Separate imports and exports

table(trade_sh$TradeFlowName)
export_sh <- trade_sh %>% filter(TradeFlowName == "Gross Exp.")
import_sh <- trade_sh %>% filter(TradeFlowName == "Gross Imp.")





################################################################################


# 2) Get total world imports values and export values

#for  export
str(export_sh)
colSums(is.na(export_sh))
export_sh <- export_sh %>% 
  group_by(ProductCode, Year) %>% 
  mutate(tot_exp_val = sum(`TradeValue in 1000 USD`, na.rm = TRUE)) %>% 
  ungroup()

export_sh <- export_sh %>% arrange(Year, ProductCode)


#for  import
str(import_sh)
colSums(is.na(import_sh))
import_sh <- import_sh %>% 
  group_by(ProductCode, Year) %>% 
  mutate(tot_imp_val = sum(`TradeValue in 1000 USD`, na.rm = TRUE)) %>% 
  ungroup()

import_sh <- import_sh %>% arrange(Year, ProductCode)



################################################################################


# 3) Create the shares:

export_sh <- export_sh %>% mutate(export_share = `TradeValue in 1000 USD`/tot_exp_val * 100)
import_sh <- import_sh %>% mutate(import_share = `TradeValue in 1000 USD`/tot_imp_val * 100)


names(export_sh)

export_sh <- export_sh %>% select(Nomenclature, ReporterISO3, Year,ProductCode, export_share )
import_sh <- import_sh %>% select(Nomenclature, ReporterISO3, Year,ProductCode,import_share )


# Merge the 2
trade_share1 <- full_join(export_sh,import_sh)
names(trade_share1)

################################################################################



# 4) Export

write.csv(trade_share1, "dta_WITS/trade_share/clean_trade_share.csv", row.names = FALSE)




