
rm(list=ls())
# Set directory
setwd("/home/sikeme/TRADE/WTO/data")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)
library(data.table)
library(stringi)



#two steps: I and normal regression



################################################################################
# 1) Get the countries for the IVs

################################################################################




#1) get trade merged data 
BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge.csv")
names(BRA_trade_Merge)
BRA_trade_Merge <- select(BRA_trade_Merge,
                          "Year", "Nomenclature","ReporterISO3",
                          "ProductCode","ReporterName",
                          "PartnerISO3", "PartnerName",
                          "price","rta","contig","dist",
                          "comlang_off","country_o",
                          "landlocked_o","landlocked_d",
                          "ln_Gdp_per_cap","ln_GDP_Brazil", "MFN_tariff")
# select a year 
BRA_trade <- subset(BRA_trade_Merge, Year == 2015)













# clean WTO data 

# a) WTO Version 

#load data:
dta_WTO_SPS <- read_csv("BRA/BRA_WTO_SPS.csv")
dta_WTO_TBT <- read_csv("BRA/BRA_WTO_TBT.csv")


# b) select the year of interest: 
dta_WTO_SPS <- subset(dta_WTO_SPS, Year == 2015)
dta_WTO_TBT <- subset(dta_WTO_TBT, Year ==2015)



dta_WTO_SPS <- dta_WTO_SPS %>% rename(ntm_bin_SPS = SPS1)
dta_WTO_SPS <- select(dta_WTO_SPS, ReporterISO3 , ProductCode, PartnerISO3,Year,ntm_bin_SPS)


dta_WTO_TBT <- dta_WTO_TBT %>% rename(ntm_bin_TBT = TBT1)
dta_WTO_TBT <- select(dta_WTO_TBT, ReporterISO3 , ProductCode, PartnerISO3,Year,ntm_bin_TBT)


#drop duplicates if needed;
dta_WTO_SPS <- distinct(dta_WTO_SPS)
dta_WTO_TBT <- distinct(dta_WTO_TBT)



# b) get rid of possible issues with WOLRD as partner
WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]) # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'Partner' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where Partner is 'WLD'
                             bind_rows(new_observations)) 
  
  combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
}


SPS <- WLD_partner(BRA_trade, dta_WTO_SPS)
TBT <- WLD_partner(BRA_trade, dta_WTO_TBT)



#3)  now merge the two:

Merge <- left_join(BRA_trade, SPS)
Merge <- left_join(Merge, TBT)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_bin_SPS <- ifelse(is.na(Merge$ntm_bin_SPS), 0 , Merge$ntm_bin_SPS) 
Merge$ntm_bin_TBT <- ifelse(is.na(Merge$ntm_bin_TBT), 0 , Merge$ntm_bin_TBT) 

write.csv(Merge, "/home/sikeme/TRADE/WTO/data/BRA/Merge_WTO.csv")







################################################################################


# run regression:

################################################################################

Merge$ProductCode <- as.factor(Merge$ProductCode)

#reg1 <- lm(log(price) ~
#             log(dist) + contig + comlang_off + rta + landlocked_o +landlocked_d +
#             ntm_bin  + MFN_tariff+
#             ProductCode +
#             PartnerISO3 ,    data = Merge)
#

library(fixest)

# Run the regression model with fixest
reg1 <- feols(log(price) ~
                log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                ntm_bin_SPS + ntm_bin_TBT   + MFN_tariff +ProductCode +
                PartnerISO3 , data = Merge)
summary(reg1)
res <- etable(reg1)

library(openxlsx)
write.xlsx(res, file = "/home/sikeme/TRADE/WTO/result/regression_WTO.xlsx")

beta_SPS <- coef(reg1)["ntm_bin_SPS"]
beta_TBT <- coef(reg1)["ntm_bin_TBT"]



var_beta <- vcov(regression_model)["x1", "x1"]  # Variance of the coefficient













