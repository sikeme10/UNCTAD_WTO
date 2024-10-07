
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



#two steps: I and normal regression


################################################################################
# 1) Get the countries for the IVs

################################################################################
# if use count

# a) UNCTAD Version 

#load data:
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD_TBT.csv")

# create a count of NTM

dta_UNCTAD_SPS_reg <- subset(dta_UNCTAD_SPS, select = c(Year, Reporter, Partner ,HSCode,ntm_all ))
BRA_dta_UNCTAD_SPS <- subset(dta_UNCTAD_SPS_reg, Reporter =="BRA" )
BRA_dta_UNCTAD_SPS <- BRA_dta_UNCTAD_SPS %>% rename( NTM_BRA_SPS = ntm_all)
names(BRA_dta_UNCTAD_SPS)

# for TBT 
dta_UNCTAD_TBT_reg <- subset(dta_UNCTAD_TBT, select = c(Year, Reporter, Partner ,HSCode,ntm_all ))
BRA_dta_UNCTAD_TBT<- subset(dta_UNCTAD_TBT_reg,Reporter =="BRA" )
BRA_dta_UNCTAD_TBT <- BRA_dta_UNCTAD_TBT %>% rename( NTM_BRA_TBT = ntm_all)




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




#2)  get NTM data:

# a) select the year of interest: 
BRA_UNCTAD_SPS <- subset(BRA_dta_UNCTAD_SPS, Year ==2015)
BRA_UNCTAD_TBT <- subset(BRA_dta_UNCTAD_TBT, Year ==2015)



# b) get rid of possible issues with WOLRD as partner


WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- c(unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]), 
                    unique(unctad_data$Partner[unctad_data$Partner != "WLD"]))  # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'Partner' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, Partner == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, Partner = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(Partner != "WLD"), # Remove rows where Partner is 'WLD'
                             bind_rows(new_observations)) 
  
  combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
}  #By hand at the bottom 

# Call the function with the provided dataframes
BRA_UNCTAD_SPS <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_SPS)
BRA_UNCTAD_TBT <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_TBT)
names(BRA_UNCTAD_SPS)
names(BRA_UNCTAD_TBT)


#calculate counts of NTM by partners and HS code 
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum(NTM_BRA_SPS, na.rm = TRUE))
# check if duplicates
duplicates <- BRA_UNCTAD_SPS_count[duplicated(BRA_UNCTAD_SPS_count), ]

#for TBT 
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_TBT= sum(NTM_BRA_TBT, na.rm = TRUE))
# check if duplicates
duplicates <- BRA_UNCTAD_TBT_count[duplicated(BRA_UNCTAD_TBT_count), ]


# rename variable
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS_count %>% rename(ReporterISO3 = Reporter,
                                            PartnerISO3 = Partner,
                                            ProductCode = HSCode,
                                            ntm_count_SPS = total_NTM_BRA_SPS)
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT_count %>% rename(ReporterISO3 = Reporter,
                                            PartnerISO3 = Partner,
                                            ProductCode = HSCode,
                                            ntm_count_TBT = total_NTM_BRA_TBT)





#3)  now merge the two:

Merge <- left_join(BRA_trade, BRA_UNCTAD_SPS_count)
Merge <- left_join(Merge, BRA_UNCTAD_TBT_count)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_count_SPS <- ifelse(is.na(Merge$ntm_count_SPS), 0 , Merge$ntm_count_SPS) 
Merge$ntm_count_TBT <- ifelse(is.na(Merge$ntm_count_TBT), 0 , Merge$ntm_count_TBT) 



Merge$ProductCode <- as.factor(Merge$ProductCode)

#reg1 <- lm(log(price) ~
#             log(dist) + contig + comlang_off + rta + landlocked_o +landlocked_d +
#             ntm_bin  + MFN_tariff+
#             ProductCode +
#             PartnerISO3 ,    data = Merge)
#

library(fixest)
names(Merge)
# Run the regression model with fixest
reg1 <- feols(log(price) ~
                log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                ntm_count_SPS + ntm_count_TBT   + log(1+ MFN_tariff) + ProductCode +
                PartnerISO3 , data = Merge)
summary(reg1)





################################################################################
#IV


countries <- c("BRA", "ARG","BOL", "COL","PRY","PER", "URY", "USA", "EUN")


dta_UNCTAD_SPS_reg <- subset(dta_UNCTAD_SPS, select = c(Year, Reporter, Partner ,HSCode,ntm_all ))
BRA_dta_UNCTAD_SPS <- subset(dta_UNCTAD_SPS_reg, Reporter =="BRA" )
BRA_dta_UNCTAD_SPS <- BRA_dta_UNCTAD_SPS %>% rename( NTM_BRA_SPS = ntm_all)
BRA_UNCTAD_SPS <- subset(BRA_dta_UNCTAD_SPS, Year ==2015)
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum(NTM_BRA_SPS, na.rm = TRUE))



country_data <- list()
countries <- c("BRA", "ARG","BOL", "COL","PRY","PER", "URY", "USA", "EUN")
for (country in countries) {
  # Subset the data for the current country
  country_subset <- subset(dta_UNCTAD_SPS, Reporter == country, select = c(Year, Partner, HSCode, ntm_all))
  
  # Rename the ntm_all column to NTM_COUNTRY_SPS
  country_subset <- country_subset %>%
    rename(!!paste0("NTM_", country, "_SPS") := ntm_all)
  
  # Filter data for the specified year
  country_subset <- filter(country_subset, Year == 2015)
  
  # Group by Year, Partner, and HSCode and summarize
  country_summary <- country_subset %>%
    group_by(Year, Partner, HSCode) %>%
    summarise(!!paste0("total_NTM_", country, "_SPS") := sum(!!rlang::sym(paste0("NTM_", country, "_SPS")), na.rm = TRUE)) %>%
    ungroup() 
  # Store the subset in the country_data list
  country_data[[country]] <- country_summary
}
# Perform full join on all data frames in the list
merged_UNCTAD_NTM <- Reduce(full_join, country_data)

colSums(is.na(merged_UNCTAD_NTM))
merged_UNCTAD_NTM <- merged_UNCTAD_NTM %>%
  mutate_all(~replace_na(., 0))

names(merged_UNCTAD_NTM)
merged_UNCTAD_NTM <- merged_UNCTAD_NTM %>% filter(!is.na(total_NTM_BRA_SPS))


reg1 <- feglm(total_NTM_BRA_SPS ~ total_NTM_ARG_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
                total_NTM_PRY_SPS + total_NTM_PER_SPS + total_NTM_URY_SPS + HSCode,
              data = merged_UNCTAD_NTM, family = poisson(link = "log"))
reg1


models <- merged_UNCTAD_NTM %>%
  group_by(HSCode) %>%
  do(model = glm(total_NTM_BRA_SPS ~ total_NTM_ARG_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
                   total_NTM_PRY_SPS + total_NTM_PER_SPS + total_NTM_URY_SPS,
                 data = ., family = poisson()))

models$model
coefficients <- tidy(models)




# with exogenous vars

dta <- subset(merged_UNCTAD_NTM, HSCode == "010121")


reg1 <- feglm(total_NTM_BRA_SPS ~ total_NTM_ARG_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
                total_NTM_PRY_SPS + total_NTM_PER_SPS + total_NTM_URY_SPS,
              data = dta, family = poisson(link = "log"))
reg1



################### trying IV 

# create a binary for presence of NTM

dta_UNCTAD_SPS_reg <- subset(dta_UNCTAD_SPS, select = c(Year, Reporter, Partner ,HSCode,ntm_bin ))
names(dta_UNCTAD_SPS_reg)



country_data <- list()
countries <- c("BRA", "ARG","BOL", "COL","PRY","PER", "URY", "USA", "EUN")
# Loop through each country in the list
for (country in countries) {
  # Subset the data for the current country
  country_subset <- subset(dta_UNCTAD_SPS_reg, Reporter == country)
  # Rename the 'ntm_bin' column to 'NTM_country'
  country_subset <- country_subset %>% rename(!!paste0("NTM_", country) := ntm_bin)
  country_subset <- select(country_subset, -Reporter)
  # Store the subset in the country_data list
  country_data[[country]] <- country_subset
}

merged_UNCTAD_NTM<- bind_rows(country_data)
merged_UNCTAD_NTM <- mutate_all(merged_UNCTAD_NTM, ~ifelse(is.na(.), 0, .))
colSums(merged_UNCTAD_NTM == 0, na.rm = TRUE)
colSums(is.na(merged_UNCTAD_NTM))


# subset for a year 
merged_UNCTAD_NTM_2012 <- subset(merged_UNCTAD_NTM, Year == 2015)

str(merged_UNCTAD_NTM_2012)
colSums(merged_UNCTAD_NTM_2012 == 0, na.rm = TRUE)


models <- merged_UNCTAD_NTM_2012 %>%
  group_by(HSCode) %>%
  do(model = glm(NTM_BRA ~ NTM_ARG + NTM_BOL + NTM_COL + NTM_PRY + NTM_PER +NTM_URY+NTM_USA+ NTM_EUN,
                 data = ., family = binomial(link = "probit")))

reg1 <- feglm(NTM_BRA ~ NTM_ARG + NTM_BOL + NTM_COL + NTM_PRY + NTM_PER + NTM_URY + NTM_USA + NTM_EUN,
              data = merged_UNCTAD_NTM_2012,cluster = "HSCode", family = binomial)

predicted_probs <- predict(reg1, type = "response")


coefficients <- models %>% tidy(model)
# Extract coefficients and other information from models

predicted_values <- models %>%
  mutate(predicted_NTM_BRA = predict(model, newdata = ., type = "response"))



HS_test <- subset(merged_UNCTAD_NTM_2012, HSCode == "010121")









################################################################################

# 2) Run the regression

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




#2)  get NTM data:

# a) select the year of interest: 
BRA_UNCTAD_SPS <- subset(BRA_dta_UNCTAD_SPS, Year ==2015)
BRA_UNCTAD_TBT <- subset(BRA_dta_UNCTAD_TBT, Year ==2015)

#drop duplicates if needed;
BRA_UNCTAD_SPS <- distinct(BRA_UNCTAD_SPS)
BRA_UNCTAD_TBT <- distinct(BRA_UNCTAD_TBT)


# b) get rid of possible issues with WOLRD as partner


WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]) # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'Partner' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, Partner == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, Partner = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(Partner != "WLD"), # Remove rows where Partner is 'WLD'
                             bind_rows(new_observations)) 
  
  combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
}  #By hand at the bottom 

# Call the function with the provided dataframes
BRA_UNCTAD_SPS <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_SPS)
BRA_UNCTAD_TBT <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_TBT)
names(BRA_UNCTAD_SPS)
names(BRA_UNCTAD_TBT)


# rename variable
BRA_UNCTAD_SPS <- BRA_UNCTAD_SPS %>% rename(ReporterISO3 = Reporter,
                                            PartnerISO3 = Partner,
                                            ProductCode = HSCode,
                                            ntm_bin_SPS = NTM_BRA_SPS)
BRA_UNCTAD_TBT <- BRA_UNCTAD_TBT %>% rename(ReporterISO3 = Reporter,
                                            PartnerISO3 = Partner,
                                            ProductCode = HSCode,
                                            ntm_bin_TBT = NTM_BRA_TBT)


BRA_UNCTAD_SPS <- distinct(BRA_UNCTAD_SPS)
BRA_UNCTAD_TBT <- distinct(BRA_UNCTAD_TBT)



#3)  now merge the two:

Merge <- left_join(BRA_trade, BRA_UNCTAD_SPS)
Merge <- left_join(Merge, BRA_UNCTAD_TBT)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_bin_SPS <- ifelse(is.na(Merge$ntm_bin_SPS), 0 , Merge$ntm_bin_SPS) 
Merge$ntm_bin_TBT <- ifelse(is.na(Merge$ntm_bin_TBT), 0 , Merge$ntm_bin_TBT) 

write.csv(Merge, "/home/sikeme/TRADE/WTO/data/BRA/Merge.csv")
