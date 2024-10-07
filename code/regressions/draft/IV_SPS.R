





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


# Load data


################################################################################

# a) UNCTAD Version 

#load data:
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")



################################################################################

# b) select neighboring countries as an IV 

#Get the countries of interest 
names(dta_UNCTAD_SPS)
unique(dta_UNCTAD_SPS$Reporter)
unique(dta_UNCTAD_SPS$Partner)
table(dta_UNCTAD_SPS$Reporter, dta_UNCTAD_SPS$Year)


#create a dtaaset with only countries of interest
country_data <- list()
countries <- c("BRA", "CHL","BOL", "COL","ECU", "PER")
for (country in countries) {
  # Subset the data for the current country
  country_subset <- subset(dta_UNCTAD_SPS, Reporter == country, select = c(Year, Partner, HSCode, ntm_all))
  
  # Rename the ntm_all column to NTM_COUNTRY_SPS
  country_subset <- country_subset %>%
    rename(!!paste0("NTM_", country, "_SPS") := ntm_all)
  
  # Filter data for the specified year
  #country_subset <- filter(country_subset, Year == 2015)
  
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

#if only select partner as WLD
merged_UNCTAD_NTM <- subset(merged_UNCTAD_NTM,Partner =="WLD" )
colSums(is.na(merged_UNCTAD_NTM))
length(unique(merged_UNCTAD_NTM$HSCode))

# put value 0 instead of NA 
colSums(is.na(merged_UNCTAD_NTM))
merged_UNCTAD_NTM <- merged_UNCTAD_NTM %>% mutate_all(~replace_na(., 0))

# Get HS levels variables
merged_UNCTAD_NTM$H1<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 1)
merged_UNCTAD_NTM$H2<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 2)
merged_UNCTAD_NTM$H3<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 3)
merged_UNCTAD_NTM$H4<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 4)
merged_UNCTAD_NTM$H5<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 5)



################################################################################


#  IV reg


################################################################################

#1) Run the regression (on product code included)
reg1 <- feglm(total_NTM_BRA_SPS ~ total_NTM_CHL_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
                total_NTM_ECU_SPS  +total_NTM_PER_SPS,
              data = merged_UNCTAD_NTM, family = poisson(link = "log"))

reg1


################################################################################


#2) Run the regression at H1 level


# sum NTM counts at HS2 level 
names(merged_UNCTAD_NTM)
merged_UNCTAD_H5 <- merged_UNCTAD_NTM %>%
  group_by(Year, Partner,H1, H2, H3, H4, H5) %>%
  summarize(total_NTM_BRA_SPS = sum(total_NTM_BRA_SPS , na.rm = TRUE),
            total_NTM_CHL_SPS = sum(total_NTM_CHL_SPS , na.rm = TRUE),
            total_NTM_BOL_SPS = sum(total_NTM_BOL_SPS , na.rm = TRUE),
            total_NTM_COL_SPS = sum(total_NTM_COL_SPS , na.rm = TRUE),
            total_NTM_ECU_SPS = sum(total_NTM_ECU_SPS , na.rm = TRUE),
            #total_NTM_ARG_SPS = sum(total_NTM_ARG_SPS , na.rm = TRUE),
            total_NTM_PER_SPS = sum(total_NTM_PER_SPS , na.rm = TRUE))
names(merged_UNCTAD_H5)
str(merged_UNCTAD_H5)



#re run the regression for each H2
unique_H1 <- unique(merged_UNCTAD_H5$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()

# Run the regression for each H1 value 
for (h2_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_UNCTAD <- subset(merged_UNCTAD_H5, H1 == h2_val)
  
  # Run the regression
  # reg_H5 <- feglm(total_NTM_BRA_SPS ~ 0+  total_NTM_CHL_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
  #                   total_NTM_ECU_SPS + total_NTM_PER_SPS,
  #                 data = sub_Merge_UNCTAD, family = poisson(link = "log"))
  
  
  reg_H5 <- feols(total_NTM_BRA_SPS ~ 0+  total_NTM_CHL_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
                    total_NTM_ECU_SPS + total_NTM_PER_SPS,
                  data = sub_Merge_UNCTAD)
  
  # Store the summary of regression
  reg_summaries[[h2_val]] <- summary(reg_H5)
}


# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H2 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}

# Prediction for one value 
# unique_H1 <- unique(merged_UNCTAD_H5$H1)
# 
# sub_Merge_UNCTAD <- subset(merged_UNCTAD_H5, H1 == "0")
# names(sub_Merge_UNCTAD)
# newdata = data.frame(Year=sub_Merge_UNCTAD$Year,
#                      Partner =sub_Merge_UNCTAD$Partner,
#                      H5=sub_Merge_UNCTAD$H5,
#                      total_NTM_BRA_SPS = sub_Merge_UNCTAD$total_NTM_BRA_SPS,
#                      total_NTM_CHL_SPS = sub_Merge_UNCTAD$total_NTM_CHL_SPS,
#                      total_NTM_BOL_SPS = sub_Merge_UNCTAD$total_NTM_BOL_SPS,
#                      total_NTM_COL_SPS = sub_Merge_UNCTAD$total_NTM_COL_SPS,
#                      total_NTM_ECU_SPS = sub_Merge_UNCTAD$total_NTM_ECU_SPS,
#                      total_NTM_PER_SPS = sub_Merge_UNCTAD$total_NTM_PER_SPS,
#                      sub_Merge_UNCTAD$H1 == "0")
# reg_summaries[[1]]
# predicted_values <- predict(reg_summaries[[1]], newdata,type = "response")
# newdata$predict_BRA <- predicted_values





# Loop through all unique values of H1
all_sub_Merge_UNCTAD <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_UNCTAD <- subset(merged_UNCTAD_H5, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_UNCTAD$Year,
                        Partner = sub_Merge_UNCTAD$Partner,
                        H5 = sub_Merge_UNCTAD$H5,
                        total_NTM_BRA_SPS = sub_Merge_UNCTAD$total_NTM_BRA_SPS,
                        total_NTM_CHL_SPS = sub_Merge_UNCTAD$total_NTM_CHL_SPS,
                        total_NTM_BOL_SPS = sub_Merge_UNCTAD$total_NTM_BOL_SPS,
                        total_NTM_COL_SPS = sub_Merge_UNCTAD$total_NTM_COL_SPS,
                        total_NTM_ECU_SPS = sub_Merge_UNCTAD$total_NTM_ECU_SPS,
                        total_NTM_PER_SPS = sub_Merge_UNCTAD$total_NTM_PER_SPS)
  
  # Get the predicted values
  predicted_values <- predict(reg_summaries[[h1_val]], newdata, type = "response")
  
  # Add the predicted values as a new column to the original data frame
  sub_Merge_UNCTAD$predict_BRA <- round(predicted_values, digits = 0)
  all_sub_Merge_UNCTAD <- rbind(all_sub_Merge_UNCTAD, sub_Merge_UNCTAD)
}



# get the predicted vlue:
names(all_sub_Merge_UNCTAD)


# rename variable
BRA_UNCTAD_SPS_count <- all_sub_Merge_UNCTAD %>% rename(PartnerISO3 = Partner,
                                                        ntm_count_SPS = predict_BRA)
names(BRA_UNCTAD_SPS_count)

################################################################################


#  Trade data Merge:


################################################################################

#1) get Brazil trade data 
BRA_trade <- read_csv("BRA/BRA_trade_Merge.csv")
names(BRA_trade_Merge)
BRA_trade1 <- BRA_trade

# a) Aggregate at H5 and recalculate prices

# aggregate at H5 level:
BRA_trade$H5<- substring(BRA_trade$ProductCode, first = 1, last = 5)
# at H5 from HS6
BRA_trade <- BRA_trade %>% group_by(Year, ReporterISO3, PartnerISO3, H5) %>%
  summarize (TradeValue_1000_USD_H5 = sum(TradeValue_1000_USD),
             Quantity_H5 = sum(Quantity),
             MFN_tariff_H5 = sum(MFN_tariff))
# drop partner: 
BRA_trade_H5 <- BRA_trade %>% group_by(Year, ReporterISO3, H5) %>%
  summarize (TradeValue_1000_USD_H5 = sum(TradeValue_1000_USD_H5),
             Quantity_H5 = sum(Quantity_H5),
             MFN_tariff_H5 = sum(MFN_tariff_H5))
BRA_trade_H5$price_H5 <- BRA_trade_H5$TradeValue_1000_USD_H5*1000 / BRA_trade_H5$Quantity_H5 



# b) create the weights: 
sum_BRA_trade <- BRA_trade %>% filter(Year == 2012) %>%
  group_by(ReporterISO3, H5) %>%
  summarize(tot_imp = sum(Quantity_H5))
#join the two
Weights_BRA_trade <- full_join(sum_BRA_trade, BRA_trade) 
#create the weights 
Weights_BRA_trade <-  Weights_BRA_trade %>% filter(Year == 2012) %>%
  mutate(weights_2012 = Quantity_H5/tot_imp)
Weights_BRA_trade <-  select(Weights_BRA_trade, H5,weights_2012,PartnerISO3)
# select the NTM data:'
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS_count %>% group_by(Year, H5) %>%
  select(Year, H5, ntm_count_SPS)
#join the two data sets
Weights_BRA_trade <- full_join(Weights_BRA_trade,BRA_UNCTAD_SPS_count, relationship = "many-to-many")
# recalculate NTM counts with the weight:
Weights_BRA_trade$NTM_SPS_weigthed <- Weights_BRA_trade$ntm_count_SPS * Weights_BRA_trade$weights_2012
Weights_BRA_trade <- Weights_BRA_trade %>% group_by(ReporterISO3, Year, H5) %>% 
  summarize( ntm_count_SPS_weight =  sum(NTM_SPS_weigthed))




# b) Merge weighted NTM data and trade data: 
BRA_trade_H5 <- select(BRA_trade_H5, Year, ReporterISO3, H5, MFN_tariff_H5, price_H5 )
Merge_UNCTAD_IV <- left_join(BRA_trade_H5, Weights_BRA_trade)


# Save the data 
write.csv(Merge_UNCTAD_IV, "BRA/IVs/IV_SPS_UNCTAD.csv")




