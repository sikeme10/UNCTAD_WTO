




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

# FOR SPS:
# 1) Load the data first 



################################################################################


# 1) Load data


################################################################################

# UNCTAD Version SPS

#load data:
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD/dta_UNCTAD_SPS.csv")



################################################################################

# 2) Country selection and aggregation

# select neighboring countries as an IV 


#Get the countries of interest 
names(dta_UNCTAD_SPS)
unique(dta_UNCTAD_SPS$Reporter)
unique(dta_UNCTAD_SPS$Partner)
table(dta_UNCTAD_SPS$Reporter, dta_UNCTAD_SPS$Year)


#create a dataset with only countries of interest: we select the country of interest 
#we get the count of ntm for a specific year, exporter and HS code for each importer selected
country_data <- list()
#select countries
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


# put value 0 instead of NA 
colSums(is.na(merged_UNCTAD_NTM))
merged_UNCTAD_NTM <- merged_UNCTAD_NTM %>% mutate_all(~replace_na(., 0))


# Create HS levels variables
merged_UNCTAD_NTM$H1<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 1)
merged_UNCTAD_NTM$H2<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 2)
merged_UNCTAD_NTM$H3<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 3)
merged_UNCTAD_NTM$H4<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 4)
merged_UNCTAD_NTM$H5<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 5)



# Can aggregate NTM counts at HS 5 level:
# we can aggregate it to have less convergence issues when running the IV regression
names(merged_UNCTAD_NTM)
#choose merging level at HS5 level: 
merged_UNCTAD_NTM  <- merged_UNCTAD_NTM %>% group_by(Year, Partner, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum (total_NTM_BRA_SPS),
            total_NTM_CHL_SPS = sum (total_NTM_CHL_SPS),
            total_NTM_BOL_SPS = sum (total_NTM_BOL_SPS),
            total_NTM_COL_SPS = sum (total_NTM_COL_SPS),
            total_NTM_ECU_SPS = sum (total_NTM_ECU_SPS),
            total_NTM_PER_SPS = sum (total_NTM_PER_SPS))


###############################################################################

# 3) Data manipulation: 
# renaming, year selection, and WLD partner subtitution to trading partners

#rename partner for coherence whith other data 
merged_UNCTAD_NTM  <-merged_UNCTAD_NTM  %>% rename(PartnerISO3 =Partner)
# length(unique(merged_UNCTAD_NTM $PartnerISO3))


# get years from 2012 to 2016
merged_UNCTAD_NTM  <- subset(merged_UNCTAD_NTM , Year< 2017)



# need to get rid of WLD as partner:
table(merged_UNCTAD_NTM $PartnerISO3)



BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge.csv")
# instead of WLD as a partner we use all the countries where 
WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- c(unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]), 
                    unique(unctad_data$PartnerISO3[unctad_data$PartnerISO3 != "WLD"]))  # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'PartnerISO3' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
} #By hand at the bottom 

# Call the function with the provided dataframes
SPS_UNCTAD <- WLD_partner(BRA_trade_Merge, merged_UNCTAD_NTM )
names(SPS_UNCTAD)

#calculate counts of NTM by partners and HS code 
SPS_UNCTAD <- SPS_UNCTAD %>%  group_by(Year, PartnerISO3, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum(total_NTM_BRA_SPS, na.rm = TRUE),
            total_NTM_CHL_SPS = sum(total_NTM_CHL_SPS, na.rm = TRUE),
            total_NTM_BOL_SPS = sum(total_NTM_BOL_SPS, na.rm = TRUE),
            total_NTM_COL_SPS = sum(total_NTM_COL_SPS, na.rm = TRUE),
            total_NTM_ECU_SPS = sum(total_NTM_ECU_SPS, na.rm = TRUE),
            total_NTM_PER_SPS = sum(total_NTM_PER_SPS, na.rm = TRUE))


colSums(is.na(SPS_UNCTAD))
#  summary(BRA_UNCTAD_SPS_count$total_NTM_BRA_SPS)
#  # check if duplicates
#  duplicates <- BRA_UNCTAD_SPS_count[duplicated(BRA_UNCTAD_SPS_count), ]



################################################################################

# 4) Exporter weights

#if we want to get rid of exporter specificity 
#we can Multiply by exporter weights 

# load the weights 
Weighting_trade <-  read_csv("BRA/IVs/Weighting_trade.csv")
names(Weighting_trade)
names(merged_UNCTAD_NTM )
class(Weighting_trade$H5)
#Merge the two data 
weighted_NTM  <- left_join(merged_UNCTAD_NTM , Weighting_trade)
colSums(is.na(weighted_NTM))





################################################################################

  # run the first IV regression

################################################################################


# Get HS levels variables
SPS_UNCTAD$H1<- substring(SPS_UNCTAD$HSCode, first = 1, last = 1)
SPS_UNCTAD$H2<- substring(SPS_UNCTAD$HSCode, first = 1, last = 2)
SPS_UNCTAD$H3<- substring(SPS_UNCTAD$HSCode, first = 1, last = 3)
SPS_UNCTAD$H4<- substring(SPS_UNCTAD$HSCode, first = 1, last = 4)




names(SPS_UNCTAD)


#re run the regression for each H2
unique_H1 <- unique(SPS_UNCTAD$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()


# Run the regression for each H1 value 
for (H1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_UNCTAD <- subset(SPS_UNCTAD, H1 == H1_val)
  
  # Run the regression
  # reg_H5 <- feglm(total_NTM_BRA_SPS ~ 0+  total_NTM_CHL_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
  #                   total_NTM_ECU_SPS + total_NTM_PER_SPS,
  #                 data = sub_Merge_UNCTAD, family = poisson(link = "log"))
  
  
  reg_H5 <- feols(total_NTM_BRA_SPS ~ 0+  total_NTM_BRA_SPS + total_NTM_BOL_SPS + total_NTM_COL_SPS +
                    total_NTM_ECU_SPS + total_NTM_PER_SPS,
                  data = sub_Merge_UNCTAD)
  
  # Store the summary of regression
  reg_summaries[[H1_val]] <- summary(reg_H5)
}


regres_table <- bind_rows(reg_summaries, .id = "H1")


# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}

sum_table <- etable(reg_summaries[[1]], reg_summaries[[2]], reg_summaries[[3]],
                        reg_summaries[[4]], reg_summaries[[5]], reg_summaries[[6]],
                        reg_summaries[[7]], reg_summaries[[8]], reg_summaries[[9]],
                        reg_summaries[[10]],
                        headers = "WTO")
names(sum_table) <- c("vars","H0","H1", "H2", "H3","H4", "H5", "H6","H7", "H8", "H9")


######### Prediction 

# Loop through all unique values of H1
all_sub_Merge_UNCTAD <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_UNCTAD <- subset(SPS_UNCTAD, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_UNCTAD$Year,
                        PartnerISO3 = sub_Merge_UNCTAD$PartnerISO3,
                        HSCode = sub_Merge_UNCTAD$HSCode,
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


BRA_UNCTAD_SPS_count <- all_sub_Merge_UNCTAD %>% rename(ntm_count_SPS = predict_BRA)
BRA_UNCTAD_SPS_count <- select(BRA_UNCTAD_SPS_count, Year , PartnerISO3, HSCode, ntm_count_SPS )


write.csv(BRA_UNCTAD_SPS_count, "BRA/IVs/IV_SPS_UNCTAD.csv")


###################################################################################











# a) UNCTAD Version TBT
rm(list = ls())
#load data:
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD/dta_UNCTAD_TBT.csv")



################################################################################

# b) select neighboring countries as an IV 


#Get the countries of interest 
# names(dta_UNCTAD_TBT)
# unique(dta_UNCTAD_TBT$Reporter)
# unique(dta_UNCTAD_TBT$Partner)
# table(dta_UNCTAD_TBT$Reporter, dta_UNCTAD_TBT$Year)


#create a dataset with only countries of interest
country_data <- list()
countries <- c("BRA", "CHL","BOL", "COL","ECU", "PER")
for (country in countries) {
  # Subset the data for the current country
  country_subset <- subset(dta_UNCTAD_TBT, Reporter == country, select = c(Year, Partner, HSCode, ntm_all))
  
  # Rename the ntm_all column to NTM_COUNTRY_TBT
  country_subset <- country_subset %>%
    rename(!!paste0("NTM_", country, "_TBT") := ntm_all)
  
  # Filter data for the specified year
  #country_subset <- filter(country_subset, Year == 2015)
  
  # Group by Year, Partner, and HSCode and summarize
  country_summary <- country_subset %>%
    group_by(Year, Partner, HSCode) %>%
    summarise(!!paste0("total_NTM_", country, "_TBT") := sum(!!rlang::sym(paste0("NTM_", country, "_TBT")), na.rm = TRUE)) %>%
    ungroup() 
  # Store the subset in the country_data list
  country_data[[country]] <- country_summary
}
# Perform full join on all data frames in the list
merged_UNCTAD_NTM <- Reduce(full_join, country_data)

#   #if only select partner as WLD
#   merged_UNCTAD_NTM <- subset(merged_UNCTAD_NTM,Partner =="WLD" )
#   colSums(is.na(merged_UNCTAD_NTM))
#   length(unique(merged_UNCTAD_NTM$HSCode))

# put value 0 instead of NA 
colSums(is.na(merged_UNCTAD_NTM))
merged_UNCTAD_NTM <- merged_UNCTAD_NTM %>% mutate_all(~replace_na(., 0))

# Get HS levels variables
merged_UNCTAD_NTM$H1<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 1)
merged_UNCTAD_NTM$H2<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 2)
merged_UNCTAD_NTM$H3<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 3)
merged_UNCTAD_NTM$H4<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 4)
merged_UNCTAD_NTM$H5<- substring(merged_UNCTAD_NTM$HSCode, first = 1, last = 5)



# Can aggregate NTM counts at HS 5 level:

names(merged_UNCTAD_NTM)

merged_UNCTAD_NTM  <- merged_UNCTAD_NTM %>% group_by(Year, Partner, HSCode) %>%
  summarise(total_NTM_BRA_TBT = sum (total_NTM_BRA_TBT),
            total_NTM_CHL_TBT = sum (total_NTM_CHL_TBT),
            total_NTM_BOL_TBT = sum (total_NTM_BOL_TBT),
            total_NTM_COL_TBT = sum (total_NTM_COL_TBT),
            total_NTM_ECU_TBT = sum (total_NTM_ECU_TBT),
            total_NTM_PER_TBT = sum (total_NTM_PER_TBT))


merged_UNCTAD_NTM <-merged_UNCTAD_NTM %>% rename(PartnerISO3 =Partner)
length(unique(merged_UNCTAD_NTM$PartnerISO3))


# get years from 2012 to 2016
merged_UNCTAD_NTM <- subset(merged_UNCTAD_NTM, Year< 2017)

# need to get rid of WLD as partner:
table(merged_UNCTAD_NTM$PartnerISO3)



BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge.csv")

WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- c(unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]), 
                    unique(unctad_data$PartnerISO3[unctad_data$PartnerISO3 != "WLD"]))  # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'PartnerISO3' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where PartnerISO3 is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
} #By hand at the bottom 

# Call the function with the provided dataframes
TBT_UNCTAD <- WLD_partner(BRA_trade_Merge, merged_UNCTAD_NTM)
names(TBT_UNCTAD)

#calculate counts of NTM by partners and HS code 
TBT_UNCTAD <- TBT_UNCTAD %>%  group_by(Year, PartnerISO3, HSCode) %>%
  summarise(total_NTM_BRA_TBT = sum(total_NTM_BRA_TBT, na.rm = TRUE),
            total_NTM_CHL_TBT = sum(total_NTM_CHL_TBT, na.rm = TRUE),
            total_NTM_BOL_TBT = sum(total_NTM_BOL_TBT, na.rm = TRUE),
            total_NTM_COL_TBT = sum(total_NTM_COL_TBT, na.rm = TRUE),
            total_NTM_ECU_TBT = sum(total_NTM_ECU_TBT, na.rm = TRUE),
            total_NTM_PER_TBT = sum(total_NTM_PER_TBT, na.rm = TRUE))


colSums(is.na(TBT_UNCTAD))
#  summary(BRA_UNCTAD_TBT_count$total_NTM_BRA_TBT)
#  # check if duplicates
#  duplicates <- BRA_UNCTAD_TBT_count[duplicated(BRA_UNCTAD_TBT_count), ]



################################################################################

# Multiply by weights 



Weighting_trade <-  read_csv("BRA/IVs/Weighting_trade.csv")
names(Weighting_trade)
names(merged_UNCTAD_NTM)
class(Weighting_trade$HSCode)

weighted_NTM  <- left_join(merged_UNCTAD_NTM, Weighting_trade)
colSums(is.na(weighted_NTM))





################################################################################

# run the first IV regression

################################################################################


# Get HS levels variables
TBT_UNCTAD$H1<- substring(TBT_UNCTAD$HSCode, first = 1, last = 1)
TBT_UNCTAD$H2<- substring(TBT_UNCTAD$HSCode, first = 1, last = 2)
TBT_UNCTAD$H3<- substring(TBT_UNCTAD$HSCode, first = 1, last = 3)
TBT_UNCTAD$H4<- substring(TBT_UNCTAD$HSCode, first = 1, last = 4)




names(TBT_UNCTAD)

length(unique(TBT_UNCTAD$HSCode))
#re run the regression for each H2
unique_H1 <- unique(TBT_UNCTAD$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()



# Run the regression for each H1 value 
for (h2_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_UNCTAD <- subset(TBT_UNCTAD, H1 == h2_val)
  
  # Run the regression
  # reg_H5 <- feglm(total_NTM_BRA_TBT ~ 0+  total_NTM_CHL_TBT + total_NTM_BOL_TBT + total_NTM_COL_TBT +
  #                   total_NTM_ECU_TBT + total_NTM_PER_TBT,
  #                 data = sub_Merge_UNCTAD, family = poisson(link = "log"))
  
  
  reg_H5 <- feols(total_NTM_BRA_TBT ~ 0+  total_NTM_CHL_TBT + total_NTM_BOL_TBT + total_NTM_COL_TBT +
                    total_NTM_ECU_TBT + total_NTM_PER_TBT,
                  data = sub_Merge_UNCTAD)
  
  # Store the summary of regression
  reg_summaries[[h2_val]] <- summary(reg_H5)
}




# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}

# Loop through all unique values of H1
all_sub_Merge_UNCTAD <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_UNCTAD <- subset(TBT_UNCTAD, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_UNCTAD$Year,
                        PartnerISO3 = sub_Merge_UNCTAD$PartnerISO3,
                        HSCode = sub_Merge_UNCTAD$HSCode,
                        total_NTM_BRA_TBT = sub_Merge_UNCTAD$total_NTM_BRA_TBT,
                        total_NTM_CHL_TBT = sub_Merge_UNCTAD$total_NTM_CHL_TBT,
                        total_NTM_BOL_TBT = sub_Merge_UNCTAD$total_NTM_BOL_TBT,
                        total_NTM_COL_TBT = sub_Merge_UNCTAD$total_NTM_COL_TBT,
                        total_NTM_ECU_TBT = sub_Merge_UNCTAD$total_NTM_ECU_TBT,
                        total_NTM_PER_TBT = sub_Merge_UNCTAD$total_NTM_PER_TBT)
  
  # Get the predicted values
  predicted_values <- predict(reg_summaries[[h1_val]], newdata, type = "response")
  
  # Add the predicted values as a new column to the original data frame
  sub_Merge_UNCTAD$predict_BRA <- round(predicted_values, digits = 0)
  all_sub_Merge_UNCTAD <- rbind(all_sub_Merge_UNCTAD, sub_Merge_UNCTAD)
}


BRA_UNCTAD_TBT_count <- all_sub_Merge_UNCTAD %>% rename(ntm_count_TBT = predict_BRA)
BRA_UNCTAD_TBT_count <- select(BRA_UNCTAD_TBT_count, Year , PartnerISO3,HSCode, ntm_count_TBT )


write.csv(BRA_UNCTAD_TBT_count, "BRA/IVs/IV_TBT_UNCTAD.csv")



