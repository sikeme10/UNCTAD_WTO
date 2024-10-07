

################################################################################


# Data structuration and IV regressiong (first staget)


################################################################################


# Structure of the code 
# 1) Load data
# 2)
# 3) 




################################################################################

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
library(dplyr)
library(purrr)







# 1) load data, and filter the years of interest:

dta_TBT <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WTO/clean_WTO_TBT.csv")
dta_SPS <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WTO/clean_WTO_SPS.csv")

# checks:
names(dta_SPS)
table(dta_SPS$ReporterISO3)


################################################################################


# 2) Select countries for IVs and create a data frame

countries <- c("BRA", "CHL", "BOL", "COL", "ECU", "PER")
# separate the original data frame by country and harmonize 
for (country in countries) {
  
  # Filter, rename, and select for SPS
  assign(paste0(country, "_SPS"), dta_SPS %>% 
           filter(ReporterISO3 == country & Year >= 2012 & Year <= 2016) %>% 
           rename(!!paste0(country, "_SPS_count") := SPS1) %>% 
           select(-SPS_count, -IMP, -ReporterISO3))
  
  # Filter, rename, and select for TBT
  assign(paste0(country, "_TBT"), dta_TBT %>% 
           filter(ReporterISO3 == country & Year >= 2012 & Year <= 2016) %>% 
           rename(!!paste0(country, "_TBT_count") := TBT1) %>% 
           select(-TBT_count, -IMP, -ReporterISO3))
}

# Merge all of them 
# Create a list of TBT data frames
tbt_list <- lapply(countries, function(country) {
  get(paste0(country, "_TBT"))
})

# Full join all TBT data frames in the list
IV_panel_TBT <- reduce(tbt_list, full_join)
colSums(is.na(IV_panel_TBT))
#replace NA by 0
IV_panel_TBT[is.na(IV_panel_TBT)] <- 0

# Create a list of TBT data frames
sps_list <- lapply(countries, function(country) {
  get(paste0(country, "_SPS"))
})

# Full join all TBT data frames in the list
IV_panel_SPS <- reduce(sps_list, full_join)
colSums(is.na(IV_panel_SPS))
#replace NA by 0
IV_panel_SPS[is.na(IV_panel_SPS)] <- 0



################################################################################


#checks
table(IV_panel_SPS$PartnerISO3)
table(IV_panel_TBT$PartnerISO3)
summary(IV_panel_SPS)
summary(IV_panel_TBT)
length(unique(IV_panel_SPS$ProductCode))
length(unique(IV_panel_TBT$ProductCode))

# HS codes check 
all_six_chars <- all(nchar(IV_panel_SPS$ProductCode) == 6)
all_six_chars <- all(nchar(IV_panel_TBT$ProductCode) == 6)

################################################################################
# remove unnecessary dfs: 

all_objects <- ls()
# Specify the objects you want to keep
keep_objects <- c("IV_panel_SPS", "IV_panel_TBT")
# Identify objects to remove
objects_to_remove <- setdiff(all_objects, keep_objects)
# Remove all objects except the specified ones
rm(list = objects_to_remove)


################################################################################

# World as a partner have to change 

# match it with trade data 
BRA_merge <- read_csv("BRA/country_char/BRA_merge.csv")

names(IV_panel_SPS)
table(IV_panel_SPS$PartnerISO3)

# For SPS 
WLD_partner <- function(trade_data, NTM_data) {
  new_partners <- unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]) # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'Partner' is 'WLD' from NTM data
  wld_rows <- filter(NTM_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(NTM_data %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where Partner is 'WLD'
                             bind_rows(new_observations)) 
  combined_data <- combined_data %>% group_by(Year,ProductCode, PartnerISO3) %>%
    summarise(BRA_SPS_count = sum(BRA_SPS_count),
              BOL_SPS_count = sum(BOL_SPS_count),
              CHL_SPS_count = sum(CHL_SPS_count),
              COL_SPS_count = sum(COL_SPS_count),
              ECU_SPS_count = sum(ECU_SPS_count),
              PER_SPS_count = sum(PER_SPS_count))
  return(combined_data)
}
IV_panel_SPS1 <- WLD_partner(BRA_merge, IV_panel_SPS)
write.csv(IV_panel_SPS1, "BRA/IVs/cleaning_SPS.csv")



# For TBT 
names(IV_panel_TBT)
table(IV_panel_TBT$PartnerISO3)

WLD_partner_TBT <- function(trade_data, NTM_data) {
  new_partners <- unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]) # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'Partner' is 'WLD' from NTM data
  wld_rows <- filter(NTM_data, PartnerISO3 == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, PartnerISO3 = partner)})
  combined_data <- bind_rows(NTM_data %>%
                               filter(PartnerISO3 != "WLD"), # Remove rows where Partner is 'WLD'
                             bind_rows(new_observations)) 
  combined_data <- combined_data %>% group_by(Year,ProductCode, PartnerISO3) %>%
    summarise(BRA_TBT_count = sum(BRA_TBT_count),
              BOL_TBT_count = sum(BOL_TBT_count),
              CHL_TBT_count = sum(CHL_TBT_count),
              COL_TBT_count = sum(COL_TBT_count),
              ECU_TBT_count = sum(ECU_TBT_count),
              PER_TBT_count = sum(PER_TBT_count))
  return(combined_data)
}
IV_panel_TBT1 <- WLD_partner_TBT(BRA_merge, IV_panel_TBT)
write.csv(IV_panel_TBT1, "BRA/IVs/cleaning_TBT.csv")


################################################################################

# 3) Get gravity characteristics and merge 
rm(BRA_merge)

BRA_merge <- read_csv("BRA/country_char/BRA_merge.csv")

names(BRA_merge)
names( IV_panel_SPS)
BRA_merge <- BRA_merge %>%select(PartnerISO3, Year, ProductCode, contig, dist, comlang_off, rta, 
         landlocked_rep, landlocked_par, Reporter_GDP, Partner_GDP)


IV_panel_SPS1 <-  left_join(IV_panel_SPS, BRA_merge)

################################################################################


# Regressions


################################################################################

IV_panel_SPS <- IV_panel_SPS


# Get HS levels variables
IV_panel_SPS1$H1<- substring(IV_panel_SPS1$ProductCode, first = 1, last = 1)
IV_panel_SPS1$H2<- substring(IV_panel_SPS1$ProductCode, first = 1, last = 2)
IV_panel_SPS1$H3<- substring(IV_panel_SPS1$ProductCode, first = 1, last = 3)
IV_panel_SPS1$H4<- substring(IV_panel_SPS1$ProductCode, first = 1, last = 4)


# Avoid multicolinearity issues
zero_counts <- sapply(IV_panel_SPS1, function(x) sum(x == 0))
# Filter rows where all specified columns are different from 0
IV_panel_SPS1 <- IV_panel_SPS1 %>%
  filter(!(SPS1_BRA == 0 & SPS1_CHL == 0 & SPS1_BOL == 0 & SPS1_COL== 0 & SPS1_ECU== 0 & SPS1_PER== 0))
summary(SPS1)



#re run the regression for each H2
unique_H1 <- unique(IV_panel_SPS1$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()

names(IV_panel_SPS1)

# Run the regression for each H1 value 
for (H1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(IV_panel_SPS1, H1 == H1_val)
  reg_H5 <- feols(BRA_SPS_count ~ 0+  CHL_SPS_count + BOL_SPS_count + COL_SPS_count +
                    ECU_SPS_count + PER_SPS_count,
                  data = sub_Merge_WTO)
  
  # Store the summary of regression
  reg_summaries[[H1_val]] <- summary(reg_H5)
}




# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}


summary_table <- etable(reg_summaries[[1]], reg_summaries[[2]], reg_summaries[[3]],
                        reg_summaries[[4]], reg_summaries[[5]], reg_summaries[[6]],
                        reg_summaries[[7]], reg_summaries[[8]], reg_summaries[[9]],
                        reg_summaries[[10]],headers = "WTO IV first stage")



# Loop through all unique values of H1
all_sub_Merge_WTO <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(IV_panel_SPS1, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_WTO$Year,
                        PartnerISO3 = sub_Merge_WTO$PartnerISO3,
                        ProductCode = sub_Merge_WTO$ProductCode,
                        BRA_SPS_count = sub_Merge_WTO$BRA_SPS_count,
                        CHL_SPS_count = sub_Merge_WTO$CHL_SPS_count,
                        BOL_SPS_count = sub_Merge_WTO$BOL_SPS_count,
                        COL_SPS_count = sub_Merge_WTO$COL_SPS_count,
                        ECU_SPS_count = sub_Merge_WTO$ECU_SPS_count,
                        PER_SPS_count = sub_Merge_WTO$PER_SPS_count)
  
  # Get the predicted values
  predicted_values <- predict(reg_summaries[[h1_val]], newdata, type = "response")
  
  # Add the predicted values as a new column to the original data frame
  sub_Merge_WTO$predict_BRA <- round(predicted_values, digits = 0)
  all_sub_Merge_WTO <- rbind(all_sub_Merge_WTO, sub_Merge_WTO)
}

names(all_sub_Merge_WTO)

BRA_WTO_SPS_count <- all_sub_Merge_WTO %>% rename(ntm_count_SPS = predict_BRA)
BRA_WTO_SPS_count <- select(BRA_WTO_SPS_count, Year , PartnerISO3, ProductCode, ntm_count_SPS )



write.csv(BRA_WTO_SPS_count, "BRA/IVs/IV_SPS_WTO.csv")


################################################################################

# for TBT 







# Get HS levels variables
IV_panel_TBT1$H1<- substring(IV_panel_TBT1$ProductCode, first = 1, last = 1)
IV_panel_TBT1$H2<- substring(IV_panel_TBT1$ProductCode, first = 1, last = 2)
IV_panel_TBT1$H3<- substring(IV_panel_TBT1$ProductCode, first = 1, last = 3)
IV_panel_TBT1$H4<- substring(IV_panel_TBT1$ProductCode, first = 1, last = 4)


# Avoid multicolinearity issues
zero_counts <- sapply(IV_panel_TBT1, function(x) sum(x == 0))
# Filter rows where all specified columns are different from 0
TBT1 <- TBT %>%
  filter(!(TBT1_BRA == 0 & TBT1_CHL == 0 & TBT1_BOL == 0 & TBT1_COL== 0 & TBT1_ECU== 0 & TBT1_PER== 0))
summary(TBT1)



#re run the regression for each H2
unique_H1 <- unique(IV_panel_TBT1$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()



# Run the regression for each H1 value 
for (H1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(IV_panel_TBT1, H1 == H1_val)
  reg_H5 <- feols(BRA_TBT_count ~ 0+  CHL_TBT_count + BOL_TBT_count + COL_TBT_count +
                    ECU_TBT_count + PER_TBT_count,
                  data = sub_Merge_WTO)
  
  # Store the summary of regression
  reg_summaries[[H1_val]] <- summary(reg_H5)
}




# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}

summary_table <- etable(reg_summaries[[1]], reg_summaries[[2]], reg_summaries[[3]],
                        reg_summaries[[4]], reg_summaries[[5]], reg_summaries[[6]],
                        reg_summaries[[7]], reg_summaries[[8]], reg_summaries[[9]],
                        reg_summaries[[10]],headers = "WTO IV first stage")




# Loop through all unique values of H1
all_sub_Merge_WTO <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(IV_panel_TBT1, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_WTO$Year,
                        PartnerISO3 = sub_Merge_WTO$PartnerISO3,
                        ProductCode = sub_Merge_WTO$ProductCode,
                        BRA_TBT_count = sub_Merge_WTO$BRA_TBT_count,
                        CHL_TBT_count = sub_Merge_WTO$CHL_TBT_count,
                        BOL_TBT_count = sub_Merge_WTO$BOL_TBT_count,
                        COL_TBT_count = sub_Merge_WTO$COL_TBT_count,
                        ECU_TBT_count = sub_Merge_WTO$ECU_TBT_count,
                        PER_TBT_count = sub_Merge_WTO$PER_TBT_count)
  
  # Get the predicted values
  predicted_values <- predict(reg_summaries[[h1_val]], newdata, type = "response")
  
  # Add the predicted values as a new column to the original data frame
  sub_Merge_WTO$predict_BRA <- round(predicted_values, digits = 0)
  all_sub_Merge_WTO <- rbind(all_sub_Merge_WTO, sub_Merge_WTO)
}


BRA_WTO_TBT_count <- all_sub_Merge_WTO %>% rename(ntm_count_TBT = predict_BRA)
BRA_WTO_TBT_count <- select(BRA_WTO_TBT_count, Year , PartnerISO3, ProductCode, ntm_count_TBT )


write.csv(BRA_WTO_TBT_count, "BRA/IVs/IV_TBT_WTO.csv")







