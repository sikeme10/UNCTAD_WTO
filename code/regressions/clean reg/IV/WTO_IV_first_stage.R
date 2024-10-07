

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


# Load data


################################################################################

# a) WTO Version SPS

#load data:


# a) load data, and filter the years of interest:

#  
#  
#  for (country in countries) {
#    # Construct file paths for SPS and TBT data
#    sps_file_path <- paste0("dta_WTO/countries/clean_WTO_SPS_", country, ".csv")
#    tbt_file_path <- paste0("dta_WTO/countries/clean_WTO_TBT_", country, ".csv")
#    
#    # Read the data and assign to dynamically created variables
#    if (file.exists(sps_file_path)) {
#      assign(paste0("dta_WTO_SPS_", country), read.csv(sps_file_path))
#      # Filter for years 2012 to 2016
#      assign(paste0("dta_WTO_SPS_", country), get(paste0("dta_WTO_SPS_", country)) %>%
#               filter(Year >= 2012 & Year <= 2016))
#    } else {
#      warning(paste("SPS file for", country, "does not exist at", sps_file_path))
#    }
#    
#    if (file.exists(tbt_file_path)) {
#      assign(paste0("dta_WTO_TBT_", country), read.csv(tbt_file_path))
#      # Filter for years 2012 to 2016
#      assign(paste0("dta_WTO_TBT_", country), get(paste0("dta_WTO_TBT_", country)) %>%
#               filter(Year >= 2012 & Year <= 2016))
#    } else {
#      warning(paste("TBT file for", country, "does not exist at", tbt_file_path))
#    }
#  }
#  





# 1) load data of interest and only keep years of interests

countries <- c("BRA", "CHL","BOL", "COL","ECU", "PER")

for (country in countries) {
  # Construct file paths for SPS and TBT data
  sps_file_path <- paste0("dta_WTO/countries/clean_WTO_SPS_", country, ".csv")
  tbt_file_path <- paste0("dta_WTO/countries/clean_WTO_TBT_", country, ".csv")
  
  # Read the data and assign to dynamically created variables
  if (file.exists(sps_file_path)) {
    sps_data <- read.csv(sps_file_path)
    # Filter for years 2012 to 2016
    sps_data <- sps_data %>%
      filter(Year >= 2012 & Year <= 2016)
    # Rename SPS1 variable
    colnames(sps_data)[colnames(sps_data) == "SPS1"] <- paste0("SPS1_", country)
    # Assign filtered and renamed data frame
    assign(paste0("dta_WTO_SPS_", country), sps_data)
  } else {
    warning(paste("SPS file for", country, "does not exist at", sps_file_path))
  }
  
  if (file.exists(tbt_file_path)) {
    tbt_data <- read.csv(tbt_file_path)
    # Filter for years 2012 to 2016
    tbt_data <- tbt_data %>%
      filter(Year >= 2012 & Year <= 2016)
    # Rename SPS1 variable
    colnames(tbt_data)[colnames(tbt_data) == "TBT1"] <- paste0("TBT1_", country)
    
    # Assign filtered data frame
    assign(paste0("dta_WTO_TBT_", country), tbt_data)
  } else {
    warning(paste("TBT file for", country, "does not exist at", tbt_file_path))
  }
}



################################################################################

# 2) join all data: 

# a) SPS case

drop_vars <- function(df) {
  df %>% select(-ReporterISO3,-IMP, -AFF, -SPS_count)
}

dfs_to_join <- list(dta_WTO_SPS_BRA, dta_WTO_SPS_BOL, dta_WTO_SPS_CHL, dta_WTO_SPS_COL, dta_WTO_SPS_ECU,dta_WTO_SPS_PER)
#drop some of the vars
dfs_to_join <- lapply(dfs_to_join, drop_vars)
# Perform full join on the list of data frames
dta_WTO_SPS <- Reduce(function(x, y) full_join(x, y, by = c("Year", "ProductCode"  ,"PartnerISO3")), dfs_to_join)

#checks
table(dta_WTO_SPS$PartnerISO3)
colSums(is.na(dta_WTO_SPS))
summary(dta_WTO_SPS)


dta_WTO_SPS <- mutate_all(dta_WTO_SPS, ~ifelse(is.na(.), 0, .))




# b) TBT case

drop_vars <- function(df) {
  df %>% select(-ReporterISO3, -IMP, -AFF, -TBT_count)
}
dfs_to_join <- list(dta_WTO_TBT_BRA, dta_WTO_TBT_BOL, dta_WTO_TBT_CHL, dta_WTO_TBT_COL, dta_WTO_TBT_ECU,dta_WTO_TBT_PER)
#drop some of the vars
dfs_to_join <- lapply(dfs_to_join, drop_vars)
# Perform full join on the list of data frames
dta_WTO_TBT <- Reduce(function(x, y) full_join(x, y, by = c("Year", "ProductCode" ,"PartnerISO3")), dfs_to_join)

#checks
table(dta_WTO_TBT$PartnerISO3)
colSums(is.na(dta_WTO_TBT))
summary(dta_WTO_TBT)


dta_WTO_TBT <- mutate_all(dta_WTO_TBT, ~ifelse(is.na(.), 0, .))


# remove unnecessary dataframes
remove_df <- c("dta_WTO_SPS_BRA", "dta_WTO_SPS_BOL", "dta_WTO_SPS_CHL", "dta_WTO_SPS_COL", "dta_WTO_SPS_ECU","dta_WTO_SPS_PER",
                 "dta_WTO_TBT_BRA", "dta_WTO_TBT_BOL", "dta_WTO_TBT_CHL", "dta_WTO_TBT_COL", "dta_WTO_TBT_ECU","dta_WTO_TBT_PER")

rm(list = remove_df)

###############################################################################

# WORLD as Affected countries 
# redistributed it to all countries 

names(dta_WTO_TBT)
names(dta_WTO_SPS)

BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge.csv")

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
    summarise(SPS1_BRA = sum(SPS1_BRA),
              SPS1_BOL = sum(SPS1_BOL),
              SPS1_CHL = sum(SPS1_CHL),
              SPS1_COL = sum(SPS1_COL),
              SPS1_ECU = sum(SPS1_ECU),
              SPS1_PER = sum(SPS1_PER))
  return(combined_data)
}
SPS <- WLD_partner(BRA_trade_Merge, dta_WTO_SPS)
write.csv(SPS, "BRA/IVs/cleaning_SPS.csv")

# checks
summary(SPS)
length(unique(SPS$ProductCode))
length(unique(SPS$PartnerISO3))
duplicated_rows <- duplicated(SPS[c("Year", "ProductCode","PartnerISO3")])


# for TBT 



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
    summarise(TBT1_BRA = sum(TBT1_BRA),
              TBT1_BOL = sum(TBT1_BOL),
              TBT1_CHL = sum(TBT1_CHL),
              TBT1_COL = sum(TBT1_COL),
              TBT1_ECU = sum(TBT1_ECU),
              TBT1_PER = sum(TBT1_PER))
  return(combined_data)
}
TBT <- WLD_partner_TBT(BRA_trade_Merge, dta_WTO_TBT)
write.csv(TBT, "BRA/IVs/cleaning_TBT.csv")


# checks
summary(TBT)
length(unique(TBT$ProductCode))
length(unique(TBT$PartnerISO3))



###############################################################################

# LOAD 

TBT <- read_csv("BRA/IVs/cleaning_TBT.csv")

SPS <- read_csv("BRA/IVs/cleaning_SPS.csv")

names(SPS)

# change HS codes to character vars
class(SPS$ProductCode)
SPS$ProductCode <- as.character(SPS$ProductCode)
SPS$ProductCode <- ifelse(nchar(SPS$ProductCode) == 5 & !is.na(SPS$ProductCode), paste0("0", SPS$ProductCode), SPS$ProductCode)
# to check:  all_six_chars <- all(nchar(SPS$ProductCode) == 6)

class(TBT$ProductCode)
TBT$ProductCode <- as.character(TBT$ProductCode)
TBT$ProductCode <- ifelse(nchar(TBT$ProductCode) == 5 & !is.na(TBT$ProductCode), paste0("0", TBT$ProductCode), TBT$ProductCode)
# to check:  all_six_chars <- all(nchar(TBT$ProductCode) == 6)

###############################################################################
# Regressions



# Get HS levels variables
SPS$H1<- substring(SPS$ProductCode, first = 1, last = 1)
SPS$H2<- substring(SPS$ProductCode, first = 1, last = 2)
SPS$H3<- substring(SPS$ProductCode, first = 1, last = 3)
SPS$H4<- substring(SPS$ProductCode, first = 1, last = 4)


# Avoid multicolinearity issues
zero_counts <- sapply(SPS, function(x) sum(x == 0))
# Filter rows where all specified columns are different from 0
SPS1 <- SPS %>%
  filter(!(SPS1_BRA == 0 & SPS1_CHL == 0 & SPS1_BOL == 0 & SPS1_COL== 0 & SPS1_ECU== 0 & SPS1_PER== 0))
summary(SPS1)



#re run the regression for each H2
unique_H1 <- unique(SPS1$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()



# Run the regression for each H1 value 
for (H1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(SPS1, H1 == H1_val)
  reg_H5 <- feols(SPS1_BRA ~ 0+  SPS1_CHL + SPS1_BOL + SPS1_COL +
                    SPS1_ECU + SPS1_PER,
                  data = sub_Merge_WTO)
  
  # Store the summary of regression
  reg_summaries[[H1_val]] <- summary(reg_H5)
}




# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}

# Loop through all unique values of H1
all_sub_Merge_WTO <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(SPS1, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_WTO$Year,
                        PartnerISO3 = sub_Merge_WTO$PartnerISO3,
                        ProductCode = sub_Merge_WTO$ProductCode,
                        SPS1_BRA = sub_Merge_WTO$SPS1_BRA,
                        SPS1_CHL = sub_Merge_WTO$SPS1_CHL,
                        SPS1_BOL = sub_Merge_WTO$SPS1_BOL,
                        SPS1_COL = sub_Merge_WTO$SPS1_COL,
                        SPS1_ECU = sub_Merge_WTO$SPS1_ECU,
                        SPS1_PER = sub_Merge_WTO$SPS1_PER)
  
  # Get the predicted values
  predicted_values <- predict(reg_summaries[[h1_val]], newdata, type = "response")
  
  # Add the predicted values as a new column to the original data frame
  sub_Merge_WTO$predict_BRA <- round(predicted_values, digits = 0)
  all_sub_Merge_WTO <- rbind(all_sub_Merge_WTO, sub_Merge_WTO)
}


BRA_WTO_SPS_count <- all_sub_Merge_WTO %>% rename(ntm_count_SPS = predict_BRA)
BRA_WTO_SPS_count <- select(BRA_WTO_SPS_count, Year , PartnerISO3, ProductCode, ntm_count_SPS )


write.csv(BRA_WTO_SPS_count, "BRA/IVs/IV_SPS_WTO.csv")



################################################################################


# for TBT 


################################################################################




# Get HS levels variables
TBT$H1<- substring(TBT$ProductCode, first = 1, last = 1)
TBT$H2<- substring(TBT$ProductCode, first = 1, last = 2)
TBT$H3<- substring(TBT$ProductCode, first = 1, last = 3)
TBT$H4<- substring(TBT$ProductCode, first = 1, last = 4)


# Avoid multicolinearity issues
zero_counts <- sapply(TBT, function(x) sum(x == 0))
# Filter rows where all specified columns are different from 0
TBT1 <- TBT %>%
  filter(!(TBT1_BRA == 0 & TBT1_CHL == 0 & TBT1_BOL == 0 & TBT1_COL== 0 & TBT1_ECU== 0 & TBT1_PER== 0))
summary(TBT1)



#re run the regression for each H2
unique_H1 <- unique(TBT1$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()



# Run the regression for each H1 value 
for (H1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(TBT1, H1 == H1_val)
  reg_H5 <- feols(TBT1_BRA ~ 0+  TBT1_CHL + TBT1_BOL + TBT1_COL +
                    TBT1_ECU + TBT1_PER,
                  data = sub_Merge_WTO)
  
  # Store the summary of regression
  reg_summaries[[H1_val]] <- summary(reg_H5)
}




# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(reg_summaries[[i]])
}

# Loop through all unique values of H1
all_sub_Merge_WTO <- data.frame()
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO <- subset(TBT1, H1 == h1_val)
  
  # Create a new data frame with the necessary columns
  newdata <- data.frame(Year = sub_Merge_WTO$Year,
                        PartnerISO3 = sub_Merge_WTO$PartnerISO3,
                        ProductCode = sub_Merge_WTO$ProductCode,
                        TBT1_BRA = sub_Merge_WTO$TBT1_BRA,
                        TBT1_CHL = sub_Merge_WTO$TBT1_CHL,
                        TBT1_BOL = sub_Merge_WTO$TBT1_BOL,
                        TBT1_COL = sub_Merge_WTO$TBT1_COL,
                        TBT1_ECU = sub_Merge_WTO$TBT1_ECU,
                        TBT1_PER = sub_Merge_WTO$TBT1_PER)
  
  # Get the predicted values
  predicted_values <- predict(reg_summaries[[h1_val]], newdata, type = "response")
  
  # Add the predicted values as a new column to the original data frame
  sub_Merge_WTO$predict_BRA <- round(predicted_values, digits = 0)
  all_sub_Merge_WTO <- rbind(all_sub_Merge_WTO, sub_Merge_WTO)
}


BRA_WTO_TBT_count <- all_sub_Merge_WTO %>% rename(ntm_count_TBT = predict_BRA)
BRA_WTO_TBT_count <- select(BRA_WTO_TBT_count, Year , PartnerISO3, ProductCode, ntm_count_TBT )


write.csv(BRA_WTO_TBT_count, "BRA/IVs/IV_TBT_WTO.csv")







