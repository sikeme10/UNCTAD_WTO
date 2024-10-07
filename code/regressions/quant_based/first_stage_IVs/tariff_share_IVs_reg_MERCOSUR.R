

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
library(countrycode)

###############################################################################


# Structure:


################################################################################

# 1) Load the data 
X_vars <- fread("/data/sikeme/TRADE/WTO/data/quant_based/merge2.csv")
import_tariff <- fread("/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/share_tariff.csv")


################################################################################


# A) For import tariff shares IV regressions


################################################################################

names(import_tariff)


# select vars of interest to match it 
X_vars <- X_vars %>% select(ReporterISO3, PartnerISO3,ProductCode, Year, contig, 
                            dist, rta, comlang_off, Reporter_GDP, Partner_GDP )

################################################################################

# test on a coutry

test <- import_tariff %>% mutate(ReporterISO3 = "BRA")

test2 <- left_join(test, X_vars, by = c("Year", "ProductCode", "PartnerISO3", "ReporterISO3"))

names(test)
names(X_vars)
colSums(is.na(test2))
class(X_vars$ProductCode)
class((test2$ProductCode))
table(import_tariff$Year)

test3 <- test2 %>% filter(is.na(Reporter_GDP))

X_vars2 <- X_vars %>% filter(ReporterISO3 == "BRA")

################################################################################

# Need to drop some reporters to be able to run the regression
table(import_tariff$PartnerISO3)

import_tariff <- import_tariff %>% filter(PartnerISO3 != "WLD")



################################################################################
# Have to run the regression for each country (to get the IVs)
# if not part of MECOSUR than run it on all countries 
# if part of MECOSUR Run it on all countries aside from MECORSUR 
# need t merge the data with the other gravity variables created from X_var


share_tariff_vars <- c("share_tariff_BRA", "share_tariff_ARG", "share_tariff_BOL", "share_tariff_CHL", "share_tariff_COL", 
                 "share_tariff_ECU", "share_tariff_PRY", "share_tariff_PER", "share_tariff_URY")

# Variables to drop from the right-hand side if the dependent variable is part of this list
non_MERCOSUR <- c("share_tariff_BOL", "share_tariff_CHL", "share_tariff_COL", "share_tariff_ECU", "share_tariff_PER")

MERCOSUR <- c("share_tariff_BRA", "share_tariff_ARG",  "share_tariff_PRY",  "share_tariff_URY")

controls_var <- c("contig", "dist", "comlang_off", "Reporter_GDP", "Partner_GDP", "rta")


colSums(is.na(X_vars))



# Loop through each tariff variable and run the regression
models <- list()

X_vars2 <-  X_vars %>% distinct(Year, ProductCode, ReporterISO3, PartnerISO3, .keep_all = TRUE)
names(X_vars2)


# Identify duplicated rows based on specified columns
duplicated_rows <- X_vars[duplicated(X_vars[, c("Year", "ProductCode", "ReporterISO3", "PartnerISO3")]), ]

import_tariff1 <- import_tariff

class(import_tariff1$ProductCode)
class(X_vars2$ProductCode)


for (var in share_tariff_vars) {
  predictions <- data.frame()
  # If the dependent variable is from MERCOSUR, use only non_MERCOSUR variables as independent variables
  if (var %in% MERCOSUR) {
    independent_vars <- non_MERCOSUR
  } else {
    # Otherwise, use all tariff variables except the dependent one
    independent_vars <- share_tariff_vars[share_tariff_vars != var]
  }
  ReporterISO3 <- substr(var, 14, 16) #get reporter name to make it a variable
  
  import_tariff1 <- import_tariff %>%  mutate(ReporterISO3 = ReporterISO3) # so that can be matched to trade data
  import_tariff1 <- left_join(import_tariff1, X_vars2, by = c("Year", "ProductCode", "PartnerISO3", "ReporterISO3"))
  
  # drop if reporter == partner country 
  import_tariff1 <- import_tariff1 %>% filter(ReporterISO3 != PartnerISO3)
  
  # Define formula: dependent variable is var, independent variables are the filtered list
  all_independent_vars <- c(independent_vars, controls_var)
  
  #  formula for the regression
  formula <- as.formula(paste(var, "~", paste(all_independent_vars, collapse = " + ")))  
  
  model <- feols(formula, data = import_tariff1) # run reg
  
  # Store each model in the list
  models[[var]] <- model
  print(summary(models[[var]])) # print summary for each regressions
  
  # tariff5 <- tariff5 %>% 
  #   select(Year, ProductCode, PartnerISO3) %>%
  #   mutate(!!paste0("predicted_", var) := fitted(model))
  # tariff <- left_join(tariff, tariff5, by = c("Year", "ProductCode", "PartnerISO3"))
  
}


# Check the summary of one model (example for tariff_BRA)
summary(model[["tariff_BRA"]])
summary(models[["tariff_COL"]])










































predictions <- data.frame()

# Get the unique years
years <- unique(tariff5$Year)

# Loop through each year
for (yr in years) {
  # Subset data for the current year
  data_year <- tariff5 %>% filter(Year == yr)
  
  # Run the fixed effects regression
  model <- lm(tariff_BRA ~ H2*mean_tariff + contig + dist + comlang_off + rta + 
                landlocked_rep + landlocked_par + Reporter_GDP + Partner_GDP, data = data_year)
  
  # Get the predicted values
  data_year$predicted_tariff_BRA <- predict(model, newdata = data_year)
  # Add year information for merging
  data_year$Year <- yr
  # Append predictions to the predictions data frame
  predictions <- bind_rows(predictions, data_year)
}

# Merge predictions back into the original data frame
tariff5 <- tariff5 %>%
  left_join(predictions %>% select(Year, H1, mean_tariff, contig, dist, comlang_off, rta, 
                                   landlocked_rep, landlocked_par, Reporter_GDP, Partner_GDP, 
                                   predicted_tariff_BRA), by = c("Year", "H1", "mean_tariff", "contig", 
                                                                 "dist", "comlang_off", "rta", 
                                                                 "landlocked_rep", "landlocked_par", 
                                                                 "Reporter_GDP", "Partner_GDP"))

# View the updated dataframe
head(tariff5)



################################################################################

# Avoid multicolinearity issues
zero_counts <- sapply(SPS, function(x) sum(x == 0))
# Filter rows where all specified columns are different from 0
tariff4 <- tariff3 %>%
  filter(!(tariff_BRA == 0 & tariff_CHL == 0 & tariff_BOL == 0 & tariff_COL== 0 & tariff_ECU== 0 
           & tariff_PER == 0))
summary(tariff4)



#re run the regression for each H2
unique_H1 <- unique(tariff4$H1)
length(unique_H1)
# List to store regression results
reg_summaries <- list()



# Run the regression for each H1 value 
for (H1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_tariff4 <- subset(tariff4, H1 == H1_val)
  reg_H5 <- feols(tariff_BRA ~ 0+  tariff_CHL + tariff_BOL + tariff_COL +
                    tariff_ECU + tariff_PER,
                  data = sub_tariff4)
  
  # Store the summary of regression
  reg_summaries[[H1_val]] <- summary(reg_H5)
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


