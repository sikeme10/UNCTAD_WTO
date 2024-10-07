

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
share_tariff <- fread("/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/share_tariff.csv")
share_tariff <- share_tariff[,-1]

################################################################################


# A) For import tariff shares IV regressions


################################################################################

names(share_tariff)
names(X_vars)

# select vars of interest to match it 
X_vars <- X_vars %>% select(ReporterISO3, PartnerISO3,ProductCode, Year, contig, 
                            dist, rta, comlang_off, Reporter_GDP, Partner_GDP )

################################################################################





################################################################################

# Need to drop some reporters to be able to run the regression
table(share_tariff$PartnerISO3)
table(share_tariff$ReporterISO3)


share_tariff <- share_tariff %>% filter(PartnerISO3 != "WLD")


#drop possible duplicates
X_vars2 <-  X_vars %>% distinct(Year, ProductCode, ReporterISO3, PartnerISO3, .keep_all = TRUE)
names(X_vars2)

#check Product codes
class(share_tariff$ProductCode)
class(X_vars2$ProductCode)



share_tariff1 <- share_tariff


################################################################################
# Have to run the regression for each country (to get the IVs)
# if not part of MECOSUR than run it on all countries 
# if part of MECOSUR Run it on all countries aside from MECORSUR 
# need t merge the data with the other gravity variables created from X_var


countries <- unique(share_tariff$ReporterISO3)
# control variables:
controls <- c("contig", "dist", "rta", "comlang_off", "Reporter_GDP", "Partner_GDP")
IVs <- c("import_mean_tariff_IV")

# Loop through each tariff variable and run the regression
models <- list()
reg_summaries <- list()
predicted_values_list <- list()  # List 
f_statistics <- list()

# For import

for (country in countries) {
  # Filter the data for the current country
  dta <- share_tariff1 %>% filter(ReporterISO3 == country)
  dta <- dta %>% filter(ReporterISO3 != PartnerISO3)
  
  # merge with gravity vars data 
  dta <- left_join(dta, X_vars2, by = c("ReporterISO3", "PartnerISO3", "ProductCode", "Year"))
  
  # Create formula for the regression
  formula <- as.formula(paste("import_tariff ~", paste(c(controls, IVs), collapse = " + ")))
  
  # Run the regression
  model <- feols(formula, data = dta)
  
  # Store the model in the list
  models[[country]] <- model
  reg_summaries[[country]] <- summary(model)
  
  # get predicted values'  
  dta$predicted_import_tariff <- predict(model, newdata = dta)
  
  # get f stat 
  f_statistics[[country]] <- fitstat(model, "f") 
  
  # Store the predicted values in the list
  predicted_values_list[[country]] <- dta
  
  print((models[[country]])) # print summary for each regressions
}

# store f stat results:f_stats <- c()
f_stats <- c("F stat")
f_p_vals <- c("F pval")

# Loop through each country in the f_statistics list
for (country in names(f_statistics)) {
  f_stats[country] <- f_statistics[[country]]$f$stat  # Extract F-statistic
  f_p_vals[country] <- f_statistics[[country]]$f$p    # Extract p-value
}
f_stats
f_p_vals


# store results of regression: 
sum_table <- etable(reg_summaries,   headers = "First stage IV import share tariffs")
sum_table <- as.data.frame(sum_table)

sum_table <- rbind(sum_table, f_stats, f_p_vals)

# export results
write.csv(sum_table, "/data/sikeme/TRADE/WTO/result/quant_based/IV_tariff_first_reg/summary_reg_import_tar.csv", row.names = FALSE)


f_statistics[[country]] <- fitstat(model, "f") 





#Merge all predict values with initial values 
predicted_values_df <- bind_rows(predicted_values_list)
names(predicted_values_df)
predicted_values_df2 <- predicted_values_df %>%
  select(Year, ProductCode, PartnerISO3, Partner, import_tariff, predicted_import_tariff)

write.csv(predicted_values_df2, "/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/predicted_import_tariff.csv", row.names = FALSE)

# 
# # if want to join it back...
# final_dta<- left_join(share_tariff1, predicted_values_df %>% select(ReporterISO3, PartnerISO3, ProductCode, Year, predicted_import_tariff),
#                       by = c("ReporterISO3", "PartnerISO3", "ProductCode", "Year"))
# 


###############################################################################

# for export 

IVs <- c("export_mean_tariff_IV")

# Loop through each tariff variable and run the regression
models <- list()
reg_summaries <- list()
predicted_values_list <- list()  # List 
f_statistics <- list()



for (country in countries) {
  # Filter the data for the current country
  dta <- share_tariff1 %>% filter(ReporterISO3 == country)
  dta <- dta %>% filter(ReporterISO3 != PartnerISO3)
  
  # merge with gravity vars data 
  dta <- left_join(dta, X_vars2, by = c("ReporterISO3", "PartnerISO3", "ProductCode", "Year"))
  
  # Create formula for the regression
  formula <- as.formula(paste("export_tariff ~", paste(c(controls, IVs), collapse = " + ")))
  
  # Run the regression
  model <- feols(formula, data = dta)
  
  # Store the model in the list
  models[[country]] <- model
  reg_summaries[[country]] <- summary(model)
  
  # get f stat 
  f_statistics[[country]] <- fitstat(model, "f") 
  
  # get predicted values'  
  dta$predicted_export_tariff <- predict(model, newdata = dta)
  
  # Store the predicted values in the list
  predicted_values_list[[country]] <- dta
  
  print(summary(models[[country]])) # print summary for each regressions
}


# store f stat results:f_stats <- c()
f_stats <- c("F stat")
f_p_vals <- c("F pval")

# Loop through each country in the f_statistics list
for (country in names(f_statistics)) {
  f_stats[country] <- f_statistics[[country]]$f$stat  # Extract F-statistic
  f_p_vals[country] <- f_statistics[[country]]$f$p    # Extract p-value
}
f_stats
f_p_vals



sum_table <- etable(reg_summaries,   headers = "First stage IV export share tariffs")
sum_table <- as.data.frame(sum_table)
sum_table <- rbind(sum_table, f_stats, f_p_vals)



write.csv(sum_table, "/data/sikeme/TRADE/WTO/result/quant_based/IV_tariff_first_reg/summary_reg_export_tar.csv", row.names = FALSE)




#Merge all predict values with initial values 
predicted_values_df <- bind_rows(predicted_values_list)
names(predicted_values_df)
predicted_values_df2 <- predicted_values_df %>%
  select(Year, ProductCode, PartnerISO3, Partner, import_tariff, predicted_export_tariff)

write.csv(predicted_values_df2, "/data/sikeme/TRADE/WTO/data/quant_based/tariffs_IVs/predicted_export_tariff.csv", row.names = FALSE)



###################################################################################




















