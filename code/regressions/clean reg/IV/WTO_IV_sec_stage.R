
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
library(ggplot2)


################################################################################
# 1) LOAD data 

# load NTM data from IV reg
BRA_WTO_TBT_count <- read_csv("BRA/IVs/IV_TBT_WTO.csv")
BRA_WTO_TBT_count <- BRA_WTO_TBT_count[, -1]
BRA_WTO_SPS_count <- read_csv("BRA/IVs/IV_SPS_WTO.csv")
BRA_WTO_SPS_count <- BRA_WTO_SPS_count[, -1]
names(BRA_WTO_SPS_count)



# load trade data  
BRA_trade <- read_csv("BRA/BRA_trade_Merge.csv") #  tariffs are MFN
# for applied tariffs
BRA_trade <- read_csv("/data/sikeme/TRADE/WTO/data/BRA/BRA_trade_Merge2.csv")
names(BRA_trade)



BRA_trade <- select(BRA_trade,
                    "Year", "Nomenclature","ReporterISO3",
                    "ProductCode","ReporterName",
                    "PartnerISO3", "PartnerName","price",
                    "price","rta","contig","dist",
                    "comlang_off",
                    "landlocked_rep","landlocked_par",
                    "Reporter_GDP","Partner_GDP", "tariff_BRA")


################################################################################

# 2)  now merge the two:

Merge <- left_join(BRA_trade, BRA_WTO_SPS_count)
names(Merge)
Merge2 <- left_join(BRA_trade, BRA_WTO_TBT_count)
Merge <- left_join(Merge, BRA_WTO_TBT_count)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_count_SPS <- ifelse(is.na(Merge$ntm_count_SPS), 0 , Merge$ntm_count_SPS) 
Merge$ntm_count_TBT <- ifelse(is.na(Merge$ntm_count_TBT), 0 , Merge$ntm_count_TBT) 


names(BRA_WTO_TBT_count)






write.csv(Merge, "/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count_IV.csv")
Merge_WTO_IV <- Merge





###############################################################################


# B) Run the regression 



###############################################################################
# if need to load the data 

rm(list=ls())
Merge_WTO_IV <- read_csv("/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count_IV.csv")
names(Merge_WTO_IV)


# 2) for each HS codes
# Run multiple regression for each HS codes using Poisson 

# create HS2 level var
Merge_WTO_IV$H2<- substring(Merge_WTO_IV$ProductCode   , first = 1, last = 2)
Merge_WTO_IV$H1<- substring(Merge_WTO_IV$ProductCode   , first = 1, last = 1)
unique(Merge_WTO_IV$H1)
Merge_WTO_IV$Year <- as.factor(Merge_WTO_IV$Year)


# Run for each H2

#re run the regression for each H2
unique_H1 <- unique(Merge_WTO_IV$H1)
# List to store regression results
regression_summaries <- list()

# Loop over each unique value of H1
for (h1_val in unique_H1) {
  # Subset the data for the current H1 value
  sub_Merge_WTO_IV <- subset(Merge_WTO_IV, H1 == h1_val)
  
  # Run the regression
  reg <- feols(log(price) ~ log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                 ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                 PartnerISO3,
               data = sub_Merge_WTO_IV)
  
  # Store the summary of regression
  regression_summaries[[h1_val]] <- summary(reg)
}
# Print the summaries
for (i in seq_along(unique_H1)) {
  cat("Summary for H1 =", unique_H1[i], ":\n")
  print(regression_summaries[[i]])
}



summary_table <- etable(regression_summaries[[1]], regression_summaries[[2]], regression_summaries[[3]],
                        regression_summaries[[4]], regression_summaries[[5]], regression_summaries[[6]],
                        regression_summaries[[7]], regression_summaries[[8]], regression_summaries[[9]],
                        regression_summaries[[10]],
                        headers = "WTO")
names(summary_table) <- c("vars","H0","H1", "H2", "H3","H4", "H5", "H6","H7", "H8", "H9")
summary_table_2  <- rbind(summary_table[1:15, ], summary_table[5192:5194, ])
names(summary_table)
write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/IV/regression_results_H1_WTO.csv", sep = ",", row.names = FALSE)




######################################################################################


library(car)


df_list_AVE_SPS <- list()
for (i in 1:9) {
  # Calculate AVE_SPS and AVE_TBT for each element
  AVE_SPS <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_SPS) - 1)) * 100")
  df_AVE_SPS <- data.frame(AVE_SPS)
  rownames(df_AVE_SPS)<- paste0("HS", i-1)
  df_list_AVE_SPS[[i]] <- df_AVE_SPS
}
combined_df_AVE_WTO_SPS <- do.call(rbind, df_list_AVE_SPS)
write.table(combined_df_AVE_WTO_SPS, file = "/data/sikeme/TRADE/WTO/result/IV/AVE_SPS_H1_WTO.csv", sep = ",", row.names = TRUE)


df_list_AVE_TBT <- list()
for (i in 1:10) {
  # Calculate AVE_TBT  for each element
  AVE_TBT <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_TBT) - 1)) * 100")
  df_AVE_TBT <- data.frame(AVE_TBT)
  rownames(df_AVE_TBT)<- paste0("HS", i-1)
  df_list_AVE_TBT[[i]] <- df_AVE_TBT
}
combined_df_AVE_WTO_TBT <- do.call(rbind, df_list_AVE_TBT)

write.table(combined_df_AVE_WTO_TBT, file = "/data/sikeme/TRADE/WTO/result/IV/AVE_TBT_H1_WTO.csv", sep = ",", row.names = TRUE)


###############################################################################

# plot WTO: 
library(tibble)
combined_df_AVE_WTO_SPS <-as.data.frame(combined_df_AVE_WTO_SPS)
combined_df_AVE_WTO_SPS <- rownames_to_column(combined_df_AVE_WTO_SPS, var = "HS_codes")

combined_df_AVE_WTO_TBT <-as.data.frame(combined_df_AVE_WTO_TBT)
combined_df_AVE_WTO_TBT <- rownames_to_column(combined_df_AVE_WTO_TBT, var = "HS_codes")



ggplot(combined_df_AVE_WTO_SPS, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5..)) +
  geom_pointrange() +
  labs(title = "Coefficients with Mean Estimates and Confidence Intervals",
       x = "Coefficient",
       y = "Mean Estimate") +
  theme_minimal()









