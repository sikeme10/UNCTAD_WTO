












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

# a) Load data

#load data:
dta_UNCTAD_SPS <- read_csv("BRA/IVs/IV_SPS_UNCTAD.csv")
dta_UNCTAD_TBT <- read_csv("BRA/IVs/IV_TBT_UNCTAD.csv")

Merge_IV_UNCTAD <- full_join(dta_UNCTAD_SPS,dta_UNCTAD_TBT)
Merge_IV_UNCTAD <- select(Merge_IV_UNCTAD, Year, ReporterISO3, H5,MFN_tariff_H5,
                          price_H5, ntm_count_SPS_weight, ntm_count_TBT_weight)
names(Merge_IV_UNCTAD)



#get number of NTMs imposed (summary)
summary_SPS <- summary(Merge_IV_UNCTAD$ntm_count_SPS_weight)
summary_TBT <- summary(Merge_IV_UNCTAD$ntm_count_TBT_weight)
# negative values...
Merge_IV_UNCTAD <- Merge_IV_UNCTAD %>%
  mutate(ntm_count_SPS_weight = ifelse(is.na(ntm_count_SPS_weight) | ntm_count_SPS_weight < 0, 0, ntm_count_SPS_weight))
Merge_IV_UNCTAD <- Merge_IV_UNCTAD %>%
  mutate(ntm_count_TBT_weight = ifelse(is.na(ntm_count_TBT_weight) | ntm_count_TBT_weight < 0, 0, ntm_count_TBT_weight))
Merge_IV_UNCTAD$MFN_tariff_H5[is.na(Merge_IV_UNCTAD$MFN_tariff_H5)] <- 0

names(Merge_IV_UNCTAD)

################################################################################

# Run the regressions

################################################################################

# create HS2 level var
Merge_IV_UNCTAD$H2<- substring(Merge_IV_UNCTAD$H5   , first = 1, last = 1)
Merge_IV_UNCTAD$H1<- substring(Merge_IV_UNCTAD$H5   , first = 1, last = 1)
unique(Merge_IV_UNCTAD$H2)
Merge_IV_UNCTAD$Year <- as.factor(Merge_IV_UNCTAD$Year)


# Run for each H2

#re run the regression for each H2
unique_H2 <- unique(Merge_IV_UNCTAD$H1)
# List to store regression results
regression_summaries <- list()

# Loop over each unique value of H2
for (h2_val in unique_H2) {
  # Subset the data for the current H2 value
  sub_Merge_IV_UNCTAD <- subset(Merge_IV_UNCTAD, H2 == h2_val)
  
  # Run the regression
  reg <- feols(log(price_H5) ~ ntm_count_SPS_weight + ntm_count_TBT_weight + 
                 log(1 + MFN_tariff_H5) + Year + H5 ,
               data = sub_Merge_IV_UNCTAD)
  
  # Store the summary of regression
  regression_summaries[[h2_val]] <- summary(reg)
}
# Print the summaries
for (i in seq_along(unique_H2)) {
  cat("Summary for H2 =", unique_H2[i], ":\n")
  print(regression_summaries[[i]])
}



summary_table <- etable(regression_summaries[[1]], regression_summaries[[2]], regression_summaries[[3]],
                        regression_summaries[[4]], regression_summaries[[5]], regression_summaries[[6]],
                        regression_summaries[[7]], regression_summaries[[8]], regression_summaries[[9]],
                        regression_summaries[[10]],
                        headers = "UNCTAD")
names(summary_table) <- c("vars","H0","H1", "H2", "H3","H4", "H5", "H6","H7", "H8", "H9")
summary_table_2  <- rbind(summary_table[1:15, ], summary_table[5192:5194, ])
names(summary_table)
write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/regression_results_H1_UNCTAD.csv", sep = ",", row.names = FALSE)

















