
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

#   A)     Clean the data

################################################################################


# load NTM data from IV reg
BRA_WTO_TBT_count <- read_csv("BRA/IVs/IV_TBT_WTO.csv")
BRA_WTO_TBT_count <- BRA_WTO_TBT_count[, -1]
BRA_WTO_SPS_count <- read_csv("BRA/IVs/IV_SPS_WTO.csv")
BRA_WTO_SPS_count <- BRA_WTO_SPS_count[, -1]
names(BRA_WTO_SPS_count)



# load trade data  
BRA_trade <- read_csv("BRA/BRA_trade_Merge2.csv")
names(BRA_trade)

################################################################################
# Select varible of interest in trade data 

BRA_trade <- select(BRA_trade,
                    "Year", "Nomenclature","ReporterISO3",
                    "ProductCode","ReporterName",
                    "PartnerISO3", "PartnerName","price",
                    "price","rta","contig","dist",
                    "comlang_off","country_o",
                    "landlocked_o","landlocked_d",
                    "ln_Gdp_per_cap","ln_GDP_Brazil", "MFN_tariff", "tariff_BRA")



################################################################################
# EU countries

# if negative ntm counts due to IV
# Because of IV we get negative count of SPS and TBT
BRA_WTO_SPS_count$ntm_count_SPS <- ifelse(BRA_WTO_SPS_count$ntm_count_SPS<0, 0 , BRA_WTO_SPS_count$ntm_count_SPS) 
BRA_WTO_TBT_count$ntm_count_TBT <- ifelse(BRA_WTO_TBT_count$ntm_count_TBT<0, 0 , BRA_WTO_TBT_count$ntm_count_TBT) 
summary(BRA_WTO_SPS_count$ntm_count_SPS)

# only applies to SPS as TBT are applied at all countries so less of an issue
# convert EU in NTM data to countries to match the two data sets
names(BRA_WTO_SPS_count)

# get the partners
table(BRA_WTO_SPS_count$PartnerISO3)
table(BRA_WTO_TBT_count$PartnerISO3)


replicate_eun_rows <- function(df, country_col = "PartnerISO3") {
  # Define the list of EU countries (including the UK, if pre 2020)
  eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", 
                    "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", 
                    "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
  # Separate rows where the country is 'EUN'
  eun_data <- df[df[[country_col]] == "EUN", ]
  
  # Replicate 'EUN' data for each EU country including the UK
  replicated_data <- do.call(rbind, lapply(eu_countries, function(c) {
    new_data <- eun_data
    new_data[[country_col]] <- c
    return(new_data)
  }))
  
  # Remove the original 'EUN' rows from the data frame
  df_no_eun <- df[df[[country_col]] != "EUN", ]
  
  # Combine the replicated data with the original data frame
  updated_df <- rbind(df_no_eun, replicated_data)
  updated_df <- updated_df %>% 
  
  
  return(updated_df)
}
BRA_WTO_SPS_count <-replicate_eun_rows(BRA_WTO_SPS_count)


BRA_WTO_SPS_count <- BRA_WTO_SPS_count %>% group_by(Year,ProductCode, PartnerISO3) %>%
  summarise(ntm_count_SPS= sum(ntm_count_SPS))
summary(BRA_WTO_SPS_count$ntm_count_SPS)


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


# Because of IV we get negative count of SPS and TBT
Merge$ntm_count_SPS <- ifelse(Merge$ntm_count_SPS<0, 0 , Merge$ntm_count_SPS) 
Merge$ntm_count_TBT <- ifelse(Merge$ntm_count_TBT<0, 0 , Merge$ntm_count_TBT) 
summary(Merge$ntm_count_TBT)


write.csv(Merge, "/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count_IV.csv")
Merge_WTO_IV <- Merge







###############################################################################

# B) Run the regression 

###############################################################################
rm(list = ls())
Merge <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count_IV.csv")
names(Merge)
table(Merge$PartnerISO3)

summary(Merge$ntm_count_SPS)
summary(Merge$ntm_count_TBT)
summary_SPS <- summary(Merge$ntm_count_SPS)
summary_TBT <- summary(Merge$ntm_count_TBT)
count_NTMs_WTO <- rbind(summary_SPS,summary_TBT)

count_NTMs_WTO
write.table(count_NTMs_WTO, file = "/data/sikeme/TRADE/WTO/result/summary/count_appliedNTMs_WTO.csv", sep = ",", row.names = TRUE)


plot <- ggplot(Merge, aes(x = factor(ntm_count_SPS))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of imposed NTMs in th WTO notification database",
       x = "NTM count  (SPS)",
       y = "Number of observations") +
  # facet_wrap(~ Year)+
  scale_x_discrete(breaks = seq(min(Merge$ntm_count_SPS), max(Merge$ntm_count_SPS), by = 10))+
  theme_minimal()

ggsave("/data/sikeme/TRADE/WTO/result/summary/WTO_count_distribution.png", plot = plot)
###############################################################################



library(fixest)
Merge$Year <- as.factor(Merge$Year)
names(Merge)
# Run the regression model with fixest
reg1 <- feols(log(price) ~
                log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                ntm_count_SPS + ntm_count_TBT   + log(1+ tariff_BRA) +  Year+ ProductCode + 
                PartnerISO3 , data = Merge)
summary(reg1)

transformation <- function(beta) ((exp(beta) - 1)) * 100
transformation(beta)






################################################################################

# Run multiple regression for each HS codes 

Merge$Year <- as.factor(Merge$Year)

# create HS2 level var
Merge$H1<- substring(Merge$ProductCode   , first = 1, last = 1)
Merge$H2<- substring(Merge$ProductCode   , first = 1, last = 2)
Merge$H3<- substring(Merge$ProductCode   , first = 1, last = 3)
Merge$H4<- substring(Merge$ProductCode   , first = 1, last = 4)
Merge$Year <- as.factor(Merge$Year)


# Run for each H1

#re run the regression for each H1
unique_H2 <- unique(Merge$H2)
unique_H2 <- sort(unique_H2, decreasing = FALSE)
# List to store regression results
regression_summaries <- list()

# Loop over each unique value of H2
for (H2_val in unique_H2) {
  # Subset the data for the current H2 value
  sub_merge <- subset(Merge, H2 == H2_val)
  
  # Run the regression
  reg <- feols(log(price) ~
                 log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                 ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                 PartnerISO3,
               data = sub_merge)
  
  # Store the summary of regression
  regression_summaries[[H2_val]] <- summary(reg)
}
# Print the summaries
for (i in unique_H2) {
  cat("Summary for H2 =", unique_H2[i], ":\n")
  print(regression_summaries[[i]])
}


fixest_list <- list(regression_summaries[[1]])

for (i in 1:length(regression_summaries)) {
  fixest_list <- c(fixest_list, list(regression_summaries[[i]]))
}

summary_table <- etable(fixest_list)

names(summary_table) <- c("vars", paste0("HS", 1:97))

summary_table_2  <- rbind(summary_table[1:14, ], summary_table[5042:5045, ])
names(summary_table)
write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/regression_results_H2_WTO_IV.csv", sep = ",", row.names = FALSE)






################################################################################


# C) Estimate ad-valorem equivalents


###############################################################################


# 1) without count 

library(car)

#get individual AVE 
AVE_SPS <- deltaMethod(regression_summaries[[1]], "((exp(ntm_count_SPS) - 1)) * 100")
AVE_TBT <- deltaMethod(regression_summaries[[1]], "((exp(ntm_count_TBT) - 1)) * 100")


# get 


# get AVE using delta mehtod (but sometimes the cowefficient did not converge so need to add lines)
df_list_AVE_SPS <- list()
for (i in 1:96) {
  # Check if the coefficient is present
  if ("ntm_count_SPS" %in% names(regression_summaries[[i]]$coefficients)) {
    # Calculate AVE_SPS if the coefficient is present
    AVE_SPS <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_SPS) - 1)) * 100")
    df_AVE_SPS <- data.frame(AVE_SPS)
  } else {
    # Set AVE_SPS to 0 if the coefficient is absent
    df_AVE_SPS <- data.frame(Estimate = 0, SE = NA, `2.5 %` = NA, `97.5 %` = NA)
  }
  rownames(df_AVE_SPS) <- paste0("HS", i-1)
  df_list_AVE_SPS[[i]] <- df_AVE_SPS
}
combined_df_AVE_WTO_SPS <- do.call(rbind, df_list_AVE_SPS)
names(combined_df_AVE_WTO_SPS)
library(tibble)

combined_df_AVE_WTO_SPS <- rownames_to_column(combined_df_AVE_WTO_SPS, var = "HS_level")
write.csv(combined_df_AVE_WTO_SPS, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_SPS_H2_WTO_IV.csv",  row.names = TRUE)



df_list_AVE_TBT <- list()
for (i in 1:96) {
  # Check if the coefficient is present
  if ("ntm_count_TBT" %in% names(regression_summaries[[i]]$coefficients)) {
    # Calculate AVE_TBT if the coefficient is present
    AVE_TBT <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_TBT) - 1)) * 100", level = 0.95)
    df_AVE_TBT <- data.frame(AVE_TBT)
  } else {
    # Set AVE_TBT to 0 if the coefficient is absent
    df_AVE_TBT <- data.frame(Estimate = 0, SE = NA, `2.5 %` = NA, `97.5 %` = NA)
  }
  rownames(df_AVE_TBT) <- paste0("HS", i-1)
  df_list_AVE_TBT[[i]] <- df_AVE_TBT
}
combined_df_AVE_WTO_TBT <- do.call(rbind, df_list_AVE_TBT)
combined_df_AVE_WTO_TBT <- rownames_to_column(combined_df_AVE_WTO_TBT, var = "HS_level")
write.csv(combined_df_AVE_WTO_TBT, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_TBT_H2_WTO_IV.csv",  row.names = TRUE)




###############################################################################
# map it 



combined_df_AVE_WTO_SPS <-as.data.frame(combined_df_AVE_WTO_SPS)
combined_df_AVE_WTO_SPS <- rownames_to_column(combined_df_AVE_WTO_SPS, var = "HS_codes")

ggplot(combined_df_AVE_WTO_SPS, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5..)) +
  geom_pointrange() +
  labs(title = "Coefficients with Mean Estimates and Confidence Intervals",
       x = "Coefficient",
       y = "Mean Estimate") +
  theme_minimal()



######### for TBT 



combined_df_AVE_WTO_TBT <-as.data.frame(combined_df_AVE_WTO_TBT)
combined_df_AVE_WTO_TBT <- rownames_to_column(combined_df_AVE_WTO_TBT, var = "HS_codes")


ggplot(combined_df_AVE_WTO_TBT, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5..)) +
  geom_pointrange() +
  labs(title = "Coefficients with Mean Estimates and Confidence Intervals",
       x = "Coefficient",
       y = "Mean Estimate") +
  theme_minimal()




###############################################################################


# 1) with count 

Count_H1_year_WTO <- Merge %>%
  group_by(H1) %>%
  summarise(ntm_count_SPS_H1 = sum(ntm_count_SPS),
            ntm_count_TBT_H1 = sum(ntm_count_TBT))

Count_H1_year_WTO$source <- "WTO"


#b) get AVE for SPS 

combined_df_AVE_SPS <- list()
combined_df_AVE_SPS_list <- list()

# Loop through each year
for (year in unique(Merge$Year)) {
  # Subset the data for the current year
  Count_H1_year <- Merge %>%
    filter(Year == year) %>%
    group_by(H1) %>%
    summarise(ntm_count_SPS_H1 = sum(ntm_count_SPS),
              ntm_count_TBT_H1 = sum(ntm_count_TBT))
  
  Count_H1_year_SPS <- Count_H1_year$ntm_count_SPS_H1
  df_list_AVE_SPS <- list()
  # Loop through elements for the current year
  for (i in seq_along(Count_H1_year_SPS)) {
    val <- Count_H1_year_SPS[i]
    # Calculate AVE_SPS and AVE_TBT for each element
    AVE_SPS <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_SPS) * val - 1)) * 100")
    df_AVE_SPS <- data.frame(AVE_SPS)
    rownames(df_AVE_SPS) <- paste0("HS", i-1)
    df_list_AVE_SPS[[i]] <- df_AVE_SPS
  }
  combined_df_AVE_SPS <- do.call(rbind, df_list_AVE_SPS)
  
  # Add the result to the list for the current year
  combined_df_AVE_SPS_list[[year]] <- combined_df_AVE_SPS
}
# name it differently 
WTO_AVE_2012 <- combined_df_AVE_SPS_list[[1]]
WTO_AVE_2013 <- combined_df_AVE_SPS_list[[2]]
WTO_AVE_2014 <- combined_df_AVE_SPS_list[[3]]
WTO_AVE_2015 <- combined_df_AVE_SPS_list[[4]]
WTO_AVE_2016 <- combined_df_AVE_SPS_list[[5]]
WTO_AVE_2012 <- rownames_to_column(WTO_AVE_2012, var = "HS_codes")
WTO_AVE_2013 <- rownames_to_column(WTO_AVE_2013, var = "HS_codes")
WTO_AVE_2014 <- rownames_to_column(WTO_AVE_2014, var = "HS_codes")
WTO_AVE_2015 <- rownames_to_column(WTO_AVE_2015, var = "HS_codes")
WTO_AVE_2016 <- rownames_to_column(WTO_AVE_2016, var = "HS_codes")





merged_WTO_AVE_list <- list()
Y <- c(2012,2013,2014,2015,2016)
# Loop through years 2012 to 2016
for (year in 1:5) {
  # Get the AVE data frame for the current year
  WTO_AVE_year <- combined_df_AVE_SPS_list[[year]]
  # Add the year as a new column
  WTO_AVE_year$Year <- Y[year]
  WTO_AVE_year <- rownames_to_column(WTO_AVE_year, var = "HS_codes")
  merged_WTO_AVE_list[[year]] <- WTO_AVE_year
}

# Combine all data frames into a single data frame
merged_WTO_AVE <- do.call(rbind, merged_WTO_AVE_list)

###############################################################################
# For TBT


combined_df_AVE_TBT <- list()
combined_df_AVE_TBT_list <- list()

# Loop through each year
for (year in unique(Merge$Year)) {
  # Subset the data for the current year
  Count_H1_year <- Merge %>%
    filter(Year == year) %>%
    group_by(H1) %>%
    summarise(ntm_count_TBT_H1 = sum(ntm_count_TBT),
              ntm_count_TBT_H1 = sum(ntm_count_TBT))
  
  Count_H1_year_TBT <- Count_H1_year$ntm_count_TBT_H1
  df_list_AVE_TBT <- list()
  # Loop through elements for the current year
  for (i in seq_along(Count_H1_year_TBT)) {
    val <- Count_H1_year_TBT[i]
    # Calculate AVE_TBT and AVE_TBT for each element
    AVE_TBT <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_TBT) * val - 1)) * 100")
    df_AVE_TBT <- data.frame(AVE_TBT)
    rownames(df_AVE_TBT) <- paste0("HS", i-1)
    df_list_AVE_TBT[[i]] <- df_AVE_TBT
  }
  combined_df_AVE_TBT <- do.call(rbind, df_list_AVE_TBT)
  
  # Add the result to the list for the current year
  combined_df_AVE_TBT_list[[year]] <- combined_df_AVE_TBT
}
# name it differently 
WTO_AVE_2012 <- combined_df_AVE_TBT_list[[1]]
WTO_AVE_2013 <- combined_df_AVE_TBT_list[[2]]
WTO_AVE_2014 <- combined_df_AVE_TBT_list[[3]]
WTO_AVE_2015 <- combined_df_AVE_TBT_list[[4]]
WTO_AVE_2016 <- combined_df_AVE_TBT_list[[5]]
WTO_AVE_2012 <- rownames_to_column(WTO_AVE_2012, var = "HS_codes")
WTO_AVE_2013 <- rownames_to_column(WTO_AVE_2013, var = "HS_codes")
WTO_AVE_2014 <- rownames_to_column(WTO_AVE_2014, var = "HS_codes")
WTO_AVE_2015 <- rownames_to_column(WTO_AVE_2015, var = "HS_codes")
WTO_AVE_2016 <- rownames_to_column(WTO_AVE_2016, var = "HS_codes")


merged_WTO_AVE_list <- list()
Y <- c(2012,2013,2014,2015,2016)
# Loop through years 2012 to 2016
for (year in 1:5) {
  # Get the AVE data frame for the current year
  WTO_AVE_year <- combined_df_AVE_TBT_list[[year]]
  # Add the year as a new column
  WTO_AVE_year$Year <- Y[year]
  WTO_AVE_year <- rownames_to_column(WTO_AVE_year, var = "HS_codes")
  merged_WTO_AVE_list[[year]] <- WTO_AVE_year
}

# Combine all data frames into a single data frame
merged_WTO_AVE <- do.call(rbind, merged_WTO_AVE_list)






###############################################################################

df_list_AVE_TBT <- list()
for (i in 1:9) {
  # Calculate AVE_TBT  for each element
  AVE_TBT <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_TBT) - 1)) * 100")
  df_AVE_TBT <- data.frame(AVE_TBT)
  rownames(df_AVE_TBT)<- paste0("HS", i)
  df_list_AVE_TBT[[i]] <- df_AVE_TBT
}
combined_df_AVE_TBT <- do.call(rbind, df_list_AVE_TBT)

write.table(combined_df_AVE_TBT, file = "/data/sikeme/TRADE/WTO/result/AVE_TBT_H1_WTO.csv", sep = ",", row.names = TRUE)





