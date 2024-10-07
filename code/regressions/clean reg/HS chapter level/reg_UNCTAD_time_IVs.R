
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

# A) clean the data for regression 

################################################################################

################################################################################

# load NTM data from IV reg
BRA_UNCTAD_TBT_count <- read_csv("BRA/IVs/IV_TBT_UNCTAD.csv")
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT_count[, -1]
BRA_UNCTAD_SPS_count <- read_csv("BRA/IVs/IV_SPS_UNCTAD.csv")
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS_count[, -1]



################################################################################
# load trade data  
BRA_trade <- read_csv("BRA/BRA_trade_Merge2.csv")
names(BRA_trade)


#1) get trade merged data 
BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge2.csv")
names(BRA_trade_Merge)

BRA_trade <- select(BRA_trade,
                    "Year", "Nomenclature","ReporterISO3",
                    "ProductCode","ReporterName",
                    "PartnerISO3", "PartnerName","price",
                    "price","rta","contig","dist",
                    "comlang_off",
                    "landlocked_rep","landlocked_par",
                    "Reporter_GDP","Partner_GDP", "tariff_BRA")

table(BRA_trade$Year)
# select a year 
# BRA_trade <- subset(BRA_trade_Merge, Year == 2015)
BRA_trade <-BRA_trade_Merge

################################################################################

# EU as parner to match to trade data 

# Because of IV we get negative count of SPS and TBT
BRA_UNCTAD_SPS_count$ntm_count_SPS <- ifelse(BRA_UNCTAD_SPS_count$ntm_count_SPS<0, 0 , BRA_UNCTAD_SPS_count$ntm_count_SPS) 
BRA_UNCTAD_TBT_count$ntm_count_TBT <- ifelse(BRA_UNCTAD_TBT_count$ntm_count_TBT<0, 0 , BRA_UNCTAD_TBT_count$ntm_count_TBT) 
summary(BRA_UNCTAD_SPS_count$ntm_count_SPS)
summary(BRA_UNCTAD_TBT_count$ntm_count_TBT)


# same for EUN
table(BRA_UNCTAD_SPS_count $PartnerISO3)
table(BRA_UNCTAD_TBT_count $PartnerISO3)
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
BRA_UNCTAD_TBT_count <- replicate_eun_rows(BRA_UNCTAD_TBT_count)
BRA_UNCTAD_SPS_count <- replicate_eun_rows(BRA_UNCTAD_SPS_count)



# 
#calculate total counts of NTM by partners and HS code 
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS_count %>%
  group_by(Year, PartnerISO3, HSCode) %>%
  summarise(ntm_count_SPS = sum(ntm_count_SPS, na.rm = TRUE))
summary(BRA_UNCTAD_SPS_count$ntm_count_SPS)
# check if duplicates
duplicates <- BRA_UNCTAD_SPS_count[duplicated(BRA_UNCTAD_SPS_count), ]

#for TBT 
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT_count %>%
  group_by(Year, PartnerISO3, HSCode) %>%
  summarise(ntm_count_TBT= sum(ntm_count_TBT, na.rm = TRUE))
summary(BRA_UNCTAD_TBT_count$ntm_count_TBT)


################################################################################

# rename variable
BRA_trade <- BRA_trade %>% rename(HSCode =ProductCode)
################################################################################

#3)  now merge the two:

Merge <- left_join(BRA_trade, BRA_UNCTAD_SPS_count)
Merge2 <- left_join(BRA_trade, BRA_UNCTAD_TBT_count)
Merge <- left_join(Merge, BRA_UNCTAD_TBT_count)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_count_SPS <- ifelse(is.na(Merge$ntm_count_SPS), 0 , Merge$ntm_count_SPS) 
Merge$ntm_count_TBT <- ifelse(is.na(Merge$ntm_count_TBT), 0 , Merge$ntm_count_TBT) 




################################################################################





###############################################################################

Merge$HSCode <- as.factor(Merge$HSCode)

write.csv(Merge, "/data/sikeme/TRADE/WTO/data/BRA/Merge_UNACTAD_count_IV.csv")
Merge_UNCTAD <- Merge




###############################################################################

# B) Run the regression 

###############################################################################
# if need to load the data 



rm(list = ls())
Merge_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_UNACTAD_count_IV.csv")
names(Merge_UNCTAD)
table(Merge_UNCTAD$PartnerISO3)

#get number of NTMs imposed (summary)
summary_SPS <- summary(Merge_UNCTAD$ntm_count_SPS)
summary_TBT <- summary(Merge_UNCTAD$ntm_count_TBT)
count_NTMs_UNCTAD <- rbind(summary_SPS,summary_TBT)
write.table(count_NTMs_UNCTAD, file = "/data/sikeme/TRADE/WTO/result/summary/count_appliedNTMs_UNACTD.csv", sep = ",", row.names = TRUE)


# plot distribution of number of NTMs per HS codes 
ggplot(Merge_UNCTAD, aes(x = factor(ntm_count_SPS))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Number of imposed NTMs",
       x = "ntm_count_SPS",
       y = "Number of observations") +
  #facet_wrap(~ Year)+
  scale_x_discrete(breaks = seq(min(Merge$ntm_count_SPS), max(Merge$ntm_count_SPS), by = 10))+
  theme_minimal()



###############################################################################

# 1) General regression

library(fixest)
names(Merge_UNCTAD)
# Run the regression model with fixest
Merge_UNCTAD$Year <- as.factor(Merge_UNCTAD$Year)
reg1 <- feols(log(price) ~
                log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                ntm_count_SPS + ntm_count_TBT   + log(1+ MFN_tariff) +  Year+ HSCode + 
                PartnerISO3 , data = Merge_UNCTAD)
summary(reg1)

transformation <- function(beta) ((exp(beta) - 1)) * 100
transformation(beta)






################################################################################

# 2) for each HS codes

# Run multiple regression for each HS codes using Poisson 
names(Merge_UNCTAD)

Merge_UNCTAD <- Merge_UNCTAD %>% rename(ProductCode = HSCode)


# create HS2 level var
Merge_UNCTAD$H2<- substring(Merge_UNCTAD$ProductCode, first = 1, last = 2)
Merge_UNCTAD$H1<- substring(Merge_UNCTAD$ProductCode, first = 1, last = 1)
unique(Merge_UNCTAD$H2)
Merge_UNCTAD$Year <- as.factor(Merge_UNCTAD$Year)

# create an HS chapter variables for each HS chapters
table(Merge_UNCTAD$H2)
Merge_UNCTAD$H2_num <- as.numeric(Merge_UNCTAD$H2)
table(Merge_UNCTAD$H2_num)

Merge_UNCTAD <- Merge_UNCTAD %>% mutate(
  HS_section = case_when(
    H2_num %in% 1:5 ~ 1,
    H2_num %in% 6:14 ~ 2,
    H2_num %in% 15 ~ 3,
    H2_num %in% 16:24 ~ 4,
    H2_num %in% 25:27 ~ 5,
    H2_num %in% 28:38 ~ 6,
    H2_num %in% 39:40 ~ 7,
    H2_num %in% 41:43 ~ 8,
    H2_num %in% 44:46 ~ 9,
    H2_num %in% 47:49 ~ 10,
    H2_num %in% 50:63 ~ 11,
    H2_num %in% 64:67 ~ 12,
    H2_num %in% 68:70 ~ 13,
    H2_num %in% 71 ~ 14,
    H2_num %in% 72:83 ~ 15,
    H2_num %in% 84:85 ~ 16,
    H2_num %in% 86:89 ~ 17,
    H2_num %in% 90:92 ~ 18,
    H2_num %in% 93 ~ 19,
    H2_num %in% 94:96 ~ 20,
    H2_num %in% 97 ~ 21
  ))
table(Merge_UNCTAD$HS_section)



#################################################################################


# Run for each HS section level

#re run the regression for each HS section
Merge_UNCTAD <- Merge_UNCTAD %>% arrange(HS_section ,Year, PartnerISO3) 
unique_HS <- unique(Merge_UNCTAD$HS_section)
# List to store regression results
regression_summaries <- list()



# Loop over each unique value of HS Section level
for (HS_val in unique_HS) {
  # Subset the data for the current H2 value
  sub_merge <- subset(Merge_UNCTAD, HS_section == HS_val)
  
  # Run the regression
  reg <- feols(log(price) ~
                 log(dist) + contig + comlang_off + rta + landlocked_rep + landlocked_par +
                 ntm_count_SPS + ntm_count_TBT + log(1 + tariff_BRA) + log(1 + tariff_BRA) + Reporter_GDP + 
                 Partner_GDP + Year + ProductCode + 
                 PartnerISO3,
               data = sub_merge)
  
  # Store the summary of regression
  regression_summaries[[HS_val]] <- summary(reg)
}
# Print the summaries
for (i in seq_along(unique_HS)) {
  cat("Summary for HS =", unique_HS[i], ":\n")
  print(regression_summaries[[i]])
}




fixest_list <- list(regression_summaries[[1]])

for (i in 2:length(regression_summaries)) {
  fixest_list <- c(fixest_list, list(regression_summaries[[i]]))
}

summary_table <- etable(fixest_list)

names(summary_table) <- c("vars", paste0("Section", 1:21))

summary_table_2  <- rbind(summary_table[1:16, ], summary_table[5004:5007, ])




write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/regression_HS_section/regression_results_HS_UNCTAD_IV.csv", sep = ",", row.names = FALSE)







################################################################################


# C) Estimate ad-valorem equivalents


###############################################################################

# Estimate AVE 


# 1) without count 

library(car)

#get individual AVE 
AVE_SPS <- deltaMethod(regression_summaries[[1]], "((exp(ntm_count_SPS) - 1)) * 100")
AVE_TBT <- deltaMethod(regression_summaries[[1]], "((exp(ntm_count_TBT) - 1)) * 100")


# get 


# get AVE using delta mehtod (but sometimes the cowefficient did not converge so need to add lines)
df_list_AVE_SPS <- list()
for (i in 1:21) {
  # Check if the coefficient is present
  if ("ntm_count_SPS" %in% names(regression_summaries[[i]]$coefficients)) {
    # Calculate AVE_SPS if the coefficient is present
    AVE_SPS <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_SPS) - 1)) * 100")
    df_AVE_SPS <- data.frame(AVE_SPS)
  } else {
    # Set AVE_SPS to 0 if the coefficient is absent
    df_AVE_SPS <- data.frame(Estimate = 0, SE = NA, `2.5 %` = NA, `97.5 %` = NA)
  }
  rownames(df_AVE_SPS) <- paste0("HS", i)
  df_list_AVE_SPS[[i]] <- df_AVE_SPS
}
combined_df_AVE_UNCTAD_SPS <- do.call(rbind, df_list_AVE_SPS)
combined_df_AVE_UNCTAD_SPS <- rownames_to_column(combined_df_AVE_UNCTAD_SPS, var = "HS_section")
write.csv(combined_df_AVE_UNCTAD_SPS, file = "/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_SPS_HS_UNCTAD_IV.csv",  row.names = TRUE)


df_list_AVE_TBT <- list()
for (i in 1:21) {
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
combined_df_AVE_UNCTAD_TBT <- do.call(rbind, df_list_AVE_TBT)
combined_df_AVE_UNCTAD_TBT <- rownames_to_column(combined_df_AVE_UNCTAD_TBT, var = "HS_section")
write.csv(combined_df_AVE_UNCTAD_TBT, file = "/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_TBT_HS_UNCTAD_IV.csv",  row.names = TRUE)


###############################################################################
# if plotting the results:


# plot UNCTAD: 
combined_df_AVE_UNCTAD_SPS <-as.data.frame(combined_df_AVE_UNCTAD_SPS)
combined_df_AVE_UNCTAD_SPS <- rownames_to_column(combined_df_AVE_UNCTAD_SPS, var = "HS_codes")

combined_df_AVE_UNCTAD_TBT <-as.data.frame(combined_df_AVE_UNCTAD_TBT)
combined_df_AVE_UNCTAD_TBT <- rownames_to_column(combined_df_AVE_UNCTAD_TBT, var = "HS_codes")



ggplot(combined_df_AVE_UNCTAD_SPS, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5..)) +
  geom_pointrange() +
  labs(title = "Coefficients with Mean Estimates and Confidence Intervals",
       x = "Coefficient",
       y = "Mean Estimate") +
  theme_minimal()


###############################################################################

# plot combined WTO and UNCTAD

###############################################################################
# if combine with WTO data:for SPS 
combined_df_AVE_UNCTAD_SPS$source <- "UNCTAD"
names(combined_df_AVE_UNCTAD_SPS)
combined_df_AVE_WTO_SPS$source <- "WTO"
names(combined_df_AVE_WTO_SPS)
# Combine the dataframes into a single dataframe
combined_AVE_SPS <- rbind(combined_df_AVE_UNCTAD_SPS, combined_df_AVE_WTO_SPS)

plot_SPS <- ggplot(combined_AVE_SPS, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
  geom_pointrange() +
  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
  labs(title = "Mean Ad Valorem Equivalents of SPS at HS1 level from 2012 to 2016",
       x = "HS1 level codes",
       y = "Mean Estimate of AVE") +
  scale_color_manual(name = "Databases",  # Change legend title
                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 
plot_SPS
ggsave("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_SPS_plot.png", plot_SPS, width = 10, height = 6, dpi = 300)

####################

# for TBT 

# if combine with WTO data:for SPS 
combined_df_AVE_UNCTAD_TBT$source <- "UNCTAD"
combined_df_AVE_WTO_TBT$source <- "WTO"
# Combine the dataframes into a single dataframe
combined_AVE_TBT <- rbind(combined_df_AVE_UNCTAD_TBT, combined_df_AVE_WTO_TBT)

plot_TBT <- ggplot(combined_AVE_TBT, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
  geom_pointrange() +
  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
  labs(title = "Mean Ad Valorem Equivalents of TBT at HS1 level from 2012 to 2016",
       x = "HS1 level codes",
       y = "Mean Estimate of AVE") +
  scale_color_manual(name = "Databases",  # Change legend title
                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 
plot_TBT
ggsave("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_TBT_plot.png", plot_TBT, width = 10, height = 6, dpi = 300)







###############################################################################


# using count of NTMs


###############################################################################
# SPS 

combined_df_AVE_SPS <- list()
combined_df_AVE_SPS_list <- list()

# Loop through each year
for (year in unique(Merge_UNCTAD$Year)) {
  # Subset the data for the current year
  Count_H1_year <- Merge_UNCTAD %>%
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
UNCTAD_AVE_2012 <- combined_df_AVE_SPS_list[[1]]
UNCTAD_AVE_2013 <- combined_df_AVE_SPS_list[[2]]
UNCTAD_AVE_2014 <- combined_df_AVE_SPS_list[[3]]
UNCTAD_AVE_2015 <- combined_df_AVE_SPS_list[[4]]
UNCTAD_AVE_2016 <- combined_df_AVE_SPS_list[[5]]
UNCTAD_AVE_2012 <- rownames_to_column(UNCTAD_AVE_2012, var = "HS_codes")
UNCTAD_AVE_2013 <- rownames_to_column(UNCTAD_AVE_2013, var = "HS_codes")
UNCTAD_AVE_2014 <- rownames_to_column(UNCTAD_AVE_2014, var = "HS_codes")
UNCTAD_AVE_2015 <- rownames_to_column(UNCTAD_AVE_2015, var = "HS_codes")
UNCTAD_AVE_2016 <- rownames_to_column(UNCTAD_AVE_2016, var = "HS_codes")

merged_UNCTAD_AVE_list <- list()
Y <- c(2012,2013,2014,2015,2016)
# Loop through years 2012 to 2016
for (year in 1:5) {
  # Get the AVE data frame for the current year
  UNCTAD_AVE_year <- combined_df_AVE_SPS_list[[year]]
  # Add the year as a new column
  UNCTAD_AVE_year$Year <- Y[year]
  UNCTAD_AVE_year <- rownames_to_column(UNCTAD_AVE_year, var = "HS_codes")
  merged_UNCTAD_AVE_list[[year]] <- UNCTAD_AVE_year
}

# Combine all data frames into a single data frame
merged_UNCTAD_AVE <- do.call(rbind, merged_UNCTAD_AVE_list)
merged_UNCTAD_AVE$source <- "UNCTAD"





################################################################################
#TBT 

combined_df_AVE_TBT <- list()
combined_df_AVE_TBT_list <- list()

# Loop through each year
for (year in unique(Merge_UNCTAD$Year)) {
  # Subset the data for the current year
  Count_H1_year <- Merge_UNCTAD %>%
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
UNCTAD_AVE_2012 <- combined_df_AVE_TBT_list[[1]]
UNCTAD_AVE_2013 <- combined_df_AVE_TBT_list[[2]]
UNCTAD_AVE_2014 <- combined_df_AVE_TBT_list[[3]]
UNCTAD_AVE_2015 <- combined_df_AVE_TBT_list[[4]]
UNCTAD_AVE_2016 <- combined_df_AVE_TBT_list[[5]]
UNCTAD_AVE_2012 <- rownames_to_column(UNCTAD_AVE_2012, var = "HS_codes")
UNCTAD_AVE_2013 <- rownames_to_column(UNCTAD_AVE_2013, var = "HS_codes")
UNCTAD_AVE_2014 <- rownames_to_column(UNCTAD_AVE_2014, var = "HS_codes")
UNCTAD_AVE_2015 <- rownames_to_column(UNCTAD_AVE_2015, var = "HS_codes")
UNCTAD_AVE_2016 <- rownames_to_column(UNCTAD_AVE_2016, var = "HS_codes")

merged_UNCTAD_AVE_list <- list()
Y <- c(2012,2013,2014,2015,2016)
# Loop through years 2012 to 2016
for (year in 1:5) {
  # Get the AVE data frame for the current year
  UNCTAD_AVE_year <- combined_df_AVE_TBT_list[[year]]
  # Add the year as a new column
  UNCTAD_AVE_year$Year <- Y[year]
  UNCTAD_AVE_year <- rownames_to_column(UNCTAD_AVE_year, var = "HS_codes")
  merged_UNCTAD_AVE_list[[year]] <- UNCTAD_AVE_year
}

# Combine all data frames into a single data frame
merged_UNCTAD_AVE <- do.call(rbind, merged_UNCTAD_AVE_list)
merged_UNCTAD_AVE$source <- "UNCTAD"








################################################################################

# map results 

# if combine with WTO data:for SPS 
merged_UNCTAD_AVE$source <- "UNCTAD"
merged_WTO_AVE$source <- "WTO"

# Combine the dataframes into a single dataframe
combined_AVE_SPS <- rbind(merged_UNCTAD_AVE, merged_WTO_AVE)

#plot_SPS <- ggplot(subset(combined_AVE_SPS,Year == 2016), aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
#  geom_pointrange() +
#  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
#  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
#  labs(title = "Total Ad Valorem Equivalents of SPS at HS1 level in 2016",
#       x = "HS1 level codes",
#       y = "Estimate of AVE") +
#  scale_color_manual(name = "Databases",  # Change legend title
#                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
#  theme_minimal()+
#  theme(plot.title = element_text(hjust = 0.5)) 
#plot_SPS
#ggsave("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_Count_time/SPS_2016_plot.png", plot_SPS, width = 10, height = 6, dpi = 300)
#



source_colors <- c("UNCTAD" = "blue", "WTO" = "red")

# Loop through each year from 2012 to 2016
for (year in 2012:2016) {
  # Filter data for the current year
  df_year <- subset(combined_AVE_SPS, Year == year)
  
  # Create the plot for the current year
  plot_SPS <- ggplot(df_year, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
    geom_pointrange() +
    geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
    geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
    labs(title = paste("Total Ad Valorem Equivalents of SPS at HS1 level in", year),
         x = "HS1 level codes",
         y = "Estimate of AVE") +
    scale_color_manual(name = "Databases", values = source_colors) +  # Specify colors
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Save the plot
  ggsave(filename = paste("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_Count_time/SPS_", year, "_plot.png", sep = ""),
         plot = plot_SPS, width = 10, height = 6, dpi = 300)
}


################################################################################



# if combine with WTO data:for SPS 
merged_UNCTAD_AVE$source <- "UNCTAD"
merged_WTO_AVE$source <- "WTO"

# Combine the dataframes into a single dataframe
combined_AVE_TBT <- rbind(merged_UNCTAD_AVE, merged_WTO_AVE)

geom_pointrange() +
  #plot_TBT <- ggplot(subset(combined_AVE_TBT,Year == 2012), aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
  #  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
  #  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
  #  labs(title = "Total Ad Valorem Equivalents of TBT at HS1 level in 2012",
  #       x = "HS1 level codes",
  #       y = "Estimate of AVE") +
  #  scale_color_manual(name = "Databases",  # Change legend title
  #                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
  #  theme_minimal()+
  #  theme(plot.title = element_text(hjust = 0.5)) 
  #plot_TBT
#ggsave("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_Count_time/TBT_2012_plot.png", plot_TBT, width = 10, height = 6, dpi = 300)
#


source_colors <- c("UNCTAD" = "blue", "WTO" = "red")

# Loop through each year from 2012 to 2016
for (year in 2012:2016) {
  # Filter data for the current year
  df_year <- subset(combined_AVE_TBT, Year == year)
  
  # Create the plot for the current year
  plot_TBT <- ggplot(df_year, aes(x = factor(HS_codes), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
    geom_pointrange() +
    geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
    geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
    labs(title = paste("Total Ad Valorem Equivalents of TBT at HS1 level in", year),
         x = "HS1 level codes",
         y = "Estimate of AVE") +
    scale_color_manual(name = "Databases", values = source_colors) +  # Specify colors
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Save the plot
  ggsave(filename = paste("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_Count_time/TBT_", year, "_plot.png", sep = ""),
         plot = plot_TBT, width = 10, height = 6, dpi = 300)
}


