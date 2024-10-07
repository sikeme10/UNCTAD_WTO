
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




#1) get trade merged data 
BRA_trade_Merge <- read_csv("BRA/BRA_trade_Merge.csv")
names(BRA_trade_Merge)
BRA_trade_Merge <- select(BRA_trade_Merge,
                          "Year", "Nomenclature","ReporterISO3",
                          "ProductCode","ReporterName",
                          "PartnerISO3", "PartnerName",
                          "price","rta","contig","dist",
                          "comlang_off","country_o",
                          "landlocked_o","landlocked_d",
                          "ln_Gdp_per_cap","ln_GDP_Brazil", "MFN_tariff")
# select a year 
BRA_trade <- subset(BRA_trade_Merge, Year >2011 & Year <2017)


################################################################################

#2) clean WTO data 


# a) load data:
dta_WTO_SPS <- read_csv("BRA/clean_WTO_SPS_count.csv")
dta_WTO_TBT <- read_csv("BRA/clean_WTO_TBT_count.csv")
names(dta_WTO_SPS)

# b) select the year of interest: 
dta_WTO_SPS <- subset(dta_WTO_SPS, Year >2011 & Year <2017)
dta_WTO_TBT <- subset(dta_WTO_TBT, Year >2011 & Year <2017)

# c) rename some of the variables and select required data
dta_WTO_SPS <- dta_WTO_SPS %>% rename(ntm_count_SPS = SPS1)
dta_WTO_SPS <- select(dta_WTO_SPS, ReporterISO3 , ProductCode, PartnerISO3,Year,ntm_count_SPS)
table(dta_WTO_SPS$PartnerISO3)
dta_WTO_TBT <- dta_WTO_TBT %>% rename(ntm_count_TBT = TBT1)
dta_WTO_TBT <- select(dta_WTO_TBT, ReporterISO3 , ProductCode, PartnerISO3,Year,ntm_count_TBT)
table(dta_WTO_TBT$PartnerISO3)




# d) get rid of possible issues with WOLRD as partner
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
  combined_data <- combined_data %>% group_by(Year, ReporterISO3,ProductCode, PartnerISO3) %>%
    summarise(ntm_count_SPS = sum(ntm_count_SPS))
  return(combined_data)
}
SPS <- WLD_partner(BRA_trade, dta_WTO_SPS)

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
  combined_data <- combined_data %>% group_by(Year, ReporterISO3,ProductCode, PartnerISO3) %>%
    summarise(ntm_count_TBT = sum(ntm_count_TBT))
  return(combined_data)
}
TBT <- WLD_partner_TBT(BRA_trade, dta_WTO_TBT)




################################################################################

#3)  now merge the two:

Merge <- left_join(BRA_trade, SPS)
Merge <- left_join(Merge, TBT)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_count_SPS <- ifelse(is.na(Merge$ntm_count_SPS), 0 , Merge$ntm_count_SPS) 
Merge$ntm_count_TBT <- ifelse(is.na(Merge$ntm_count_TBT), 0 , Merge$ntm_count_TBT) 

write.csv(Merge, "/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count.csv")






###############################################################################

# B) Run the regression 

###############################################################################
rm(list = ls())
Merge <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count.csv")
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
                ntm_count_SPS + ntm_count_TBT   + log(1+ MFN_tariff) +  Year+ ProductCode + 
                PartnerISO3 , data = Merge)
summary(reg1)

transformation <- function(beta) ((exp(beta) - 1)) * 100
transformation(beta)






################################################################################

# Run multiple regression for each HS codes 



# create HS2 level var
Merge$H1<- substring(Merge$ProductCode   , first = 1, last = 1)
Merge$H2<- substring(Merge$ProductCode   , first = 1, last = 2)
Merge$H3<- substring(Merge$ProductCode   , first = 1, last = 3)
Merge$H4<- substring(Merge$ProductCode   , first = 1, last = 4)
Merge$Year <- as.factor(Merge$Year)


# Run for each H1

#re run the regression for each H1
unique_H2 <- unique(Merge$H2)
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
for (i in seq_along(unique_H2)) {
  cat("Summary for H2 =", unique_H2[i], ":\n")
  print(regression_summaries[[i]])
}


fixest_list <- list(regression_summaries[[1]])

for (i in 2:length(regression_summaries)) {
  fixest_list <- c(fixest_list, list(regression_summaries[[i]]))
}

summary_table <- etable(fixest_list)

names(summary_table) <- c("vars", paste0("HS", 1:96))

summary_table_2  <- rbind(summary_table[1:14, ], summary_table[5193:5195, ])
names(summary_table)
write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/regression_results_H2_WTO.csv", sep = ",", row.names = FALSE)






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
  rownames(df_AVE_SPS) <- paste0("HS", i)
  df_list_AVE_SPS[[i]] <- df_AVE_SPS
}
combined_df_AVE_WTO_SPS <- do.call(rbind, df_list_AVE_SPS)
write.csv(combined_df_AVE_WTO_SPS, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_SPS_H2_WTO.csv", row.names = TRUE)




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

write.csv(combined_df_AVE_WTO_TBT, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_TBT_H2_WTO.csv", row.names = TRUE)



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





