
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
# if use count

# a) UNCTAD Version 

#load data:
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD_TBT.csv")

# create a count of NTM

dta_UNCTAD_SPS_reg <- subset(dta_UNCTAD_SPS, select = c(Year, Reporter, Partner ,HSCode,ntm_all ))
BRA_dta_UNCTAD_SPS <- subset(dta_UNCTAD_SPS_reg, Reporter =="BRA" )
BRA_dta_UNCTAD_SPS <- BRA_dta_UNCTAD_SPS %>% rename( NTM_BRA_SPS = ntm_all)
names(BRA_dta_UNCTAD_SPS)

# for TBT 
dta_UNCTAD_TBT_reg <- subset(dta_UNCTAD_TBT, select = c(Year, Reporter, Partner ,HSCode,ntm_all ))
BRA_dta_UNCTAD_TBT<- subset(dta_UNCTAD_TBT_reg,Reporter =="BRA" )
BRA_dta_UNCTAD_TBT <- BRA_dta_UNCTAD_TBT %>% rename( NTM_BRA_TBT = ntm_all)




################################################################################
#years from 2012 to 2016


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
table(BRA_trade_Merge$Year)
# select a year 
# BRA_trade <- subset(BRA_trade_Merge, Year == 2015)
BRA_trade <-BRA_trade_Merge



#2)  get NTM data:

# a) select the year of interest: 
table(BRA_dta_UNCTAD_SPS$Year)
BRA_UNCTAD_SPS <- subset(BRA_dta_UNCTAD_SPS, Year< 2017)
BRA_UNCTAD_TBT <- subset(BRA_dta_UNCTAD_TBT, Year< 2017)



# b) get rid of possible issues with WOLRD as partner


WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- c(unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]), 
                    unique(unctad_data$Partner[unctad_data$Partner != "WLD"]))  # Get unique partners that are not "WLD" from trade data
  # Filter rows where 'Partner' is 'WLD' from UNCTAD data
  wld_rows <- filter(unctad_data, Partner == "WLD")
  
  # Create new observations for each value in 'new_partners'
  new_observations <- lapply(new_partners, function(partner) {
    mutate(wld_rows, Partner = partner)})
  combined_data <- bind_rows(unctad_data %>%
                               filter(Partner != "WLD"), # Remove rows where Partner is 'WLD'
                             bind_rows(new_observations)) 
  
  #combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
}  #By hand at the bottom 

# Call the function with the provided dataframes
BRA_UNCTAD_SPS <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_SPS)
BRA_UNCTAD_TBT <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_TBT)


#calculate counts of NTM by partners and HS code 
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum(NTM_BRA_SPS, na.rm = TRUE))
summary(BRA_UNCTAD_SPS_count$total_NTM_BRA_SPS)
# check if duplicates
duplicates <- BRA_UNCTAD_SPS_count[duplicated(BRA_UNCTAD_SPS_count), ]

#for TBT 
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_TBT= sum(NTM_BRA_TBT, na.rm = TRUE))
summary(BRA_UNCTAD_TBT_count$total_NTM_BRA_TBT)




# rename variable
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS_count %>% rename(ReporterISO3 = Reporter,
                                                        PartnerISO3 = Partner,
                                                        ProductCode = HSCode,
                                                        ntm_count_SPS = total_NTM_BRA_SPS)
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT_count %>% rename(ReporterISO3 = Reporter,
                                                        PartnerISO3 = Partner,
                                                        ProductCode = HSCode,
                                                        ntm_count_TBT = total_NTM_BRA_TBT)





#3)  now merge the two:

Merge <- left_join(BRA_trade, BRA_UNCTAD_SPS_count)
Merge <- left_join(Merge, BRA_UNCTAD_TBT_count)

colSums(is.na(Merge))
# drop NA in NTM_bin
Merge$ntm_count_SPS <- ifelse(is.na(Merge$ntm_count_SPS), 0 , Merge$ntm_count_SPS) 
Merge$ntm_count_TBT <- ifelse(is.na(Merge$ntm_count_TBT), 0 , Merge$ntm_count_TBT) 



Merge$ProductCode <- as.factor(Merge$ProductCode)

write.csv(Merge, "/data/sikeme/TRADE/WTO/data/BRA/Merge_UNACTAD_count.csv")
Merge_UNCTAD <- Merge




###############################################################################

# B) Run the regression 

###############################################################################
# if need to load the data 



rm(list = ls())
Merge_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_UNACTAD_count.csv")
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
                ntm_count_SPS + ntm_count_TBT   + log(1+ MFN_tariff) +  Year+ ProductCode + 
                PartnerISO3 , data = Merge_UNCTAD)
summary(reg1)

transformation <- function(beta) ((exp(beta) - 1)) * 100
transformation(beta)






################################################################################

# 2) for each HS codes

# Run multiple regression for each HS codes using Poisson 



# create HS2 level var
Merge_UNCTAD$H2<- substring(Merge_UNCTAD$ProductCode   , first = 1, last = 1)
Merge_UNCTAD$H1<- substring(Merge_UNCTAD$ProductCode   , first = 1, last = 1)
unique(Merge_UNCTAD$H2)
Merge_UNCTAD$Year <- as.factor(Merge_UNCTAD$Year)


# Run for each H2

#re run the regression for each H2
unique_H2 <- unique(Merge_UNCTAD$H2)
# List to store regression results
regression_summaries <- list()

# Loop over each unique value of H2
for (h2_val in unique_H2) {
  # Subset the data for the current H2 value
  sub_Merge_UNCTAD <- subset(Merge_UNCTAD, H2 == h2_val)
  
  # Run the regression
  reg <- feols(log(price) ~ log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                   ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                   PartnerISO3,
                 data = sub_Merge_UNCTAD)
  
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
write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/regression_H1/regression_results_H1_UNCTAD.csv", sep = ",", row.names = FALSE)







################################################################################


# C) Estimate ad-valorem equivalents


###############################################################################

# Estimate AVE 

library(car)


df_list_AVE_SPS <- list()
for (i in 1:10) {
  # Calculate AVE_SPS and AVE_TBT for each element
  AVE_SPS <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_SPS) - 1)) * 100")
  df_AVE_SPS <- data.frame(AVE_SPS)
  rownames(df_AVE_SPS)<- paste0("HS", i-1)
  df_list_AVE_SPS[[i]] <- df_AVE_SPS
}
combined_df_AVE_UNCTAD_SPS <- do.call(rbind, df_list_AVE_SPS)
write.table(combined_df_AVE_UNCTAD_SPS, file = "/data/sikeme/TRADE/WTO/result/AVE_SPS_H1_UNCTAD.csv", sep = ",", row.names = TRUE)


df_list_AVE_TBT <- list()
for (i in 1:10) {
  # Calculate AVE_TBT  for each element
  AVE_TBT <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_TBT) - 1)) * 100")
  df_AVE_TBT <- data.frame(AVE_TBT)
  rownames(df_AVE_TBT)<- paste0("HS", i-1)
  df_list_AVE_TBT[[i]] <- df_AVE_TBT
}
combined_df_AVE_UNCTAD_TBT <- do.call(rbind, df_list_AVE_TBT)

write.table(combined_df_AVE_UNCTAD_TBT, file = "/data/sikeme/TRADE/WTO/result/AVE_TBT_H1_UNCTAD.csv", sep = ",", row.names = TRUE)


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


