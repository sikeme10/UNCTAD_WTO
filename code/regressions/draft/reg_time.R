
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
# 1) Get the countries for the IVs

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
  
  combined_data <- distinct(combined_data)# Drop duplicate observations
  return(combined_data)
}  #By hand at the bottom 

# Call the function with the provided dataframes
BRA_UNCTAD_SPS <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_SPS)
BRA_UNCTAD_TBT <- WLD_partner(BRA_trade_Merge, BRA_UNCTAD_TBT)
names(BRA_UNCTAD_SPS)
names(BRA_UNCTAD_TBT)


#calculate counts of NTM by partners and HS code 
BRA_UNCTAD_SPS_count <- BRA_UNCTAD_SPS %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_SPS = sum(NTM_BRA_SPS, na.rm = TRUE))
# check if duplicates
duplicates <- BRA_UNCTAD_SPS_count[duplicated(BRA_UNCTAD_SPS_count), ]

#for TBT 
BRA_UNCTAD_TBT_count <- BRA_UNCTAD_TBT %>%
  group_by(Year, Reporter, Partner, HSCode) %>%
  summarise(total_NTM_BRA_TBT= sum(NTM_BRA_TBT, na.rm = TRUE))
# check if duplicates
duplicates <- BRA_UNCTAD_TBT_count[duplicated(BRA_UNCTAD_TBT_count), ]


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


#reg1 <- lm(log(price) ~
#             log(dist) + contig + comlang_off + rta + landlocked_o +landlocked_d +
#             ntm_bin  + MFN_tariff+
#             ProductCode +
#             PartnerISO3 ,    data = Merge)
#



library(fixest)
names(Merge)
# Run the regression model with fixest
reg1 <- feols(log(price) ~
                log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                ntm_count_SPS + ntm_count_TBT   + log(1+ MFN_tariff) +  Year+ ProductCode + 
                PartnerISO3 , data = Merge)
summary(reg1)




###############################################################################
# get coefficient at H2 level



names(Merge)
str(Merge)


#re run the regression 
reg1 <- feols(log(price) ~
                log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                Year*ntm_count_SPS + Year*ntm_count_TBT   + log(1+ MFN_tariff) +  Year+ ProductCode + 
                PartnerISO3 , data = Merge)
summary(reg1)




###############################################################################

# create HS2 level var
Merge$H2<- substring(Merge$ProductCode   , first = 1, last = 1)
unique(Merge$H2)



# Run for each H2

#re run the regression for each H2
unique_H2 <- unique(Merge$H2)
# List to store regression results
regression_summaries <- list()

# Loop over each unique value of H2
for (h2_val in unique_H2) {
  # Subset the data for the current H2 value
  sub_merge <- subset(Merge, H2 == h2_val)
  
  # Run the regression
  reg <- feols(log(price) ~
                 log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                 ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                 PartnerISO3,
               data = sub_merge)
  
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
                        headers = "UNCTAD")
names(summary_table) <- c("vars","H1", "H2", "H3","H4", "H5", "H6","H7", "H8", "H9")
summary_table_2 <- summary_table[1:12, ]
names(summary_table)
write.table(summary_table_2, file = "/data/sikeme/TRADE/WTO/result/regression_results_H1_UNCTAD.csv", sep = ",", row.names = FALSE)


# or 
# get results in
selected_summaries <- list()
for (i in seq_along(regression_summaries)) {
  # Obtain the summary table
  summary_table <- etable(regression_summaries[[i]])
  
  # Rename variables
  names(summary_table) <- c("var_name", paste0("values_", i))
  
  # Select specific variables
  selected_summary <- summary_table[summary_table$var_name %in% c("ntm_count_SPS", "ntm_count_TBT", "log(1+MFN_tariff)"), ]
  
  # Store the selected and renamed summary table in the list
  selected_summaries[[i]] <- selected_summary
}
merged_summary <- selected_summaries %>% reduce(full_join, by = "var_name")


###############################################################################

library(car)

#get individual AVE 
AVE_SPS <- deltaMethod(regression_summaries[[1]], "((exp(ntm_count_SPS) - 1)) * 100")
AVE_TBT <- deltaMethod(regression_summaries[[1]], "((exp(ntm_count_TBT) - 1)) * 100")


# get 
df_list_AVE_SPS <- list()
for (i in 1:9) {
  # Calculate AVE_SPS and AVE_TBT for each element
  AVE_SPS <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_SPS) - 1)) * 100")
  df_AVE_SPS <- data.frame(AVE_SPS)
  rownames(df_AVE_SPS)<- paste0("AVE SPS H", i)
  df_list_AVE_SPS[[i]] <- df_AVE_SPS
}
combined_df_AVE_SPS <- do.call(rbind, df_list_AVE_SPS)

df_list_AVE_TBT <- list()
for (i in 1:9) {
  # Calculate AVE_TBT  for each element
  AVE_TBT <- deltaMethod(regression_summaries[[i]], "((exp(ntm_count_TBT) - 1)) * 100")
  df_AVE_TBT <- data.frame(AVE_TBT)
  rownames(df_AVE_TBT)<- paste0("AVE TBT H", i)
  df_list_AVE_TBT[[i]] <- df_AVE_TBT
}
combined_df_AVE_TBT <- do.call(rbind, df_list_AVE_TBT)






###############################################################################

# or regress on each: 

#by H2
regression_results <- Merge %>%
  group_by(H2) %>%
  do(
    # Run the regression within each group
    reg <- lm(log(price) ~
                   log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                   ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                   PartnerISO3, 
                 data = .)
  )

# View the results
print(regression_results)

# Determine the transformation function
transformation <- function(beta) (tan(exp(beta)*HS4 - 1)) * 100
transformation(beta)



###############################################################################








