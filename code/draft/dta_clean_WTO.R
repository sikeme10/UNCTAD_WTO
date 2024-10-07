

rm(list=ls())
# Set directory
setwd("~/TRADE/WTO/data")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)


################################################################################

#using Ghodsi's website, where missing HS codes have been found
# https://wiiw.ac.at/wiiw-ntm-data-ds-2.html
# can get info on: https://wiiw.ac.at/files/data/set/ds-0002-01-readme_1995_2019.txt


################################################################################
#  Variables 

#   HScombined .. 6-digit product code of the Harmonised System implied by the given raw HS code.
# HS1996 ...... HS_combined converted to HS revision 1996.
# ID .......... ID of NTM Notifications, linking the file to HS codes.
# IMP ......... Name of the NTM-imposing country
# IMP_ISO3 .... ISO3 country codes of the NTM-imposing country
# AFF ......... Name of the country affected by the NTM
# AFF_ISO3 .... ISO3 country codes of the affected country
# NTM ......... NTM types covered
# 1)  ADP - Antidumping
# 2)  CVD - Countervailing duties
# 3)  EXS - Export subsidies
# 4)  QRS - Quantitative Restrictions
# 5)  SFG - Safeguards
# 6)  SPS - Sanitary and Phytosanitary Measures
# 7)  SSG - Special Safeguards (agriculture)
# 8)  STE - State trading enterprises
# 9)  TBT - Technical barriers to trade
# 10) TRQ - Tariff-rate quotas
# +)  SPS_STC - specific trade concern raised against an SPS
# ++) TBT_STC - specific trade concern raised against a TBT
# SUB ............. Sub-requirements: e.g. price-based or volume-based measure, specific or ad valorem tariff increase etc.
# Year/Date ....... Year or date of (i) the initiation of a measure, (ii) its entry into force and (iii) its withdrawal
# DOC ............. References to WTO documents
# HSorigin ........ Information on how the product code was retrieved (step of the imputation procedure):
#   * ORI:     Original notified product codes
# * WTO:     WTO interpreted product codes
# * ICS/CAS: International Classificatin Standards notified or included in product descriptions
# * DES:     Matching product descriptions
# * TTBD:    Matching with the Temporary Trade Barriers Database of the World Bank/Chad Bown 
# * SET:     Set comparisons
# * MIS:     HS codes still missing
# * ALL:     Notifications with a product description 'all' or 'all commodities'
# DBorigin ........ WTO I-TIP or TTBD
# Measuredesc ..... Description of the measure
# Proddesc ........ Description of the affected products
# Keywords ........ Indicated keywords 
# ID_TTBD ......... Case ID used in the TTBD database


################################################################################
################################################################################
#1) Load the data


load("ds-0002-04-ntm_hs_and_notifications_1995_2019.rdata")

################################################################################
################################################################################
# Look at how the data is 


names(NTM_HS)
names(NTM_Notifications_1995_2019)

# check difference between year in force and year initiation
colSums(is.na(NTM_Notifications_1995_2019))
all(is.na(NTM_Notifications_1995_2019$Year_Initiation) == is.na(NTM_Notifications_1995_2019$Year_Inforce))
check <- NTM_Notifications_1995_2019[is.na(NTM_Notifications_1995_2019$Year_Initiation), ]
colSums(is.na(check))
all(is.na(check$Year_Initiation) == is.na(check$Year_Inforce))

#difference between the ID of the two datasets
differences <- setdiff(NTM_HS$ID,  NTM_Notifications_1995_2019$ID)
length(differences)
#so some HS code might not have observations 

################################################################################

# merge the two of them

merging <- function(data_HS, data_NTM) {
  #1) create a year var
  data_NTM$year <-  ifelse(!is.na(data_NTM$Year_Inforce),
                           data_NTM$Year_Inforce, data_NTM$Year_Initiation)
  #2) merge the data
  merged_data <- merge(data_HS, data_NTM, by = "ID", all.x = TRUE, 
                       all.y = TRUE, allow.cartesian=TRUE)
  #3) drop duplicates 
  merged_data <- unique(merged_data)
  #4) drop empty HS codes:
  merged_data <- merged_data[!is.na(merged_data$HScombined), ]
  return(merged_data)
}

merged_data <- merging(NTM_HS, NTM_Notifications_1995_2019)
names(merged_data)


# drop duplicates that have multiple year of initiation: ()
merged_data1 <- merged_data[!duplicated(merged_data[, c("ID", "HScombined", "HS1996", "IMP", "NTM","year")]), ]
duplicates <- merged_data[duplicated(merged_data[, c("ID", "HScombined", "HS1996", "IMP", "NTM","year","AFF")]),  ]


#check how looks like:
names(merged_data)
class(merged_data$HS1996)
colSums(is.na(merged_data))

#check how HS codes are looking 
check <- merged_data[is.na(merged_data$HScombined), ]
all(is.na(check$HScombined) == is.na(check$HS1996))

#check if duplicated observation:
duplicates <- merged_data[duplicated(merged_data), ]
duplicates
#still duplicates but with date of enforcement different





# export data
write.csv(merged_data, "clean_WTO.csv", row.names = FALSE)
merged_data <- read_csv("clean_WTO.csv")

################################################################################

#1) Separate by type of NTM

process_data <- function(data, NTM_type) {
  # Filter data based on NTM type
  filtered_data <- subset(data, NTM == NTM_type)
  filtered_data$NTM <- 1  #change the NTM variable
  names(filtered_data)[names(filtered_data) == "NTM"] <- NTM_type 
  write.csv(filtered_data, paste0("clean_WTO_", NTM_type, ".csv"), row.names = FALSE)
  return(filtered_data)
}

#Apply the function to  the two types of NTM
dta_SPS <-process_data(merged_data, "SPS")
names(dta_SPS)
dta_TBT <-process_data(merged_data, "TBT")
names(dta_TBT)

colSums(is.na(dta_SPS))
colSums(is.na(dta_TBT))

################################################################################
# the ID is counting the number of NTMs 
dta_SPS <- dta_SPS[!duplicated(dta_SPS[, c("HScombined", "HS1996","IMP","year","SPS","AFF")]), ]
length(unique(dta_SPS))

dta_TBT <- dta_TBT[!duplicated(dta_TBT[, c("HScombined", "HS1996","TBT","year","SPS","AFF")]), ]
length(unique(dta_TBT))


# we drop duplicate so that we get only a binary (sometimes NTMs present multiple times because of different ID)
#sometimes different ID for same NTRM (just possibly rectification on the first NTM) 

#if drop AFF 
table(dta_SPS$AFF)
dta_SPS <- dta_SPS[!duplicated(dta_SPS[, c("HScombined", "HS1996","IMP","year","SPS")]), ]
dta_TBT <- dta_TBT[!duplicated(dta_TBT[, c("HScombined", "HS1996","IMP","year","TBT")]), ]


################################################################################
# we are only going to do binary var for NTMs and not counts for the moment 


################################################################################

# 2) re balance the panel (without all year, IMP, and HS combinations )

year <- unique(dta_SPS$year)
HScombined <- unique(dta_SPS$HScombined)
IMP <- unique(dta_SPS$IMP)
AFF <- unique(dta_SPS$AFF)
all_combinations <- expand.grid(year=year, HScombined=HScombined,IMP=IMP)
panel_dta <- left_join(all_combinations,dta_SPS)

# use expand grid to get a full panel (with AFF)
full_panel <- function(data) {
  year <- unique(data$year)
  HScombined <- unique(data$HScombined)
  IMP <- unique(data$IMP)
  AFF <- unique(data$AFF)
  all_combinations <- expand.grid(year=year, HScombined=HScombined,IMP=IMP,AFF=AFF)
  #panel_dta <- left_join(all_combinations,data)
}

# use expand grid to get a full panel (without AFF)
full_panel <- function(data) {
  year <- unique(data$year)
  HScombined <- unique(data$HScombined)
  IMP <- unique(data$IMP)
  all_combinations <- expand.grid(year=year, HScombined=HScombined,IMP=IMP)
  panel_dta <- left_join(all_combinations,data)
}



#apply the function to the two data set
panel_dta_SPS <-full_panel(dta_SPS)
colSums(is.na(panel_dta_SPS))

panel_dta_TBT <-full_panel(dta_TBT)
colSums(is.na(panel_dta_TBT))
names(panel_dta_SPS)


# check the results
table(panel_dta_SPS$year)
table(panel_dta_SPS$IMP)
table(panel_dta_SPS$HScombined)
colSums(is.na(panel_dta_SPS))
#check why discrepancy between all_combinations and panel_dta-SPS
dta_SPS3 <- dta_SPS2[!duplicated(dta_SPS2[, c("ID", "HScombined", "HS1996", "IMP", "SPS","year")]), ]
duplicates <- panel_dta_SPS[duplicated(panel_dta_SPS[, c("HScombined", "HS1996", "IMP","year","AFF")]),  ]
duplicates1 <- panel_dta_SPS[duplicated(panel_dta_SPS[, c("HScombined", "HS1996", "IMP","year")]),  ]
duplicates <- arrange(duplicates, IMP, desc(year), HScombined)




################################################################################
# for faster processing 
#take only two country for the moment

table(panel_dta_SPS$IMP)

panel_dta_SPS_sub <- subset(panel_dta_SPS, IMP %in% c("Brazil"))
colSums(is.na(panel_dta_SPS_sub))
table(panel_dta_SPS_sub$SPS)

################################################################################
# as no withdrawal,  if implement an NTM need to leave it 

names(panel_dta_SPS)
table(panel_dta_SPS$SPS)
panel_dta_SPS_sub$SPS <- ifelse(is.na(panel_dta_SPS_sub$SPS), 0, panel_dta_SPS_sub$SPS)
panel_dta_SPS$SPS <- as.numeric(panel_dta_SPS$SPS)

cum_Sum <- function(data) {
  data$SPS <- as.numeric(data$SPS)
  data <- data %>% arrange(year, IMP, HScombined) %>%
    group_by(IMP, HScombined) %>%      # Grouping by product
    mutate(SPS1 = cummax(replace(SPS, is.na(SPS), 0)))
}

panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS_sub)
colSums(is.na(panel_dta_SPS_sub))
table(panel_dta_SPS_sub$SPS1)
table(panel_dta_SPS_sub$SPS)
table(panel_dta_SPS_sub$SPS, panel_dta_SPS_sub$SPS1)

plot_SPS <- panel_dta_SPS_sub %>%  filter(SPS1 == 1)
# Group the data by year and count the number of distinct HS codes within each group
distinct_hs_codes <- plot_SPS %>%
  group_by(year) %>% summarise(distinct_hs_codes = n_distinct(HScombined))
ggplot(distinct_hs_codes, aes(x = year, y = distinct_hs_codes)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of Distinct HS Codes with Treatment = 1") +
  theme_minimal()


panel_dta_SPS <-  MoveFront(panel_dta_SPS, c("IMP", "HScombined", "year", "SPS", "SPS1"))

################################################################################



plot_distinct_hs_codes <- function(country, data) {
  # Subset the data for the specified country
  panel_dta_SPS_sub <- subset(data, IMP %in% country)
  
  # Ensure SPS is numeric
  panel_dta_SPS_sub$SPS <- ifelse(is.na(panel_dta_SPS_sub$SPS), 0, panel_dta_SPS_sub$SPS)
  panel_dta_SPS_sub$SPS <- as.numeric(panel_dta_SPS_sub$SPS)
  
  cum_Sum <- function(data) {
    data$SPS <- as.numeric(data$SPS)
    data <- data %>% arrange(year, IMP, HScombined) %>%
      group_by(IMP, HScombined) %>%      # Grouping by product
      mutate(SPS1 = cummax(replace(SPS, is.na(SPS), 0)))
  }
  
  panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS_sub)
  
  # Return both the modified data frame and the plot
  return(panel_dta_SPS_sub)
}

# Usage example:
result <- plot_distinct_hs_codes("Brazil", panel_dta_SPS)
colSums(is.na(result))
table(result$SPS1)
table(result$SPS)
table(result$SPS, result$SPS1)


result <- plot_distinct_hs_codes("Canada", panel_dta_SPS)

# Usage example:
plot_distinct_hs_codes("Canada", panel_dta_SPS)








################################################################################


# Export Clean data 

write.csv(merged_data_TBT, "clean_WTO_TBT.csv", row.names = FALSE)

write.csv(merged_data_SPS, "clean_WTO_SPS.csv", row.names = FALSE)














