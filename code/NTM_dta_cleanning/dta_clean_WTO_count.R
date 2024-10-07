

rm(list=ls())
# Set directorys
setwd("/data/sikeme/TRADE/WTO/data/dta_WTO")
# setwd("/data")
# setwd("/home")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)


################################################################################

#using Ghodsi's website, where missing HS codes have been found
# https://wiiw.ac.at/wiiw-ntm-data-ds-2.html
# can get info on: https://wiiw.ac.at/files/data/set/ds-0002-01-readme_1995_2019.txt

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
# STEPS of the code

# 1) We first load the data 
# 2) Merge the two data from the Pronto file, and export it
# 3) separate between TBT and SPS type 
# 4) Solve some of the HS code issue,a nd convert at HS 4 nomenclature 
#    a) Get HS revision from WITS data
#    b) SPS: HS code concordance with WITS data 
#    c) TBT: HS code concordance with WITS data 
#    We get two data sets: dta_SPS_clean, dta_TBT_clean
# 5) Count NTM, as each distinct ID represent different NTM
# 6) re balance the panel to make cumulative NTMs counts
# 7) select country of interest and look at cumulative NTMs counts (once an NTM is imposed it stays imposed)
# 8) Harmonize name of variables 
# 9) export
# 10) Vizualise

################################################################################

################################################################################

#1) Load the data

################################################################################

load("ds-0002-04-ntm_hs_and_notifications_1995_2019.rdata")


# Look at how the data is 
names(NTM_HS)
names(NTM_Notifications_1995_2019)

table(NTM_HS$ID)
table(NTM_Notifications_1995_2019$ID)


################################################################################

# 2)  merge the two of them

################################################################################

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
  merged_data <- merged_data[!is.na(merged_data$HScombined) | !is.na(merged_data$HS1996), ]
}

merged_data <- merging(NTM_HS, NTM_Notifications_1995_2019)
names(merged_data)
colSums(is.na(merged_data))

# Some columns did not match, (all IDs did not get matched) so drop them 
merged_data <- merged_data[!is.na(merged_data$IMP), ]

# export data
write.csv(merged_data, "NTM_data/clean_WTO_1.csv", row.names = FALSE)
merged_data <- read_csv("NTM_data/clean_WTO_1.csv")






################################################################################

# 4) solve HS code issues:

################################################################################
# Some HS codes are not always retrieved so use HS1996 codes to get HS4 revision codes back


# We have to HS codes variables: HS1996 and HScombined, 
# the objectif is to concord all HS codes at the HS4 revisions (to easily match it with UNCTAD data, and trade data)
# If the HS combined revision does not match correctly, we use the variable HS1996 code present in the data


# 1)  first put HS codes as character var in the WTO data (check if 6 characters, add 0 if not)
class(merged_data$HScombined) #check
class(merged_data$HS1996)
merged_data$HScombined <- as.character(merged_data$HScombined)
merged_data$HScombined <- ifelse(nchar(merged_data$HScombined) == 5 & !is.na(merged_data$HScombined), paste0("0", merged_data$HScombined), merged_data$HScombined)
merged_data$HS1996 <- as.character(merged_data$HS1996)
merged_data$HS1996 <- ifelse(nchar(merged_data$HS1996) == 5 & !is.na(merged_data$HS1996), paste0("0", merged_data$HS1996), merged_data$HS1996)
# checks: num_characters <- all(nchar(merged_data$HS1996) == 6)


# a) Load data with HS revision info from WITS 
HS_to_H4 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-109_Concordance_HS_to_H4.CSV")
H4 <- HS_to_H4 %>% rename(HScomb = 'HS - Combined  Product Code', HS4code ='HS 2012 Product Code') %>% select(HScomb, HS4code)
class(H4$HScomb)
length(unique(H4$HS4code))
length(unique(H4$HScomb))
length(unique(merged_data$HScombined))


# b) Match wih HS combined and HS1996 to get all in HS 4 revision
# use concord function to concond HS1 revision to HS4 revision
library(concordance)
merged_data <- left_join(merged_data, H4, by = c("HScombined" = "HScomb"))
colSums(is.na(merged_data))
if(any(is.na(merged_data$HS4code))) {
  # Create an index for rows where HS4code is NA and HS1996 is not NA
  na_index <- is.na(merged_data$HS4code) & !is.na(merged_data$HS1996)
  
  # Apply concord_hs only to rows where HS4code is NA and HS1996 is not NA
  merged_data$HS4code[na_index] <- concord_hs(merged_data$HS1996[na_index], origin = "HS1", destination = "HS4", dest.digit = 6)
}
length(unique(merged_data$HS4code))
colSums(is.na(merged_data))

# c) if HS4 code is NA drop them 
# choce is debatable ...
merged_data <- merged_data[!is.na(merged_data$HS4code), ]



################################################################################

# 3) Separate by type of NTM

################################################################################

# check type of NTMs:
table(merged_data$NTM)

process_data <- function(data, NTM_type) {
  # Filter data based on NTM type
  filtered_data <- subset(data, NTM == NTM_type)
  filtered_data$NTM <- 1  #change the NTM variable
  names(filtered_data)[names(filtered_data) == "NTM"] <- NTM_type 
  #write.csv(filtered_data, paste0("clean_WTO_", NTM_type), row.names = FALSE)
  return(filtered_data)
}

#Apply the function to  the two types of NTM
dta_SPS <-process_data(merged_data, "SPS")
dta_TBT <-process_data(merged_data, "TBT")


colSums(is.na(dta_SPS))
colSums(is.na(dta_TBT))







################################################################################

#5) each  distinct ID represents different count of NTM

################################################################################

# for SPS case:
#we want a count of NTM instead of binary 
names(dta_SPS)
dta_SPS$SPS <- as.numeric(dta_SPS$SPS)
summary(dta_SPS$SPS)
# add all NTM counts 
dta_SPS_clean <- dta_SPS %>% group_by(year,IMP, AFF, HS4code) %>% 
  summarise(SPS_count = n_distinct(ID)) # each ID represent a notification, so to get cumulative add all distinct ID
summary(dta_SPS_clean$SPS_count)
length(unique(dta_SPS_clean$HS4code)) # should get 5205

# for TBT case:
#we want a count of NTM instead of binary 
names(dta_TBT)
dta_TBT$TBT <- as.numeric(dta_TBT$TBT)
summary(dta_TBT$TBT)
# add all NTM counts 
dta_TBT_clean <- dta_TBT %>% group_by(year,IMP, AFF, HS4code) %>% 
  summarise(TBT_count  = n_distinct(ID))
summary(dta_TBT_clean$TBT_count)
length(unique(dta_TBT_clean$HS4code))


################################################################################

# 6) re balance the panel (without all year, IMP, and HS combinations )

################################################################################

# separate between SPS imposed on all memebers and NTM imposed on specific members
dta_SPS_clean_1 <-subset(dta_SPS_clean, AFF =="All Members")
dta_SPS_clean_2 <-subset(dta_SPS_clean, AFF !="All Members")


dta_TBT_clean_1 <-subset(dta_TBT_clean, AFF =="All Members")
dta_TBT_clean_2 <-subset(dta_TBT_clean, AFF !="All Members")
table(dta_TBT_clean$AFF)

# use expand grid to get a full panel (without AFF)
full_panel_WLD <- function(data) {
  year <- unique(data$year)
  HS4code <- unique(data$HS4code)
  IMP <- unique(data$IMP)
  all_combinations <- expand.grid(year=year, HS4code=HS4code,IMP=IMP)
  panel_dta <- left_join(all_combinations,data)
}



#apply the function to the two data set
panel_dta_SPS <-full_panel_WLD(dta_SPS_clean_1)
# put AFF back (world)
table(panel_dta_SPS$AFF)
panel_dta_SPS$AFF<- "All Members"
colSums(is.na(panel_dta_SPS))

panel_dta_TBT <-full_panel_WLD(dta_TBT_clean_1)
colSums(is.na(panel_dta_TBT))
table(panel_dta_TBT$AFF)
panel_dta_TBT$AFF<- "All Members"



# for non all world members 
# use expand grid to get a full panel (with AFF)
full_panel_WLD_2 <- function(data) {
  year <- unique(data$year)
  HS4code <- unique(data$HS4code)
  IMP <- unique(data$IMP)
  AFF <- unique(data$AFF)
  all_combinations <- expand.grid(year=year, HS4code=HS4code,IMP=IMP, AFF = AFF)
  panel_dta <- left_join(all_combinations,data)
}
panel_dta_SPS_2 <-full_panel_WLD_2(dta_SPS_clean_2)
panel_dta_TBT_2 <-full_panel_WLD_2(dta_TBT_clean_2)
head(panel_dta_SPS_2)
colSums(is.na(panel_dta_SPS_2))
write.csv(panel_dta_SPS_2, "WTO_panel_dta_SPS2.csv", row.names = FALSE)


#join back the two 
panel_dta_SPS <- rbind(panel_dta_SPS,panel_dta_SPS_2 )
unique(panel_dta_SPS$AFF)

write.csv(panel_dta_TBT, "NTM_data/WTO_panel_dta_TBT.csv", row.names = FALSE)
write.csv(panel_dta_SPS, "NTM_data/WTO_panel_dta_SPS.csv", row.names = FALSE)
# rm(list=ls())
panel_dta_TBT <- read_csv("NTM_data/WTO_panel_dta_TBT.csv")
panel_dta_SPS <- read_csv("NTM_data/WTO_panel_dta_SPS.csv")
table(panel_dta_SPS$AFF)
table(panel_dta_SPS$IMP)


################################################################################

# 7) Cumulative NTM counts

################################################################################
panel_dta_SPS <- read_csv("NTM_data/WTO_panel_dta_SPS.csv")

# a)  calculate cumulated SPS counts 
cum_Sum <- function(data) {
       data$SPS_count <- as.numeric(data$SPS_count)
       data$SPS_count <- ifelse(is.na(data$SPS_count), 0, data$SPS_count)
       data <- data %>%
         arrange(year, IMP, HS4code, AFF) %>%
         group_by(IMP, AFF, HS4code) %>%      # Grouping by product
         mutate(SPS1 = cumsum(replace(SPS_count, is.na(SPS_count), 0)))
}

summary(panel_dta_SPS$SPS_count)
panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS)
summary(panel_dta_SPS_sub$SPS1)

write.csv(panel_dta_SPS_sub, "NTM_data/WTO_panel_dta_SPS_2.csv", row.names = FALSE)


test<- panel_dta_SPS_sub %>% filter(IMP=="Brazil")
test <- test %>%arrange(HS4code, IMP, AFF)


# to lighten the dta can dop all zeros... and export it 

panel_dta_SPS3 <- panel_dta_SPS_sub %>% filter(SPS1 !=0)
panel_dta_SPS3 <- panel_dta_SPS3 %>%arrange(HS4code, IMP, AFF)
write.csv(panel_dta_SPS3, "NTM_data/WTO_panel_dta_SPS_3.csv", row.names = FALSE)

rm(list=ls())



# b) calculate cumulated TBT counts 

panel_dta_TBT <- read_csv("NTM_data/WTO_panel_dta_TBT.csv")

cum_Sum <- function(data) {
  data$TBT_count <- as.numeric(data$TBT_count)
  data$TBT_count <- ifelse(is.na(data$TBT_count), 0, data$TBT_count)
  data <- data %>%
    arrange(year, IMP, HS4code, AFF) %>%
    group_by(IMP,AFF,  HS4code) %>%      # Grouping by product
    mutate(TBT1 = cumsum(replace(TBT_count, is.na(TBT_count), 0)))
}
summary(panel_dta_TBT$TBT_count)
panel_dta_TBT_sub <- cum_Sum(panel_dta_TBT)
summary(panel_dta_TBT_sub$TBT1)


test<- panel_dta_TBT_sub %>% filter(IMP=="Brazil")
test <- test %>%arrange(HS4code, IMP, AFF)


# to lighten the dta can dop all zeros... and export it 
panel_dta_TBT1 <- panel_dta_TBT_sub %>% filter(TBT1 !=0)
panel_dta_TBT2 <- panel_dta_TBT1 %>%arrange(HS4code, IMP, AFF)
write.csv(panel_dta_TBT2, "NTM_data/WTO_panel_dta_TBT_3.csv", row.names = FALSE)


################################################################################

# 7 bis) If do It by country

################################################################################

# for unique countries

#   # for faster processing 
#   #take only one country
#   IMP <- unique(panel_dta_SPS$IMP)
#   
#   panel_dta_SPS_sub <- subset(panel_dta_SPS, IMP %in% c("Brazil"))
#   # as no withdrawal,  if implement an NTM need to leave it 
#   cum_Sum <- function(data) {
#     data$SPS_count <- as.numeric(data$SPS_count)
#     data$SPS_count <- ifelse(is.na(data$SPS_count), 0, data$SPS_count)
#     data <- data %>%
#       arrange(year, IMP, HS4code) %>%
#       group_by(IMP, HS4code) %>%      # Grouping by product
#       mutate(SPS1 = cumsum(replace(SPS_count, is.na(SPS_count), 0)))
#   }
#   panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS_sub)
#   
#   #function alone
#   panel_dta_SPS_sub$SPS_count <- ifelse(is.na(panel_dta_SPS_sub$SPS_count), 0, panel_dta_SPS_sub$SPS_count)
#   panel_dta_SPS_sub <- panel_dta_SPS_sub %>%
#     arrange(year, IMP,AFF, HS4code) %>%
#     group_by(IMP, AFF, HS4code) %>%      # Grouping by product
#     mutate(SPS1 = cumsum(replace(SPS_count, is.na(SPS_count), 0)))


################################################################################


# or apply this function: 
unique(panel_dta_SPS$IMP)
names(panel_dta_SPS)



# for SPS
SPS_country <- function(country, data) {
  # Subset the data for the specified country
  panel_dta_SPS_sub <- subset(data, IMP %in% country)
  # Ensure SPS is numeric
  panel_dta_SPS_sub$SPS_count <- as.numeric(panel_dta_SPS_sub$SPS_count)
  # 
  
  cum_Sum <- function(data) {
    data$SPS_count <- as.numeric(data$SPS_count)
    data$SPS_count <- ifelse(is.na(data$SPS_count), 0, data$SPS_count)
    data <- data %>%
      arrange(year, IMP, AFF, HS4code) %>%
      group_by(IMP, AFF, HS4code) %>%      # Grouping by product
      mutate(SPS1 = cumsum(replace(SPS_count, is.na(SPS_count), 0)))
  }
  panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS_sub)
  # Return both the modified data frame and the plot
  return(panel_dta_SPS_sub)
}




# for TBT
unique(panel_dta_TBT$IMP)

TBT_country <- function(country, data) {
  # Subset the data for the specified country
  panel_dta_TBT_sub <- subset(data, IMP %in% country)
  # Ensure SPS is numeric
  panel_dta_TBT_sub$TBT_count <- as.numeric(panel_dta_TBT_sub$TBT_count)
  cum_Sum <- function(data) {
    data$TBT_count <- as.numeric(data$TBT_count)
    data$TBT_count <- ifelse(is.na(data$TBT_count), 0, data$TBT_count)
    data <- data %>%
      arrange(year, IMP, HS4code) %>%
      group_by(IMP, HS4code) %>%      # Grouping by product
      mutate(TBT1 = cumsum(replace(TBT_count, is.na(TBT_count), 0)))
  }
  panel_dta_TBT_sub <-cum_Sum(panel_dta_TBT_sub)
  # Return both the modified data frame and the plot
  return(panel_dta_TBT_sub)
}


# apply functions to the country of interest :

# other countries of interest:
# Ecuador Colombia Peru Chile Bolivia
# Brazil
#"BRA", "CHL","BOL", "COL","ECU", "PER"

panel_dta_SPS_sub <- SPS_country("Brazil", panel_dta_SPS)

panel_dta_SPS_sub <- SPS_country("Brazil", panel_dta_SPS)
panel_dta_SPS_sub <- SPS_country("Ecuador", panel_dta_SPS)
panel_dta_SPS_sub <- SPS_country("Colombia", panel_dta_SPS)
panel_dta_SPS_sub <- SPS_country("Peru", panel_dta_SPS)
panel_dta_SPS_sub <- SPS_country("Bolivia", panel_dta_SPS)

# check
table(panel_dta_SPS_sub$SPS_count)
table(panel_dta_SPS_sub$SPS1)
table(panel_dta_SPS_sub$IMP)

panel_dta_TBT_sub <- TBT_country("Brazil", panel_dta_TBT)
panel_dta_TBT_sub <- TBT_country("Ecuador", panel_dta_TBT)
panel_dta_TBT_sub <- TBT_country("Colombia", panel_dta_TBT)
panel_dta_TBT_sub <- TBT_country("Peru", panel_dta_TBT)
panel_dta_TBT_sub <- TBT_country("Bolivia", panel_dta_TBT)
# check
table(panel_dta_TBT_sub$TBT_count)
table(panel_dta_TBT_sub$TBT1)
table(panel_dta_TBT_sub$IMP)




################################################################################

# 8) Harmonize name of variables 

################################################################################
rm(list=ls())

panel_dta_TBT_sub <- read_csv("NTM_data/WTO_panel_dta_TBT_3.csv")
panel_dta_SPS_sub <- read_csv("NTM_data/WTO_panel_dta_SPS_3.csv")

names(panel_dta_SPS_sub)
names(panel_dta_TBT_sub)


# Function to convert to isocode and match trade data :
library(countrycode)
isocode <- function(data, WLD_vector) {
  data$ReporterISO3 <- countrycode(data$IMP, "country.name", "iso3c")
  data$PartnerISO3 <- countrycode(data$AFF, "country.name", "iso3c")
  data$ReporterISO3[data$IMP == "European Union"] <- "EUN"
  data$PartnerISO3[data$AFF == "European Union"] <- "EUN"
  data$PartnerISO3[data$AFF == "All Members"] <- "WLD"
  data <- data %>% rename(ProductCode = HS4code,
                          Year = year)
  return(data)
}

panel_dta_SPS_sub <- isocode(panel_dta_SPS_sub)
panel_dta_TBT_sub <- isocode(panel_dta_TBT_sub)


###############################################################################

#9) Export Clean data 

###############################################################################

# change country names: 

write.csv(panel_dta_TBT_sub, "clean_WTO_TBT.csv", row.names = FALSE)
write.csv(panel_dta_SPS_sub, "clean_WTO_SPS.csv", row.names = FALSE)









############################################################################################


# or run a function forcountry specific ones 

# can directly load the data
panel_dta_TBT <- read_csv("NTM_data/WTO_panel_dta_TBT.csv")
panel_dta_SPS <- read_csv("NTM_data/WTO_panel_dta_SPS.csv")


countries <- c("Brazil", "Ecuador", "Colombia", "Peru", "Chile", "Bolivia")
countries <- c( "Bolivia")

for (country in countries) {
  # Process SPS data
  panel_dta_SPS_sub <- SPS_country(country, panel_dta_SPS)
  panel_dta_SPS_sub <- isocode(panel_dta_SPS_sub)
  
  # Process TBT data
  panel_dta_TBT_sub <- TBT_country(country, panel_dta_TBT)
  panel_dta_TBT_sub <- isocode(panel_dta_TBT_sub)
  
  # Generate ISO code for filenames
  country_iso <- countrycode(country, "country.name", "iso3c")
  
  assign(paste0("panel_dta_SPS_", country_iso), panel_dta_SPS_sub)
  assign(paste0("panel_dta_TBT_", country_iso), panel_dta_TBT_sub)
  
  # Save processed data to CSV files
  write.csv(panel_dta_TBT_sub, paste0("NTM_data/countries/clean_WTO_TBT_", country_iso, ".csv"), row.names = FALSE)
  write.csv(panel_dta_SPS_sub, paste0("NTM_data/countries/clean_WTO_SPS_", country_iso, ".csv"), row.names = FALSE)
}





for (country in countries) {
  # Process TBT data
  panel_dta_TBT_sub <- TBT_country(country, panel_dta_TBT)
  panel_dta_TBT_sub <- isocode(panel_dta_TBT_sub)
  
  # Generate ISO code for filenames
  country_iso <- countrycode(country, "country.name", "iso3c")
  
  assign(paste0("panel_dta_TBT_", country_iso), panel_dta_TBT_sub)
  
  # Save processed data to CSV files
  write.csv(panel_dta_TBT_sub, paste0("NTM_data/countries/clean_WTO_TBT_", country_iso, ".csv"), row.names = FALSE)
}



################################################################################




#to vizualize previous results 

# to visualize it 
hs_combined_coverage <- panel_dta_SPS_sub %>%
  filter(SPS1>0) %>%
  group_by(year) %>%
  summarise(num_hs_combined_with_sps1 = n_distinct(HS4code))
# Create ggplot
plot_hs_combined <-ggplot(hs_combined_coverage, aes(x = year, y = num_hs_combined_with_sps1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HS4code with SPS") +
  ggtitle("WTO: Number of HS6 with SPS per Year")
ggsave("~/TRADE/WTO/result/WTO_BRA_HScoverage.png", plot = plot_hs_combined, width = 10, height = 6, dpi = 300)

# to visualize it 
hs_combined_coverage <- panel_dta_TBT_sub %>%
  filter(TBT1 >0) %>%
  group_by(year) %>%
  summarise(num_hs_combined_with_tbt1 = n_distinct(HS4code))
# Create ggplot
plot_hs_combined2 <-ggplot(hs_combined_coverage, aes(x = year, y = num_hs_combined_with_tbt1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HS4code with TBT") +
  ggtitle("WTO: Number of HS6 with TBT per Year")
ggsave("~/TRADE/WTO/result/WTO_BRA_HScoverage_TBT.png", plot = plot_hs_combined2, width = 10, height = 6, dpi = 300)


################################################################################


