

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
# 7) select country of interest and look at cumulative NTMs counts 
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
  merged_data <- merged_data[!is.na(merged_data$HScombined), ]
  return(merged_data)
}

merged_data <- merging(NTM_HS, NTM_Notifications_1995_2019)
names(merged_data)



# export data
write.csv(merged_data, "NTM_data/clean_WTO_1.csv", row.names = FALSE)
merged_data <- read_csv("NTM_data/clean_WTO_1.csv")

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

# 4) solve HS code issues:

################################################################################
# Some HS codes are not always retrieved so use HS1996 codes to get HS4 revision codes back

# a) Load data with HS revision info from WITS 
H4_to_H5 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-92_Concordance_H5_to_H4.CSV")
H4_to_H5 <- H4_to_H5 %>% rename(HS4code = 'HS 2012 Product Code', HS5code ='HS 2017 Product Code')
H4_to_H5 <- select(H4_to_H5 ,HS5code,HS4code )
H5 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-110_Concordance_HS_to_H5.CSV")
H5 <- H5 %>% rename(HScomb = 'HS - Combined  Product Code', HS5code ='HS 2017 Product Code')
H5 <- select( H5 ,HScomb,HS5code )
#for HS1 revision and HS1
H4_to_H1 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-78_Concordance_H4_to_H1.CSV")
H4_to_H1 <- H4_to_H1 %>% rename(HS1code = 'HS 1996 Product Code', HS4code ='HS 2012 Product Code')
H4_to_H1 <- select( H4_to_H1 ,HS1code,HS4code )
H4 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-109_Concordance_HS_to_H4.CSV")
H4 <- H4 %>% rename(HScomb = 'HS - Combined  Product Code', HS4code ='HS 2012 Product Code')
H4 <- select( H4 ,HScomb,HS4code )
#for HS3 revision
H4_to_H3 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-80_Concordance_H4_to_H3.CSV")
H4_to_H3 <- H4_to_H3 %>% rename(HS4code = 'HS 2012 Product Code', HS3code ='HS 2007 Product Code')
H4_to_H3 <- select(H4_to_H3 ,HS3code,HS4code )
H3 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-62_Concordance_HS_to_H3.CSV")
H3 <- H3 %>% rename(HScomb = 'HS - Combined  Product Code', HS3code ='HS 2007 Product Code')
H3 <- select( H3 ,HScomb,HS3code )
#for HS2 revision
H4_to_H2 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-79_Concordance_H4_to_H2.CSV")
H4_to_H2 <- H4_to_H2 %>% rename(HS4code = 'HS 2012 Product Code', HS2code ='HS 2002 Product Code')
H4_to_H2 <- select(H4_to_H2 ,HS2code,HS4code )
H2 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-61_Concordance_HS_to_H2.CSV")
H2 <- H2 %>% rename(HScomb = 'HS - Combined  Product Code', HS2code ='HS 2002 Product Code')
H2 <- select( H2 ,HScomb,HS2code)
#for HS1 revision
H1 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-60_Concordance_HS_to_H1.CSV")
H1 <- H1 %>% rename(HScomb = 'HS - Combined  Product Code', HS1code ='HS 1996 Product Code')
H1 <- select( H1 ,HScomb,HS1code)


################################################################################

# b) SPS: HS concordance 

#the objectif is to concord all HS codes at the HS4 revisions (to easily match it with UNCTAD data)


# 1)  first put HS codes as character var in the WTo data 
dta_SPS$HScombined <- ifelse(nchar(dta_SPS$HScombined) == 5 & !is.na(dta_SPS$HScombined), paste0("0", dta_SPS$HScombined), dta_SPS$HScombined)
dta_SPS$HS1996 <- as.character(dta_SPS$HS1996)
dta_SPS$HS1996 <- ifelse(nchar(dta_SPS$HS1996) == 5 & !is.na(dta_SPS$HS1996), paste0("0", dta_SPS$HS1996), dta_SPS$HS1996)
# for TBT 
dta_TBT$HScombined <- as.character(dta_TBT$HScombined)
dta_TBT$HScombined <- ifelse(nchar(dta_TBT$HScombined) == 5 & !is.na(dta_TBT$HScombined), paste0("0", dta_TBT$HScombined), dta_TBT$HScombined)
dta_TBT$HS1996 <- as.character(dta_TBT$HS1996)
dta_TBT$HS1996 <- ifelse(nchar(dta_TBT$HS1996) == 5 & !is.na(dta_TBT$HS1996), paste0("0", dta_TBT$HS1996), dta_TBT$HS1996)



# 2) separate data between each HS revision
dta_SPS_sub_H5 <- subset(dta_SPS, year>2016)# H5: from 2017 to 2022
dta_SPS_sub_H4 <- subset(dta_SPS, year>2011 & year <2017) # H4: from 2012 to 2017 
dta_SPS_sub_H3 <- subset(dta_SPS, year>2006 & year <2012) # H3: from 2007 to 2002 
dta_SPS_sub_H2 <- subset(dta_SPS, year>2001 & year <2007) # H2: from 2002 to 2007 
dta_SPS_sub_H1 <- subset(dta_SPS, year>1995 & year <2002) # H1: from 1996 to 2022 
dta_SPS_sub_H0 <- subset(dta_SPS, year==1995 ) # H0: for 1995
# to check  : nrow(dta_SPS_sub_H4) +nrow(dta_SPS_sub_H3)+nrow(dta_SPS_sub_H2)+nrow(dta_SPS_sub_H1)+nrow(dta_SPS_sub_H0)


# 3) separate data between each HS revision
#for HS5 revisions
matching_HS5 <- dta_SPS_sub_H5[dta_SPS_sub_H5$HScombined %in% H5$HScomb, ]
matching_HS5 <- left_join(matching_HS5, H4_to_H5, by = c("HScombined" = "HS5code"))
#for not matching ones, use HS1996
not_matching_HS5 <- dta_SPS_sub_H5[!dta_SPS_sub_H5$HScombined %in% H5$HScomb, ] # if not matching use HS1996
not_matching_HS5 <- left_join(not_matching_HS5, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_SPS_sub_H5_clean <- rbind(matching_HS5, not_matching_HS5)


# for with H4 
# unmatching values (separate between matching and not matching)
matching_HS4 <- dta_SPS_sub_H4[dta_SPS_sub_H4$HScombined %in% H4$HScomb, ]
matching_HS4$HS4code <- matching_HS4$HScombined  #the matching ones are rename in HS4code
not_matching_HS4 <- dta_SPS_sub_H4[!dta_SPS_sub_H4$HScombined %in% H4$HScomb, ]
#among not matching retrieve HS4
not_matching_HS4_1 <- left_join(not_matching_HS4, H4_to_H1, by = c("HS1996" = "HS1code"))
#join the matching and not matching data back:
#getting the new data with HS4 revision 
dta_SPS_sub_H4_clean <- rbind(matching_HS4, not_matching_HS4_1)

#for H3 revision: 
matching_HS3 <- dta_SPS_sub_H3[dta_SPS_sub_H3$HScombined %in% H3$HScomb, ]
matching_HS3 <- left_join(matching_HS3, H4_to_H3, by = c("HScombined" = "HS3code"))
#for not matching ones, use HS1996
not_matching_HS3 <- dta_SPS_sub_H3[!dta_SPS_sub_H3$HScombined %in% H3$HScomb, ] # if not matching use HS1996
not_matching_HS3 <- left_join(not_matching_HS3, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_SPS_sub_H3_clean <- rbind(matching_HS3, not_matching_HS3)


#for H2 revision: 
matching_HS2 <- dta_SPS_sub_H2[dta_SPS_sub_H2$HScombined %in% H2$HScomb, ]
matching_HS2 <- left_join(matching_HS2, H4_to_H2, by = c("HScombined" = "HS2code"))
# # or not matching ones, use HS1996
not_matching_HS2 <- dta_SPS_sub_H2[!dta_SPS_sub_H2$HScombined %in% H2$HScomb, ] # if not matching use HS1996
not_matching_HS2 <- left_join(not_matching_HS2, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_SPS_sub_H2_clean <- rbind(matching_HS2, not_matching_HS2)

#for H1 revision: 
matching_HS1 <- dta_SPS_sub_H1[dta_SPS_sub_H1$HScombined %in% H1$HScomb, ]
matching_HS1 <- left_join(matching_HS1, H4_to_H1, by = c("HScombined" = "HS1code"))
# or not matching ones, use HS1996
not_matching_HS1 <- dta_SPS_sub_H1[!dta_SPS_sub_H1$HScombined %in% H1$HScomb, ] # if not matching use HS1996
not_matching_HS1 <- left_join(not_matching_HS1, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_SPS_sub_H1_clean <- rbind(matching_HS1, not_matching_HS1)


#for H0 revision: 
dta_SPS_sub_H0_clean <- left_join(dta_SPS_sub_H0, H4_to_H1, by = c("HS1996" = "HS1code"))



# Combine all the data together: 
dta_SPS_clean <- rbind(dta_SPS_sub_H0_clean, dta_SPS_sub_H1_clean, dta_SPS_sub_H2_clean,
                       dta_SPS_sub_H3_clean, dta_SPS_sub_H4_clean,dta_SPS_sub_H5_clean)


# Need o use HScode4 from now on 
# can drop all missing HS code revision 4
dta_SPS_clean <- dta_SPS_clean[complete.cases(dta_SPS_clean$HS4code), ]
table(dta_SPS_clean$year)
# to check:
length(unique(dta_SPS_clean$HS4code))

################################################################################

# c) TBT: HS concordance 

#the objectif is to concord all HS codes at the HS4 revisions (to easily match it with UNCTAD data)



# 1) separate data between each HS revision

table(dta_TBT$year)
dta_TBT_sub_H5 <- subset(dta_TBT, year>2016)
dta_TBT_sub_H4 <- subset(dta_TBT, year>2011 & year <2017) # H4: from 2012 to 2017 
dta_TBT_sub_H3 <- subset(dta_TBT, year>2006 & year <2012) # H3: from 2007 to 2002 
dta_TBT_sub_H2 <- subset(dta_TBT, year>2001 & year <2007) # H2: from 2002 to 2007 
dta_TBT_sub_H1 <- subset(dta_TBT, year>1995 & year <2002) # H1: from 1996 to 2022 
dta_TBT_sub_H0 <- subset(dta_TBT, year==1995 ) # H0: for 1995
# to check  : nrow(dta_TBT_sub_H4) +nrow(dta_TBT_sub_H3)+nrow(dta_TBT_sub_H2)+nrow(dta_TBT_sub_H1)+nrow(dta_TBT_sub_H0)


# 2) match for each revision and find concordance in H4 revision

# for H5
matching_HS5 <- dta_TBT_sub_H5[dta_TBT_sub_H5$HScombined %in% H5$HScomb, ]
matching_HS5 <- left_join(matching_HS5, H4_to_H5, by = c("HScombined" = "HS5code"))
#for not matching ones, use HS1996
not_matching_HS5 <- dta_TBT_sub_H5[!dta_TBT_sub_H5$HScombined %in% H5$HScomb, ] # if not matching use HS1996
not_matching_HS5 <- left_join(not_matching_HS5, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_TBT_sub_H5_clean <- rbind(matching_HS5, not_matching_HS5)


# for H4 
# unmatching values (separate between matching and not matching)
matching_HS4 <- dta_TBT_sub_H4[dta_TBT_sub_H4$HScombined %in% H4$HScomb, ]
matching_HS4$HS4code <- matching_HS4$HScombined  #the matching ones are rename in HS4code
not_matching_HS4 <- dta_TBT_sub_H4[!dta_TBT_sub_H4$HScombined %in% H4$HScomb, ]
#among not matching retrieve HS4
not_matching_HS4_1 <- left_join(not_matching_HS4, H4_to_H1, by = c("HS1996" = "HS1code"))
#join the matching and not matching data back:

#getting the new data with HS4 revision 
dta_TBT_sub_H4_clean <- rbind(matching_HS4, not_matching_HS4_1)

#for H3 revision: 
matching_HS3 <- dta_TBT_sub_H3[dta_TBT_sub_H3$HScombined %in% H3$HScomb, ]
matching_HS3 <- left_join(matching_HS3, H4_to_H3, by = c("HScombined" = "HS3code"))
# or use concord function 
# matching_HS3_concord <- matching_HS3 %>%
#   mutate(HS4code_sub = concord_hs(HScombined, origin = "HS3", destination = "HS4", dest.digit = 6))
# colSums(is.na(matching_HS3_concord))
colSums(is.na(matching_HS3))
#for not matching ones, use HS1996
not_matching_HS3 <- dta_TBT_sub_H3[!dta_TBT_sub_H3$HScombined %in% H3$HScomb, ] # if not matching use HS1996
colSums(is.na(not_matching_HS3))
not_matching_HS3 <- left_join(not_matching_HS3, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_TBT_sub_H3_clean <- rbind(matching_HS3, not_matching_HS3)
colSums(is.na(dta_TBT_sub_H3_clean))


#for H2 revision: 
matching_HS2 <- dta_TBT_sub_H2[dta_TBT_sub_H2$HScombined %in% H2$HScomb, ]
matching_HS2 <- left_join(matching_HS2, H4_to_H2, by = c("HScombined" = "HS2code"))
# # or not matching ones, use HS1996
not_matching_HS2 <- dta_TBT_sub_H2[!dta_TBT_sub_H2$HScombined %in% H2$HScomb, ] # if not matching use HS1996
not_matching_HS2 <- left_join(not_matching_HS2, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_TBT_sub_H2_clean <- rbind(matching_HS2, not_matching_HS2)

#for H1 revision: 
matching_HS1 <- dta_TBT_sub_H1[dta_TBT_sub_H1$HScombined %in% H1$HScomb, ]
matching_HS1 <- left_join(matching_HS1, H4_to_H1, by = c("HScombined" = "HS1code"))
# or not matching ones, use HS1996
not_matching_HS1 <- dta_TBT_sub_H1[!dta_TBT_sub_H1$HScombined %in% H1$HScomb, ] # if not matching use HS1996
not_matching_HS1 <- left_join(not_matching_HS1, H4_to_H1, by = c("HS1996" = "HS1code"))
dta_TBT_sub_H1_clean <- rbind(matching_HS1, not_matching_HS1)


#for H0 revision: 
dta_TBT_sub_H0_clean <- left_join(dta_TBT_sub_H0, H4_to_H1, by = c("HS1996" = "HS1code"))



# Combine all the data together: 
dta_TBT_clean <- rbind(dta_TBT_sub_H0_clean, dta_TBT_sub_H1_clean, dta_TBT_sub_H2_clean,
                       dta_TBT_sub_H3_clean, dta_TBT_sub_H4_clean, dta_TBT_sub_H5_clean)


# Need o use HScode4 from now on 
# can drop all missing HS code revision 4
dta_TBT_clean <- dta_TBT_clean[complete.cases(dta_TBT_clean$HS4code), ]
# to check:
length(unique(dta_TBT_clean$HS4code))




################################################################################

#5) each  distinct ID represents different count of NTM

################################################################################

# for SPS case:
#we want a count of NTM instead of binary 
names(dta_SPS_clean)
dta_SPS_clean$SPS <- as.numeric(dta_SPS_clean$SPS)
summary(dta_SPS_clean$SPS)
# add all NTM counts 
dta_SPS_clean <- dta_SPS_clean %>% group_by(year,IMP, AFF, HS4code) %>% 
  summarise(SPS_count = sum(SPS))
summary(dta_SPS_clean$SPS_count)
length(unique(dta_SPS_clean$HS4code)) # should get 5205

# for TBT case:
#we want a count of NTM instead of binary 
names(dta_TBT_clean)
dta_TBT_clean$TBT <- as.numeric(dta_TBT_clean$TBT)
summary(dta_TBT_clean$TBT)
# add all NTM counts 
dta_TBT_clean <- dta_TBT_clean %>% group_by(year,IMP, AFF, HS4code) %>% 
  summarise(TBT_count = sum(TBT))
summary(dta_TBT_clean$TBT_count)
length(unique(dta_TBT_clean$HS4code))



################################################################################

# 6) re balance the panel (without all year, IMP, and HS combinations )

################################################################################

dta_SPS_clean_1 <-subset(dta_SPS_clean, AFF=="All Members")
dta_SPS_clean_2 <-subset(dta_SPS_clean, AFF !="All Members")


dta_TBT_clean_1 <-subset(dta_TBT_clean, AFF=="All Members")
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




################################################################################

# 7) select country and look at cumulative NTMs

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
# check
table(panel_dta_SPS_sub$SPS_count)
table(panel_dta_SPS_sub$SPS1)
table(panel_dta_SPS_sub$IMP)

panel_dta_TBT_sub <- TBT_country("Brazil", panel_dta_TBT)
# check
table(panel_dta_TBT_sub$TBT_count)
table(panel_dta_TBT_sub$TBT1)
table(panel_dta_TBT_sub$IMP)




################################################################################

# 8) Harmonize name of variables 

################################################################################

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
# "BRA", "CHL","BOL", "COL","ECU", "PER"

write.csv(panel_dta_TBT_sub, "NTM_data/countries/clean_WTO_TBT_BOL.csv", row.names = FALSE)
write.csv(panel_dta_SPS_sub, "NTM_data/countries/clean_WTO_SPS_BOL.csv", row.names = FALSE)

# 
# for 
write.csv(panel_dta_TBT_sub, "BRA/clean_WTO_TBT_count.csv", row.names = FALSE)
write.csv(panel_dta_SPS_sub, "BRA/clean_WTO_SPS_count.csv", row.names = FALSE)




panel_dta_SPS_sub <- read_csv("NTM_data/countries/clean_WTO_SPS_BRA.csv")


names(panel_dta_SPS_sub)

table(panel_dta_SPS_sub$PartnerISO3)





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


