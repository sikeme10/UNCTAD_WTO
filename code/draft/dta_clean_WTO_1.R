

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



# export data
write.csv(merged_data, "clean_WTO.csv", row.names = FALSE)


################################################################################

# 0) Change hS revisions and all 

# first put HS codes as character var 
merged_data$HS1996 <- as.character(merged_data$HS1996)
merged_data$HS1996 <- ifelse(nchar(merged_data$HS1996) == 5 & !is.na(merged_data$HS1996), paste0("0", merged_data$HS1996), merged_data$HS1996)
#check <-  all(nchar(merged_data$HS1996) == 6 | is.na(merged_data$HS1996))
#



test <- merged_data %>% filter(is.na(HS1996))
length(unique(test$HScombined))

# have concordance on other file 
# vec <- unique(merged_data$HS1996)
# vec <- data.frame(vec)
# write.csv(vec, "~/TRADE/WTO/HS1996.csv", row.names = FALSE)

# get revision data with HS and each HS2, Hs3, HS4 codes
HS_revisions <- read_csv("HS_revisions.csv")
names(HS_revisions)
HS_revisions <- HS_revisions %>% rename(HS1996 =vec)

merged_data <- left_join(merged_data,HS_revisions)

merged_data <- merged_data %>% 
  mutate(HS_revised = case_when(
    year < 2002 ~ HS1996,
    year < 2007 & year > 2001 ~ HS2_concord,
    year < 2012 & year > 2006 ~ HS3_concord,
    year < 2017 & year > 2011 ~ HS4_concord,
    year > 2017 ~ HS5_concord
  ))

merged_data$HS_revised <- ifelse(is.na(merged_data$HS_revised),merged_data$HScombined,merged_data$HS_revised  )
colSums(is.na(merged_data))
#create a new HS code varaibales that takes HS revised (from HS1996) or the HScombined


test<- subset(merged_data, year>2011 & year<2017)
length(unique(test$HS_revised)) #looks decent 

################################################################################
# or trying matching 

#how many HS combined by HS 1996
hs_values <- merged_data %>%
  group_by(HS1996) %>%
  summarise(unique_HScombined = paste(unique(HScombined), collapse = ", "))
#look at number of unmatched
last_row <- tail(hs_values, n = 1)
print(last_row)

#check if one HScombined  for each HS1996
hs_values1 <- merged_data %>%
  group_by(HScombined) %>%
  summarise(unique_HS1996 = paste(unique(HS1996), collapse = ", "))
subset_hs <- hs_values1[grepl(",", hs_values1$unique_HS2), ] #we get 76 HS codes (at the 6 level changes a bit but most of the time at HS5 its the same)


# separate values after coma 
split_values <- strsplit(as.character(hs_values1$unique_HS1996), ",")

# Determine the maximum number of new variables needed
max_vars <- max(sapply(split_values, length)) - 1
# Create new variables in the dataframe
for (j in 1:max_vars) {
  var_name <- paste0("new_var_", j)
  hs_values1[[var_name]] <- NA_character_}
# Iterate over the split values and assign values to new variables
for (i in seq_along(split_values)) {
  for (j in seq_len(length(split_values[[i]]) - 1)) {
    var_name <- paste0("new_var_", j)
    hs_values1[[var_name]][i] <- split_values[[i]][j + 1]
  }}
# Drop values after the last comma in each element
hs_values1$unique_HS1996 <- gsub(",[^,]*$", "", hs_values1$unique_HS1996)



# Extract values after comma and create a new variable
df$new_var <- sapply(split_values, function(x) if (length(x) > 1) tail(x, -1) else NA)




# try to reconvert some into HS 1


#to find eauivalents ni deifferent revisions:
library(concordance)
HS1996 <- as.character(unique(merged_data$HS1996))
HS1996 <- HS1996[!is.na(HS1996)]
add_zero <- function(string) { if (nchar(string) == 5) {
    string <- paste0("0", string)}
  return(string)
}
HS1996 <- unlist(lapply(HS1996, add_zero))
# to check if all has 6 characters:   has_six_characters <- all(sapply(HS1996, function(x) nchar(x) == 6))

HS1996 <- HS1996[1:2]

equivalent_hs2 <- function(HS1996) {
  hs2_code <- concord_hs(sourcevar = HS1996, origin = "HS1", destination = "HS2", dest.digit = 6, all = FALSE)
  return(hs2_code)
}
hs2_equivalents <- sapply(HS1996, equivalent_hs2)











################################################################################

#1) Separate by type of NTM

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
names(dta_SPS)
dta_TBT <-process_data(merged_data, "TBT")
names(dta_TBT)

colSums(is.na(dta_SPS))
colSums(is.na(dta_TBT))

################################################################################
# 2) If want to drop duplicate due to multiple ID or AFF 


# the ID is counting the number of NTMs 
dta_SPS <- dta_SPS[!duplicated(dta_SPS[, c("HScombined", "HS1996","IMP","year","SPS","AFF")]), ]
dta_TBT <- dta_TBT[!duplicated(dta_TBT[, c("HScombined", "HS1996","TBT","year","IMP","AFF")]), ]



# we drop duplicate so that we get only a binary (sometimes NTMs present multiple times because of different ID)
#sometimes different ID for same NTRM (just possibly rectification on the first NTM) 

#if drop AFF 
#table(dta_SPS$AFF)
#dta_SPS <- dta_SPS[!duplicated(dta_SPS[, c("HScombined", "HS1996","IMP","year","SPS")]), ]
#dta_TBT <- dta_TBT[!duplicated(dta_TBT[, c("HScombined", "HS1996","IMP","year","TBT")]), ]
#

################################################################################
# we are only going to do binary var for NTMs and not counts for the moment 


################################################################################

# 2) re balance the panel (without all year, IMP, and HS combinations )

dta_SPS_1 <-subset(dta_SPS, AFF=="All Members")
dta_SPS_2 <-subset(dta_SPS, AFF !="All Members")


dta_TBT_1 <-subset(dta_TBT, AFF=="All Members")
dta_TBT_2 <-subset(dta_TBT, AFF !="All Members")


# use expand grid to get a full panel (without AFF)
full_panel_WLD <- function(data) {
  year <- unique(data$year)
  HScombined <- unique(data$HScombined)
  IMP <- unique(data$IMP)
  all_combinations <- expand.grid(year=year, HScombined=HScombined,IMP=IMP)
  panel_dta <- left_join(all_combinations,data)
}



#apply the function to the two data set
panel_dta_SPS <-full_panel_WLD(dta_SPS_1)
colSums(is.na(panel_dta_SPS))

panel_dta_TBT <-full_panel_WLD(dta_TBT_1)
colSums(is.na(panel_dta_TBT))
names(panel_dta_SPS)



# use expand grid to get a full panel (without AFF)
# full_panel_AFF <- function(data) {
#   year <- unique(data$year)
#   HScombined <- unique(data$HScombined)
#   IMP <- unique(data$IMP)
#   AFF <- unique(data$AFF)
#   all_combinations <- expand.grid(year=year, HScombined=HScombined,IMP=IMP, AFF= AFF)
#   panel_dta <- left_join(all_combinations,data)
# }
# 
# panel_dta_SPS_2 <-full_panel_AFF(dta_SPS_2)
# 


write.csv(panel_dta_TBT, "WTO_panel_dta_TBT.csv", row.names = FALSE)

write.csv(panel_dta_SPS, "WTO_panel_dta_SPS.csv", row.names = FALSE)


################################################################################

# 3) select country and do this code: 
# for faster processing 
#take only one country
IMP <- unique(panel_dta_SPS$IMP)

panel_dta_SPS_sub <- subset(panel_dta_SPS, IMP %in% c("Brazil"))
colSums(is.na(panel_dta_SPS_sub))
table(panel_dta_SPS_sub$SPS)

# as no withdrawal,  if implement an NTM need to leave it 

cum_Sum <- function(data) {
  data$SPS <- as.numeric(data$SPS)
  data$SPS <- ifelse(is.na(data$SPS), 0, data$SPS)
  data <- data %>%
    arrange(year, IMP, HScombined) %>%
    group_by(IMP, HScombined) %>%      # Grouping by product
    mutate(SPS1 = cummax(replace(SPS, is.na(SPS), 0)))
}

panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS_sub)
colSums(is.na(panel_dta_SPS_sub))


#function alone
panel_dta_SPS_sub$SPS <- ifelse(is.na(panel_dta_SPS_sub$SPS), 0, panel_dta_SPS_sub$SPS)
panel_dta_SPS_sub <- panel_dta_SPS_sub %>%
  arrange(year, IMP, HScombined) %>%
  group_by(IMP, HScombined) %>%      # Grouping by product
  mutate(SPS1 = cummax(replace(SPS, is.na(SPS), 0)))


colSums(is.na(panel_dta_SPS_sub))
table(panel_dta_SPS_sub$SPS1)
test <- panel_dta_SPS_sub %>%  filter(SPS1 == 1)
test <-test %>% arrange(HScombined, year)
test <- test %>%select(year, HScombined, SPS, SPS1, everything())
table(panel_dta_SPS_sub$SPS1,panel_dta_SPS_sub$SPS)


################################################################################
# or apply this function: 
unique(panel_dta_SPS$IMP)

# for SPS
SPS_country <- function(country, data) {
  # Subset the data for the specified country
  panel_dta_SPS_sub <- subset(data, IMP %in% country)
  # Ensure SPS is numeric
  panel_dta_SPS_sub$SPS <- as.numeric(panel_dta_SPS_sub$SPS)
  cum_Sum <- function(data) {
    data$SPS <- as.numeric(data$SPS)
    data$SPS <- ifelse(is.na(data$SPS), 0, data$SPS)
    data <- data %>%
      arrange(year, IMP, HScombined) %>%
      group_by(IMP, HScombined) %>%      # Grouping by product
      mutate(SPS1 = cummax(replace(SPS, is.na(SPS), 0)))
  }
  panel_dta_SPS_sub <-cum_Sum(panel_dta_SPS_sub)
  # Return both the modified data frame and the plot
  return(panel_dta_SPS_sub)
}

panel_dta_SPS_sub <- SPS_country("Brazil", panel_dta_SPS)

table(panel_dta_SPS_sub$SPS, panel_dta_SPS_sub$SPS1)



# for TBT
unique(panel_dta_TBT$IMP)

TBT_country <- function(country, data) {
  # Subset the data for the specified country
  panel_dta_TBT_sub <- subset(data, IMP %in% country)
  # Ensure SPS is numeric
  panel_dta_TBT_sub$TBT <- as.numeric(panel_dta_TBT_sub$TBT)
  cum_Sum <- function(data) {
    data$TBT <- as.numeric(data$TBT)
    data$TBT <- ifelse(is.na(data$TBT), 0, data$TBT)
    data <- data %>%
      arrange(year, IMP, HScombined) %>%
      group_by(IMP, HScombined) %>%      # Grouping by product
      mutate(TBT1 = cummax(replace(TBT, is.na(TBT), 0)))
  }
  panel_dta_TBT_sub <-cum_Sum(panel_dta_TBT_sub)
  # Return both the modified data frame and the plot
  return(panel_dta_TBT_sub)
}

panel_dta_TBT_sub <- TBT_country("Brazil", panel_dta_TBT)

table(panel_dta_TBT_sub$TBT, panel_dta_TBT_sub$TBT1)






################################################################################

# to visualize it 
hs_combined_coverage <- panel_dta_SPS_sub %>%
  filter(SPS1 == 1) %>%
  group_by(year) %>%
  summarise(num_hs_combined_with_sps1 = n_distinct(HScombined))
# Create ggplot
plot_hs_combined <-ggplot(hs_combined_coverage, aes(x = year, y = num_hs_combined_with_sps1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HScombined with SPS") +
  ggtitle("WTO: Number of HS6 with SPS per Year")
ggsave("~/TRADE/WTO/result/WTO_BRA_HScoverage.png", plot = plot_hs_combined, width = 10, height = 6, dpi = 300)

# to visualize it 
hs_combined_coverage <- panel_dta_TBT_sub %>%
  filter(TBT1 == 1) %>%
  group_by(year) %>%
  summarise(num_hs_combined_with_tbt1 = n_distinct(HScombined))
# Create ggplot
plot_hs_combined2 <-ggplot(hs_combined_coverage, aes(x = year, y = num_hs_combined_with_tbt1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HScombined with TBT") +
  ggtitle("WTO: Number of HS6 with TBT per Year")
ggsave("~/TRADE/WTO/result/WTO_BRA_HScoverage_TBT.png", plot = plot_hs_combined2, width = 10, height = 6, dpi = 300)

length(unique(panel_dta_TBT_sub$HScombined))
length(unique(panel_dta_SPS_sub$HScombined))

subset_SPS <- panel_dta_TBT_sub[panel_dta_TBT_sub$SPS1 == 1, ]
length(unique(subset_data$HScombined))



panel_dta_SPS <-  MoveFront(panel_dta_SPS, c("IMP", "HScombined", "year", "SPS", "SPS1"))




#  #convert in isocode
#  library(countrycode)
#  panel_dta_SPS$ReporterISO3 <- countrycode(panel_dta_SPS$IMP, "country.name", "iso3c")
#  panel_dta_SPS$ReporterISO3[panel_dta_SPS$IMP == "European Union"] <- "EUN"
#  colSums(panel_dta_SPS)
#  panel_dta_SPS<- panel_dta_SPS %>% rename (ProductCode = HS_revised )
#  panel_dta_SPS$PartnerISO3<- "WLD"
#  
#  
#  isocode <- function(data, WLD_vector) {
#    data$ReporterISO3 <- countrycode(data$IMP, "country.name", "iso3c")
#    data$ReporterISO3[data$IMP == "European Union"] <- "EUN"
#    data <- data %>% rename(ProductCode = HS_revised)
#    data$PartnerISO3 <- "WLD"
#    return(data)
#  }
#  
#  panel_dta_SPS <- isocode(panel_dta_SPS)
#  panel_dta_TBT <- isocode(panel_dta_SPS)

################################################################################


# Export Clean data 

write.csv(merged_data_TBT, "clean_WTO_TBT.csv", row.names = FALSE)

write.csv(merged_data_SPS, "clean_WTO_SPS.csv", row.names = FALSE)









################################################################################

dta <- panel_dta_TBT_sub
dta <- panel_dta_SPS_sub


# first put HS codes as character var 
dta$HS1996 <- as.character(dta$HS1996)
dta$HS1996 <- ifelse(nchar(dta$HS1996) == 5 & !is.na(dta$HS1996), paste0("0", dta$HS1996), dta$HS1996)
#check <-  all(nchar(dta$HS1996) == 6 | is.na(dta$HS1996))
#



test <- dta %>% filter(is.na(HS1996))
length(unique(test$HScombined))

# have concordance on other file 
# vec <- unique(dta$HS1996)
# vec <- data.frame(vec)
# write.csv(vec, "~/TRADE/WTO/HS1996.csv", row.names = FALSE)

# get revision data with HS and each HS2, Hs3, HS4 codes
HS_revisions <- read_csv("HS_revisions.csv")
names(HS_revisions)
HS_revisions <- HS_revisions %>% rename(HS1996 =vec)

dta <- left_join(dta,HS_revisions)

dta <- dta %>% 
  mutate(HS_revised = case_when(
    year < 2002 ~ HS1996,
    year < 2007 & year > 2001 ~ HS2_concord,
    year < 2012 & year > 2006 ~ HS3_concord,
    year < 2017 & year > 2011 ~ HS4_concord,
    year > 2017 ~ HS5_concord
  ))

dta$HS_revised <- ifelse(is.na(dta$HS_revised),dta$HScombined,dta$HS_revised  )
colSums(is.na(dta))
#create a new HS code varaibales that takes HS revised (from HS1996) or the HScombined


test<- subset(dta, year>2011 & year<2017)
length(unique(test$HS_revised)) #looks decent 


names(dta)
distinct_count <- dta %>% filter(TBT1 == 1) 
length(unique(distinct_count$HS_revised))

distinct_count <- dta %>% filter(SPS1 == 1) 
length(unique(distinct_count$HS_revised))





isocode <- function(data, WLD_vector) {
  data$ReporterISO3 <- countrycode(data$IMP, "country.name", "iso3c")
  data$ReporterISO3[data$IMP == "European Union"] <- "EUN"
  data <- data %>% rename(ProductCode = HS_revised,
                          Year = year)
  data$PartnerISO3 <- "WLD"
  return(data)
}

dta <- isocode(dta)



write.csv(dta, "BRA/BRA_WTO_TBT.csv", row.names = FALSE)
write.csv(dta, "BRA/BRA_WTO_SPS.csv", row.names = FALSE)







##############################################################################




















































##############################################################################
#if want to try on multiple country




unique_IMPs <- unique(panel_dta_SPS$IMP)
unique_IMPs <- unique_IMPs[1:3]
# Initialize an empty list to store the resulting dataframes
result_list <- list()

# Loop over each unique value of IMP
for (imp in unique_IMPs) {
  # Subset data for the current IMP
  panel_dta_SPS_sub <- subset(panel_dta_SPS, IMP == imp)
  
  # Perform operations on the subset
  # Example operations:
  col_sums <- colSums(is.na(panel_dta_SPS_sub))
  sps_table <- table(panel_dta_SPS_sub$SPS)
  year_sps_table <- table(panel_dta_SPS_sub$year, panel_dta_SPS_sub$SPS)
  
  # Apply the cum_Sum function
  panel_dta_SPS_sub <- cum_Sum(panel_dta_SPS_sub)
  
  # Store the result in the list
  final_result <- rbind(final_result, panel_dta_SPS_sub)
}

# Bind all obtained panel_dta_SPS_sub for each IMP together
final_result <- do.call(rbind, result_list)

table(final_result$IMP)
sub <- subset(final_result, IMP ="Afghanistan")
table(sub$SPS1,sub$SPS)

# to vizualize it 
hs_combined_coverage <- sub %>%
  filter(SPS1 == 1) %>%
  group_by(year) %>%
  summarise(num_hs_combined_with_sps1 = n_distinct(HScombined))
# Create ggplot
ggplot(hs_combined_coverage, aes(x = year, y = num_hs_combined_with_sps1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HScombined with SPS1") +
  ggtitle("Number of HScombined with SPS1 per Year")



