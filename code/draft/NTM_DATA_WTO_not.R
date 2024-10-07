#When downloading on my own

dta <- read_excel("Trade/WTO notification/ITIP/WTO_notification_1995_2019.xlsx")
names(dta)
table(dta$'Latest notified date')

# Convert the date column to Date objects and extract the year
dta <- dta %>%
  mutate(initiation_year = format(as.Date(Initiation), "%Y"))
table(dta$initiation_year)


dta <- dta %>%
  mutate(in_force_year = format(as.Date(`In force`), "%Y"))
table(dta$in_force_year)



# missing HS code 
sum(is.na(dta$HS))
length(unique(dta$`Member imposing`))


# number of country 
length(unique(dta$`Member imposing`))
#number of distinct country observed per year 
distinct_country_counts <- dta %>%
  group_by(initiation_year) %>%
  summarise(Count = n_distinct(`Member imposing`))
print(distinct_country_counts, n = Inf)



# data availability of TRAINS 

UNCTAD_avai <- read_csv("Trade/WTO notification/DataJobID-2535790_2535790_dataavailability.csv")
names(UNCTAD_avai)
sub_UNCTAD_avai <- subset(UNCTAD_avai, Count > 6)
print(sub_UNCTAD_avai)
###############################################################################




###############################################################################

#using Ghodsi's website, where missing HS codes have been found
# https://wiiw.ac.at/wiiw-ntm-data-ds-2.html
# can get info on: https://wiiw.ac.at/files/data/set/ds-0002-01-readme_1995_2019.txt

load("~/Trade/WTO notification/ds-0002-04-ntm_hs_and_notifications_1995_2019.rdata")


names(NTM_HS)
names(NTM_Notifications_1995_2019)


# 1) need to merge the two data sets to obtain HS codes and NTM data together:
# matching with ID code 


library(tidyr)
library(dplyr)


merged_data <- merge(NTM_HS, NTM_Notifications_1995_2019, by = "ID", all.x = TRUE, all.y = TRUE)

merged_data <- merged_data %>%
  separate_rows(HScode, sep = ",")

names(merged_data)
class(merged_data$HS1996)

# I have some code with HS 5 numbers 
sub_merged_data <- subset(merged_data1, nchar(as.character(HS1996)) == 5)
# need to convert HS codes in character 
library(stringr)
merged_data$HS1996 <- as.character(merged_data$HS1996)


###############################################################################
# select only SPS 
table(merged_data$NTM)



#select SPS type of NTM
merged_data_SPS <- subset(merged_data, NTM == "SPS")
names(merged_data_SPS)

#how many NA for withdrawel year
colSums(is.na(merged_data_SPS))


table(merged_data_SPS$IMP)







#number of distinct country observed per year 
distinct_country_counts <- merged_data_SPS %>%
  group_by(Year_Initiation) %>%
  summarise(Count = n_distinct(IMP))
print(distinct_country_counts, n = Inf)
#bumber of observation per countries
distinct_country_year_counts <- merged_data_SPS %>%
  group_by(IMP) %>%
  summarise(Count = n_distinct(Year_Initiation))
print(distinct_country_year_counts, n = Inf)
#what countries for each year 
unique_countries <- distinct(merged_data_SPS, Year_Initiation, IMP)







