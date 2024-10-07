
rm(list=ls())
# Set directory
setwd("/home/sikeme/TRADE/WTO/data")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)
library(data.table)
library(stringi)


################################################################################
#load the data 
library(readr)
dta <- read_csv("NTM_hs6_2010_2022_H4V12.csv")
dta <- fread("NTM_hs6_2010_2022_H4V12.csv")

################################################################################
names(dta)
table(dta$Year)
table(dta$ntm_all)
names(dta)
table(dta$Reporter)
table(dta$NTMCode)

# get the data from 2010 to 2019 
dta <- subset(dta, Year<2020)

dta$NTM <- substr(dta$NTMCode, 1, 1)
table(dta$NTM)




################################################################################
#country names:
table(dta$Reporter)

country_counts <- dta %>%
  group_by(Year) %>%
  summarise(num_countries = n_distinct(Reporter), reporters = list(unique(Reporter)))

country_counts


summary_table <- dta %>%
  group_by(Year) %>%
  summarize(Country = paste(Reporter, collapse = ", ")) %>%
  ungroup()

# observation per year for EU:
observations_per_year <- dta %>%
  filter(Reporter == "EUN") %>%
  count(Year)



################################################################################

#create subset of data for the country 


dta_BRA <- subset(dta, Reporter == "BRA")

#for SPS specific: (class A)
dta_EUN <- subset(dta_EUN, NTM == "A")

write.csv(dta_EUN, "dta_EUN_SPS.csv", row.names = FALSE)


################################################################################
#for only SPS 
dta_SPS<- subset(dta, NTM == "A")
write.csv(dta_SPS, "dta_UNCTAD_SPS.csv", row.names = FALSE)

#for only TBT 
dta_TBT<- subset(dta, NTM == "B")
write.csv(dta_TBT, "dta_UNCTAD_TBT.csv", row.names = FALSE)



################################################################################
# 1) HS code in character

dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD_TBT.csv")

# the data is at HS 2012 revision, so need to change some of the HS codes

# make the variable in character: 
dta_UNCTAD_SPS$HSCode <- stri_pad_left(dta_UNCTAD_SPS$HSCode,6,0)
num_characters <- nchar(dta_UNCTAD_SPS$HSCode )
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)



write.csv(dta_UNCTAD_SPS, "dta_UNCTAD_SPS.csv", row.names = FALSE)

#for TBt:
dta_UNCTAD_TBT$HSCode <- stri_pad_left(dta_UNCTAD_TBT$HSCode,6,0)
num_characters <- nchar(dta_UNCTAD_TBT$HSCode )
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)

write.csv(dta_UNCTAD_TBT, "dta_UNCTAD_TBT.csv", row.names = FALSE)







###############################################################################
# 2) look at withdrawel

library(data.table)

names(dta_UNCTAD_SPS)
length(unique(country$HSCode))




 

Reporter <- unique(dta_UNCTAD_SPS$Reporter)

country <- subset(dta_UNCTAD_SPS,Reporter == "EUN")
length(unique(country$HSCode))
# Convert the data frame to a data.table
setDT(country)
# Cast the data
distinct_codes <- dcast(country, HSCode ~ Year, value.var = "Year", fun.aggregate = length)
observations_with_zero <- distinct_codes[distinct_codes$`2019` == 0, ]
nrow(observations_with_zero)



# to get number of HS code retrieve by country we can run this loop 
reporters <- unique(dta_UNCTAD_SPS$Reporter)
# Create a list to store results
results <- lapply(reporters, function(reporter) {
  country <- subset(dta_UNCTAD_SPS, Reporter == reporter)
  num_hs_codes <- length(unique(country$HSCode))
  setDT(country)
  # Cast the data
  distinct_codes <- dcast(country, HSCode ~ Year, value.var = "Year", fun.aggregate = length)
  observations_with_zero <- distinct_codes[distinct_codes$`2019` == 0, ]
  num_observations <- nrow(observations_with_zero)
  data.frame(Reporter = reporter, Num_Observations_with_Zero = num_observations, Num_HS_Codes = num_hs_codes)
})
# Combine the results into a single dataframe
final_result <- do.call(rbind, results)


write.xlsx(final_result, file = "HS_withdrawed_country.xlsx", row.names = FALSE)



# to get number of HS code retrieve by country we can run this loop 
reporters <- unique(dta_UNCTAD_TBT$Reporter)
# Create a list to store results
results <- lapply(reporters, function(reporter) {
  country <- subset(dta_UNCTAD_TBT, Reporter == reporter)
  num_hs_codes <- length(unique(country$HSCode))
  setDT(country)
  # Cast the data
  distinct_codes <- dcast(country, HSCode ~ Year, value.var = "Year", fun.aggregate = length)
  observations_with_zero <- distinct_codes[distinct_codes$`2019` == 0, ]
  num_observations <- nrow(observations_with_zero)
  data.frame(Reporter = reporter, Num_Observations_with_Zero = num_observations, Num_HS_Codes = num_hs_codes)
})
# Combine the results into a single dataframe
final_result <- do.call(rbind, results)


write.xlsx(final_result, file = "HS_withdrawed_country_TBT.xlsx", row.names = FALSE)










###############################################################################
# Get country specific 

names(dta_UNCTAD_SPS)



BRA_dta_UNCTAD_SPS <- subset(dta_UNCTAD_SPS,Reporter =="BRA" )
BRA_dta_UNCTAD_TBT <- subset(dta_UNCTAD_TBT,Reporter =="BRA" )
table(BRA_dta_UNCTAD_SPS$Year)
table(BRA_dta_UNCTAD_TBT$Year)

write.csv(BRA_dta_UNCTAD_SPS, "/home/sikeme/TRADE/WTO/data/BRA/NTM/BRA_dta_UNCTAD_SPS.csv", row.names = FALSE)
write.csv(BRA_dta_UNCTAD_TBT, "/home/sikeme/TRADE/WTO/data/BRA/NTM/BRA_dta_UNCTAD_TBT.csv", row.names = FALSE)




BRA_dta_UNCTAD_SPS <- read_csv("/home/sikeme/TRADE/WTO/data/BRA/NTM/BRA_dta_UNCTAD_SPS.csv")
table(BRA_dta_UNCTAD_SPS$Year)
BRA_dta_UNCTAD_TBT <- read_csv("/home/sikeme/TRADE/WTO/data/BRA/NTM/BRA_dta_UNCTAD_TBT.csv")





###############################################################################


# get count of NTMs instead 

###############################################################################


names(BRA_dta_UNCTAD_SPS)

dup <- BRA_dta_UNCTAD_SPS[duplicated(BRA_dta_UNCTAD_SPS[, c("ReporterISON", "Year","HSCode")]), ]



#1) get trade merged data to get countries of the world

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

new_partners <- unique(BRA_trade_Merge$PartnerISO3[BRA_trade_Merge$PartnerISO3 != "WLD"]) + 
  unique(BRA_dta_UNCTAD_SPS$PartnerISON[BRA_dta_UNCTAD_SPS$PartnerISON != "WLD"])

WLD_partner <- function(trade_data, unctad_data) {
  new_partners <- unique(trade_data$PartnerISO3[trade_data$PartnerISO3 != "WLD"]) + unique() # Get unique partners that are not "WLD" from trade data
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





###############################################################################

# HS coverage

BRA_dta_UNCTAD_SPS <- subset(dta_UNCTAD_SPS,Reporter =="BRA" )
BRA_dta_UNCTAD_TBT <- subset(dta_UNCTAD_TBT,Reporter =="BRA" )



# to visualize it 
hs_combined_coverage <- BRA_dta_UNCTAD_SPS %>%
  group_by(Year) %>%
  summarise(num_hs_combined_with_sps1 = n_distinct(HSCode))
# Create ggplot
plot_hs_combined <-ggplot(hs_combined_coverage, aes(x = Year, y = num_hs_combined_with_sps1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HScombined with SPS") +
  ggtitle("UNCTAD: Number of HS6 with SPS per Year")+
  scale_y_continuous(limits = c(0, 3200))
ggsave("~/TRADE/WTO/result/UNCTAD_BRA_HScoverage.png", plot = plot_hs_combined, width = 10, height = 6, dpi = 300)






# to visualize it 
hs_combined_coverage <- BRA_dta_UNCTAD_TBT %>%
  group_by(Year) %>%
  summarise(num_hs_combined_with_sps1 = n_distinct(HSCode))
# Create ggplot
plot_hs_combined <-ggplot(hs_combined_coverage, aes(x = Year, y = num_hs_combined_with_sps1)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Number of HScombined with TBT") +
  ggtitle("UNCTAD: Number of HS6 with TBT per Year")+
   scale_y_continuous(limits = c(0, 5000))
ggsave("~/TRADE/WTO/result/UNCTAD_BRA_HScoverage_TBT.png", plot = plot_hs_combined, width = 10, height = 6, dpi = 300)












