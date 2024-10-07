
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
library(openxlsx)

# results summary directory 
directory <- "/data/sikeme/TRADE/WTO/result/summary"


###############################################################################
# STRUCTURE of code 

# 0) Load the data 
# 1) Check data availabitlity 



###############################################################################

# 0) Load the data 

dta_UNCTAD_TBT <- read_csv("dta_UNCTAD/clean_dta_UNCTAD_TBT.csv")
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD/clean_dta_UNCTAD_SPS.csv") 


###############################################################################

# 1) data availabitlity 

###############################################################################


names(dta_UNCTAD_SPS)


length(unique(dta_UNCTAD_SPS$Reporter))

# create a data frame to get datab aivalabitly by country with 1 if country observerved in a certain year
UNCTAD_available <- dta_UNCTAD_SPS %>% arrange(Year) %>% group_by(Reporter, Year) %>% 
  summarise(obs = n(), .groups = "drop") %>%
  # Mark presence of observation with 1
  mutate(presence = ifelse(obs > 0, 1, 0)) %>%
  # Reshape the data to have years as columns
  select(-obs) %>%
  pivot_wider(names_from = Year, values_from = presence, values_fill = 0)  %>% relocate(order(colnames(.)))


write.csv(UNCTAD_available, file.path(directory , "/dta_availability/UNCTAD_NTM.csv"), row.names = FALSE)


# a) I f look at south american countries specifically 
# allow to check for IVs 

# look at south american countries
south_america_iso <- c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "GUY", "PRY", "PER", "SUR", "URY", "VEN")

UNCTAD_available_SA <- UNCTAD_available %>% filter(Reporter %in% south_america_iso)


# Plotting dta availability:   

library(ggplot2)
UNCTAD_available_long <- UNCTAD_available_SA %>%
  pivot_longer(
    cols = -Reporter,  # Keep Reporter fixed, unpivot all year columns
    names_to = "Year",  # Name the new column 'Year'
    values_to = "presence"  # Values will be stored in 'presence'
  ) %>%
  mutate(Year = as.numeric(Year))  # Ensure Year is numeric

# Create the ggplot
plot <- ggplot(UNCTAD_available_long, aes(x = Year, y = Reporter)) +
  geom_point(aes(color = factor(presence)), shape = 16, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("0" = NA, "1" = "blue")) +  # Show blue dots where presence == 1
  labs(x = "Year", y = "Reported country", title = "Data availabaility by country and year: case of South American countries") +
  scale_x_continuous(breaks = unique(UNCTAD_available_long$Year)) +  # Ensure all years are shown
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
    panel.grid.major = element_line(color = "grey80")
  )
plot

ggsave(file.path(directory,"/dta_availability/UNCTAD_dta_availability_SA.png"), plot = plot, width = 10, height = 6, dpi = 300)






#################################################################################




# b) Look at country withdrawels

names(dta_UNCTAD_SPS)


# Get distinct HS codes affected by a permanent withdrawl 

# to get number of HS code retrieve by country we can run this loop 
reporters <- unique(dta_UNCTAD_SPS$Reporter)
# Create a list to store results
results <- lapply(reporters, function(reporter) {
  country <- subset(dta_UNCTAD_SPS, Reporter == reporter)
  num_hs_codes <- length(unique(country$HSCode))
  setDT(country)
  # Cast the data: reshapes the country data by converting rows into columns.
  distinct_codes <- dcast(country, HSCode ~ Year, value.var = "Year", fun.aggregate = length) 
  
  observations_with_zero <- distinct_codes[distinct_codes$`2019` == 0, ]
  num_observations <- nrow(observations_with_zero)
  data.frame(Reporter = reporter, Num_Observations_with_Zero = num_observations, Num_HS_Codes = num_hs_codes)
})
# Combine the results into a single dataframe
final_result <- do.call(rbind, results)


write.xlsx(final_result, file.path(directory,"/UNCTAD/HS_withdrawed_country_SPS.xlsx"), row.names = FALSE)



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












