rm(list=ls())
setwd("/data/sikeme/TRADE/WTO/data")
getwd()

# directory name for exporting the results 
exp_directory <- "/data/sikeme/TRADE/WTO/result/summary"

library(readr)
library(ggplot2)
library(ggplot2)
library(maps)
library(rworldmap)
library(countrycode)
library(knitr)
library(kableExtra)
library(tibble)
################################################################################

      
                               # UNCTAD # 


#################################################################################

#map data available in UNCTAD 

dta_avai_UNCTAD <- read_csv("data_availability_UNCTAD_2018.csv")

# to allow a good match, need to change name of certain country 
dta_avai_UNCTAD$ISO3[dta_avai_UNCTAD$Country == "US"] <- "USA"
dta_avai_UNCTAD$ISO3[dta_avai_UNCTAD$Country == "UK"] <- "GBR"
dta_avai_UNCTAD$ISO3[dta_avai_UNCTAD$Country == "US"] <- "USA"


world_map <- map_data("world")
iso3 <- countrycode::countrycode(sourcevar = world_map$region,
                                 origin = "country.name",
                                 destination = "iso3c")
world_map <- world_map %>% mutate(iso3 = iso3)

#check if good match 
common_countries_1 <- intersect(dta_avai_UNCTAD$ISO3, world_map$iso3)
length(common_countries_1)
unmatched_countries1 <- setdiff(dta_avai_UNCTAD$ISO3, world_map$iso3)
unmatched_countries1
#map it 
world_map <- left_join(world_map, dta_avai_UNCTAD, by = c("iso3" = "ISO3"))
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = Count),
               color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") + # Adjust color gradient as needed
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_fixed(1.3) +
  theme_void()




################################################################################

dta_UNCTAD_SPS <- read_csv("dta_UNCTAD_SPS.csv")
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD_TBT.csv")


names(dta_UNCTAD_SPS)
table(dta_UNCTAD_SPS$Year)
table(dta_UNCTAD_SPS$ReporterISON)




#1) number of countries per year 
total_counts <- dta_UNCTAD_SPS %>%
  group_by(Reporter) %>%
  summarise(tot_Year = n_distinct(Year))
print(total_counts, n = Inf)



#2) create a function that shows the number of observations by country 
UNCTAD_dta_avail <- function(data) {
  # Get distinct counts of Reporter-Year combinations
  distinct_country_counts <- data %>%
    group_by(Reporter, Year) %>%
    summarise(Count = n()) %>% ungroup()
  complete_list <- expand.grid(Reporter = unique(data$Reporter),  # Create a complete list of Reporter-Year combinations
                               Year = unique(data$Year))
  # Left join distinct counts with complete list
  distinct_country_counts <- left_join(complete_list, distinct_country_counts, by = c("Reporter", "Year"))
   # Replace NA counts with 0 and create binary variable
  distinct_country_counts$Count <- ifelse(is.na(distinct_country_counts$Count), 0, 1)
  # Spread the data to wide format
  distinct_country_counts <- distinct_country_counts %>%
    pivot_wider(names_from = Year, values_from = Count, values_fill = 0)
  total_counts <- dta_UNCTAD_SPS %>%
    group_by(Reporter) %>%
    summarise(tot_Year = n_distinct(Year))
  distinct_country_counts <- left_join(distinct_country_counts,total_counts, by = "Reporter" )
  distinct_country_counts <- distinct_country_counts %>%
    select(Reporter, '2010','2011','2012','2013','2014','2015','2016','2017','2018','2019', everything())
  
  return(distinct_country_counts)
}


# Usage it on SPS 
result_SPS <- UNCTAD_dta_avail(dta_UNCTAD_SPS)
print(result_SPS, n = Inf, na.print = "NA")

result_TPT <- UNCTAD_dta_avail(dta_UNCTAD_TBT)
print(result_SPS, n = Inf, na.print = "NA")



#for SPS
world_map <- map_data("world")
iso3 <- countrycode::countrycode(sourcevar = world_map$region,
                                 origin = "country.name",
                                 destination = "iso3c")
world_map <- world_map %>% mutate(iso3 = iso3)

european_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
                        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

# Replace ISO3 codes of European countries with "EUN"
world_map$iso3[world_map$iso3 %in% european_countries] <- "EUN"


world_map <- left_join(world_map, result_SPS, by = c("iso3" = "Reporter"))
plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = factor(tot_Year)),
               color = "black") +
  scale_fill_brewer(palette = "YlOrRd", name = "Number of\nobserved years", # Empty string to omit default legend title
                    breaks = c(0, 2, 4, 6, 8, 10), # Define custom breaks
                    labels = c("NA", "1-2", "3-4", "5-6", "7-8", "9-10"),
                    na.value = "white") + # Set NA values to white
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_fixed(1.3) +
  ggtitle("") + # Empty ggtitle to suppress default plot title
  theme_void() +
  theme(
    legend.title = element_text(hjust = 0.5, vjust = 0.5), # Horizontal and vertical alignment at center
    legend.title.align = 0.5, # Center-align legend title horizontally
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0)) # Top middle title with some bottom margin
  ) +
  labs(title = "UNCTAD Collected years for SPS") # Set main title

plot
ggsave(file.path(exp_directory, "maps/UNCTAD_SPS_map.png"), plot, width = 10, height = 6)



#for TBT
world_map <- map_data("world")
iso3 <- countrycode::countrycode(sourcevar = world_map$region,
                                 origin = "country.name",
                                 destination = "iso3c")
world_map <- world_map %>% mutate(iso3 = iso3)
european_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
                        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

# Replace ISO3 codes of European countries with "EUN"
world_map$iso3[world_map$iso3 %in% european_countries] <- "EUN"
world_map <- left_join(world_map, result_TPT, by = c("iso3" = "Reporter"))
plot <-ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = factor(tot_Year)),
               color = "black") +
  scale_fill_brewer(palette = "YlOrRd", name = "Number of\nobserved years", # Empty string to omit default legend title
                    breaks = c(0, 2, 4, 6, 8, 10), # Define custom breaks
                    labels = c("NA", "1-2", "3-4", "5-6", "7-8", "9-10"),
                    na.value = "white") + # Set NA values to white
  expand_limits(x = world_map$long, y = world_map$lat) +
  coord_fixed(1.3) +
  ggtitle("") + # Empty ggtitle to suppress default plot title
  theme_void() +
  theme(
    legend.title = element_text(hjust = 0.5, vjust = 0.5), # Horizontal and vertical alignment at center
    legend.title.align = 0.5, # Center-align legend title horizontally
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 20, 0)) # Top middle title with some bottom margin
  ) +
  labs(title = "UNCTAD Collected years for TBT") # Set main title
plot
ggsave(file.path(exp_directory, "maps/UNCTAD_TBT_map.png"), plot, width = 10, height = 6)



################################################################################


                                # WTO #


################################################################################

# 1) get data 
getwd()
# get SPS
panel_dta_TBT <- read_csv("WTO_panel_dta_TBT.csv")
panel_dta_SPS <- read_csv("WTO_panel_dta_SPS.csv")
names(dta_WTO_SPS)

################################################################################
#2)Number of countries per year 


#number of year observed by country
years_observed_WTO_SPS <- dta_WTO_SPS  %>% group_by(IMP,ReporterISO3) %>%
  summarize(years_observed = n_distinct(Year))
Years_observed_WTO_TBT <- dta_WTO_TBT  %>% group_by(IMP,ReporterISO3) %>%
  summarize(Years_observed = n_distinct(Year))


#number of country observed by Year
Years_observed_WTO <- dta_WTO_SPS  %>%
  group_by(Year) %>%
  summarize(country_observed_WTO = n_distinct(IMP))



################################################################################
#3)Number of countries per Year 
obs_Year_WTO_SPS <- as.data.frame.matrix(table(dta_WTO_SPS$ReporterISO3, dta_WTO_SPS$Year))


plot_observed_Years <- function(dta_WTO) {
  # Compute the number of observed Years by country
  obs_Year_WTO <- dta_WTO %>%
    group_by(IMP, ReporterISO3) %>%
    summarize(count_Years = n_distinct(Year))
  
  world_map <- map_data("world")
  iso3 <- countrycode::countrycode(sourcevar = world_map$region,
                                   origin = "country.name",
                                   destination = "iso3c")
  world_map <- world_map %>% mutate(iso3 = iso3)
  
  # Replace ISO3 codes of European countries with "EUN"
  european_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
                          "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
                          "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
  world_map$iso3[world_map$iso3 %in% european_countries] <- "EUN"
  
  # Merge observed Years data with world map data
  world_map <- left_join(world_map, obs_Year_WTO, by = c("iso3" = "ReporterISO3"))
  
  # Create groups of observed Years
  world_map$observed_groups <- cut(world_map$count_Years, 
                                   breaks = c(0, 2, 4, 6, 8, 10), 
                                   labels = c("1-2", "3-4", "5-6", "7-8", "9-10"))
  
  plot <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = factor(observed_groups)),
                 color = "black") +
    scale_fill_brewer(palette = "YlOrRd", name = "Number of\nobserved Years", na.value = "white") +
    expand_limits(x = world_map$long, y = world_map$lat) +
    coord_fixed(1.3) +
    ggtitle("") + # Empty ggtitle to suppress default plot title
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0)) # Top center title with top margin
    ) +
    labs(title = "WTO Collected Years") # Set main title
  
  return(plot)
}

#for SPS 
plot <- plot_observed_Years(dta_WTO_SPS)
plot
ggsave(file.path(exp_directory, "WTO_SPS_map.png"), plot, width = 10, height = 6)

# For TBT 
plot <- plot_observed_Years(dta_WTO_TBT)
ggsave(file.path(exp_directory, "WTO_TBT_map.png"), plot, width = 10, height = 6)













##############################################################################


                            # UNCTAD vs WTO #


################################################################################



################################################################################
# 2)Number of countries per Year 



# Years observed as a function 
Years_observed <- function(dta_WTO_SPS, dta_UNCTAD_SPS) {
  # Number of Years observed by country for WTO
  Years_observed_WTO <- dta_WTO_SPS %>%
    group_by(IMP, ReporterISO3) %>%
    summarize(Years_observed = n_distinct(Year))
  
  # Number of countries observed by Year for WTO
  Years_observed_WTO <- dta_WTO_SPS %>%
    group_by(Year) %>%
    summarize(country_observed_WTO = n_distinct(IMP))
  Years_observed_WTO$Year <- as.character(Years_observed_WTO$Year)
  total_WTO <- data.frame(Year = "Total", country_observed_WTO = length(unique(dta_WTO_SPS$IMP)))
  Years_observed_WTO <- bind_rows(Years_observed_WTO, total_WTO)
  
  # Number of Years observed by country for UNCTAD
  Years_observed_UNCTAD <- dta_UNCTAD_SPS %>%
    group_by(Year) %>%
    summarise(country_observed_UNCTAD = n_distinct(Reporter))
  Years_observed_UNCTAD$Year <- as.character(Years_observed_UNCTAD$Year)
  total_UNCTAD <- data.frame(Year = "Total", country_observed_UNCTAD = length(unique(dta_UNCTAD_SPS$Reporter)))
  Years_observed_UNCTAD <- bind_rows(Years_observed_UNCTAD, total_UNCTAD)
  
  # Combine Years observed datasets
  total_Year_observed <- full_join(Years_observed_WTO, Years_observed_UNCTAD, by = c("Year" = "Year"))
  total_Year_observed <- column_to_rownames(total_Year_observed, var = "Year")
  total_Year_observed <- t(total_Year_observed)
  return(total_Year_observed)
}
#for SPS 
total_Year_observed <-Years_observed(dta_WTO_SPS, dta_UNCTAD_SPS)
write.csv(total_Year_observed, 
          file = file.path(exp_directory, "Countries_observed_per_Year_SPS.csv"), 
          row.names = TRUE)

total_Year_observed <-Years_observed(dta_WTO_TBT, dta_UNCTAD_TBT)
write.csv(total_Year_observed, 
          file = file.path(exp_directory, "Countries_observed_per_Year_TBT.csv"), 
          row.names = TRUE)



#if needed
table <- kable(total_Year_observed, 
               caption = "Countries Observed Per Year",
               col.names = c("Year", "WTO", "UNCTAD"),
               align = "c")

write.csv(total_Year_observed, 
          file = file.path(exp_directory, "Countries_observed_per_Year.csv"), 
          row.names = TRUE)

################################################################################
 
#3) Compare country coverage between thw two data set :
result_SPS <- result_SPS %>% select(Reporter , tot_Year)
result_SPS <- result_SPS %>% rename(tot_Years_UNCTAD = tot_Year)
Years_observed_WTO_SPS <- Years_observed_WTO_SPS %>% rename(tot_Years_WTO = Years_observed)

Reporter <- unique(result_SPS$Reporter)
country_names <- countrycode(Reporter, "iso3c", "country.name")
country_names <- as.data.frame(cbind(Reporter,country_names))
result_SPS <- left_join(result_SPS, country_names, by=c("Reporter" = "Reporter")  )




common_countries_1 <- intersect(result_SPS$Reporter, Years_observed_WTO_SPS$ReporterISO3)
length(common_countries_1)
unmatched_countries1 <- setdiff( Years_observed_WTO_SPS$ReporterISO3, result_SPS$Reporter)
unmatched_countries1

countries <- full_join(result_SPS,Years_observed_WTO_SPS, by = c("Reporter" = "ReporterISO3"), keep =TRUE )

countries <- countries %>%  mutate(names = coalesce(Reporter, ReporterISO3))
countries <- countries %>%  mutate(names1 = coalesce(IMP, country_names))

countries$tot_Years_WTO <- ifelse(is.na(countries$tot_Years_WTO), 0, countries$tot_Years_WTO)
countries$tot_Years_UNCTAD <- ifelse(is.na(countries$tot_Years_UNCTAD), 0, countries$tot_Years_UNCTAD)


#plot various countreis
plot <- ggplot(countries, aes(x = reorder(names1,  tot_Years_UNCTAD))) +
  geom_bar(aes(y = tot_Years_WTO, fill = "tot_Years_WTO"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = tot_Years_UNCTAD, fill = "tot_Years_UNCTAD"), stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = 1:10)
plot

ggsave(file.path(exp_directory, "WTO_UNCTAD_countries.png"), plot, width = 10, height = 6)












################################################################################
#number of countries in the different datasets
length(unique(Years_observed_WTO$IMP))
length(unique(dta_avai_UNCTAD$Country))



dta_UNCTAD <- subset(dta_avai_UNCTAD, select = c(Country, ISO3, Count))
#merge the two 
count_countries <- full_join(Years_observed_WTO,dta_UNCTAD, by = c("ReporterISO3" = "ISO3"))
names(count_countries)
count_countries <- rename(count_countries, WTO_Year_count = Years_observed)
count_countries <- rename(count_countries, UNCTAD_Year_count = Count)
count_countries$IMP[is.na(count_countries$IMP)] <- count_countries$Country[is.na(count_countries$IMP)]
#replace NA by 0
count_countries$WTO_Year_count[is.na(count_countries$WTO_Year_count)] <- 0
count_countries$UNCTAD_Year_count[is.na(count_countries$UNCTAD_Year_count)] <- 0
count_countries$IMP <- reorder(count_countries$IMP, count_countries$UNCTAD_Year_count)

#plot them all
ggplot(count_countries, aes(x = IMP)) +
  geom_bar(aes(y = WTO_Year_count, fill = "WTO_Year_count"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = UNCTAD_Year_count, fill = "UNCTAD_Year_count"), stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_y_continuous(breaks = 1:10)



half <- nrow(count_countries) / 2
subset1 <- count_countries[1:half, ]
subset2 <- count_countries[(half + 1):nrow(count_countries), ]

# Create separate plots for each subset
plot1 <- ggplot(subset1, aes(x = IMP)) +
  geom_bar(aes(y = WTO_Year_count, fill = "WTO_Year_count"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = UNCTAD_Year_count, fill = "UNCTAD_year_count"), stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = 1:10)


plot2 <- ggplot(subset2, aes(x = IMP)) +
  geom_bar(aes(y = WTO_year_count, fill = "WTO_year_count"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = UNCTAD_year_count, fill = "UNCTAD_year_count"), stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Country", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = 1:10)



# Print the plots
print(plot1)
print(plot2)


################################################################################
# Number of country per year


dta_WTO <- read_csv("clean_WTO.csv")



country_counts_UNCTAD <- dta %>%
  group_by(Year) %>%
  summarise(num_countries = n_distinct(Reporter), reporters = list(unique(Reporter)))

country_counts_UNCTAD
country_counts_UNCTAD$reporters <- sapply(country_counts_UNCTAD$reporters, paste, collapse = ",")

write.xlsx(country_counts_UNCTAD, "/home/sikeme/TRADE/WTO/code/summary_compare/country_counts_UNCTAD.xlsx", rowNames = FALSE)


country_counts_WTO <- dta_WTO %>%
  group_by(year) %>%
  summarise(num_countries = n_distinct(IMP), reporters = list(unique(IMP)))

country_counts_WTO



library(openxlsx)

# Convert the 'reporters' column to a character vector
country_counts_WTO$reporters <- sapply(country_counts_WTO$reporters, paste, collapse = ",")

# Save country_counts_UNCTAD as an Excel file
write.xlsx(country_counts_WTO, "/home/sikeme/TRADE/WTO/code/summary_compare/country_counts_WTO.xlsx", rowNames = FALSE)




################################################################################


                # look NTM coverage per year # 


################################################################################
rm(list=ls())

# get data from both WTO and UNCTAD 
dta_WTO_SPS <- read_csv("dta_WTO/clean_WTO_SPS.csv")
dta_WTO_TBT <- read_csv("dta_WTO/clean_WTO_TBT.csv")

# load UNCTAD 
dta_UNCTAD_SPS <- read_csv("dta_UNCTAD/clean_dta_UNCTAD_SPS.csv")
dta_UNCTAD_TBT <- read_csv("dta_UNCTAD/clean_dta_UNCTAD_TBT.csv")


names(dta_UNCTAD_SPS)
names(dta_WTO_SPS)


###############################################################################


# For a specific country
# Select country of interest 
BRA_WTO <- dta_WTO_SPS %>% filter(ReporterISO3 =="BRA")
BRA_UNCTAD <- dta_UNCTAD_SPS %>% filter(Reporter =="BRA")



# to visualize it 
hs_combined_coverage_WTO <- BRA_WTO %>%
  filter(SPS1 >0) %>%
  group_by(Year) %>%
  summarise(num_hs_SPS_WTO = n_distinct(ProductCode))
hs_combined_coverageUNCTAD <- BRA_UNCTAD %>%
  group_by(Year) %>%
  summarise(num_hs_SPS_UNCTAD = n_distinct(HSCode))
#join all 
hs_combined <- full_join(hs_combined_coverage_WTO, hs_combined_coverageUNCTAD, by = c("Year" = "Year"))

# Create ggplot
p <-  ggplot() +
  geom_line(data = hs_combined, aes(x = Year, y = num_hs_SPS_WTO, color = "WTO")) +
  geom_point(data = hs_combined, aes(x = Year, y = num_hs_SPS_WTO, color = "WTO")) +
  geom_line(data = hs_combined, aes(x = Year, y = num_hs_SPS_UNCTAD, color = "UNCTAD")) +
  geom_point(data = hs_combined, aes(x = Year, y = num_hs_SPS_UNCTAD, color = "UNCTAD")) +
  labs(x = "Year", y = "Number of distinct HS code covered by an SPS") +
  ggtitle("SPS: Brazil HS coverage per Year ") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("UNCTAD SPS", "WTO SPS"))+
  ylim(0, 5000)
p
ggsave("/data/sikeme/TRADE/WTO/result/country/BRA_HScoverage_SPS.png", plot = p, width = 10, height = 6, dpi = 300)



###############################################################################

# for  a list of countries

reporters <-c("ARG", "BOL", "BRA", "CHL", "COL", 
              "ECU", "GUY", "PRY", "PER", 
              "SUR", "URY", "VEN")   # Add as many as needed

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each reporter
for (reporter in reporters) {
  # Filter data for the current reporter
  WTO_data <- dta_WTO_SPS %>% filter(ReporterISO3 == reporter)
  UNCTAD_data <- dta_UNCTAD_SPS %>% filter(Reporter == reporter)
  
  # Summarize the number of distinct HS codes in each year for WTO and UNCTAD
  hs_combined_coverage_WTO <- WTO_data %>%    filter(SPS1 > 0) %>%
    group_by(Year) %>% summarise(num_hs_SPS_WTO = n_distinct(ProductCode), .groups = 'drop')
  
  hs_combined_coverage_UNCTAD <- UNCTAD_data %>% group_by(Year) %>%
    summarise(num_hs_SPS_UNCTAD = n_distinct(HSCode), .groups = 'drop')
  
  # Join the WTO and UNCTAD data
  hs_combined <- full_join(hs_combined_coverage_WTO, hs_combined_coverage_UNCTAD, by = "Year")
  # Add a column to specify the current reporter
  hs_combined$ReporterISO3 <- reporter
  
  # Append the current data to the combined data frame
  combined_data <- rbind(combined_data, hs_combined)
}

# Now create a single plot with facet_wrap
p <- ggplot() +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_SPS_WTO, color = "WTO")) +
  geom_point(data = combined_data, aes(x = Year, y = num_hs_SPS_WTO, color = "WTO")) +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_SPS_UNCTAD, color = "UNCTAD")) +
  geom_point(data = combined_data, aes(x = Year, y = num_hs_SPS_UNCTAD, color = "UNCTAD")) +
  labs(x = "Year", y = "Number of distinct HS code covered by an SPS") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("UNCTAD SPS", "WTO SPS")) +
  ylim(0, 5000) +
  facet_wrap(~ReporterISO3, scales = "free_y") +  # Use facet_wrap to create separate panels for each reporter
  ggtitle("SPS HS Coverage per Year by Reporter")+
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),     # Adjust legend text size
    axis.title.x = element_text(size = 10),   # Adjust x-axis label size
    axis.title.y = element_text(size = 10),   # Adjust y-axis label size
    axis.text.x = element_text(size = 8),      # Adjust x-axis tick label size
    axis.text.y = element_text(size = 8)       # Adjust y-axis tick label size
  )
p

ggsave(plot = p, filename = file.path(exp_directory, "dta_comparison/SPS_distinctHS_coverage.png"), width = 8, height = 6, dpi = 300)




# For total number of HS codes:

reporters <-c("ARG")  


reporters <-c("ARG", "BOL", "BRA", "CHL", "COL", 
              "ECU", "GUY", "PRY", "PER", 
              "SUR", "URY", "VEN")  

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each reporter
for (reporter in reporters) {
  # Filter data for the current reporter
  WTO_data <- dta_WTO_SPS %>% filter(ReporterISO3 == reporter)
  UNCTAD_data <- dta_UNCTAD_SPS %>% filter(Reporter == reporter)
  
  # Summarize the number of distinct HS codes in each year for WTO and UNCTAD
  hs_combined_coverage_WTO <- WTO_data %>%    filter(SPS1 > 0) %>%
    group_by(Year) %>% summarise(num_hs_SPS_WTO =sum(SPS1), .groups = 'drop')
  
  hs_combined_coverage_UNCTAD <- UNCTAD_data %>% group_by(Year) %>%
    summarise(num_hs_SPS_UNCTAD = sum(SPS_count), .groups = 'drop')
  
  # Join the WTO and UNCTAD data
  hs_combined <- full_join(hs_combined_coverage_WTO, hs_combined_coverage_UNCTAD, by = "Year")
  # Add a column to specify the current reporter
  hs_combined$ReporterISO3 <- reporter
  
  # Append the current data to the combined data frame
  combined_data <- rbind(combined_data, hs_combined)
}

# Now create a single plot with facet_wrap
p <- ggplot() +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_SPS_WTO, color = "WTO")) +
  geom_point(data = combined_data, aes(x = Year, y = num_hs_SPS_WTO, color = "WTO")) +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_SPS_UNCTAD, color = "UNCTAD")) +
  geom_point(data = combined_data, aes(x = Year, y = num_hs_SPS_UNCTAD, color = "UNCTAD")) +
  labs(x = "Year", y = "Number of total SPS at HS 6 level") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("UNCTAD SPS", "WTO SPS")) +
  ylim(0, 200000) +
  facet_wrap(~ReporterISO3, scales = "free_y") +  # Use facet_wrap to create separate panels for each reporter
  ggtitle("Count of tatal number of SPS per Year by Reporter")+
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),     # Adjust legend text size
    axis.title.x = element_text(size = 10),   # Adjust x-axis label size
    axis.title.y = element_text(size = 10),   # Adjust y-axis label size
    axis.text.x = element_text(size = 8),      # Adjust x-axis tick label size
    axis.text.y = element_text(size = 8)       # Adjust y-axis tick label size
  )
p

ggsave(plot = p, filename = file.path(exp_directory, "dta_comparison/SPS_total_HS.png"), width = 8, height = 6, dpi = 300)






###############################################################################
# for TBT
reporters <-c("ARG", "BOL", "BRA", "CHL", "COL", 
              "ECU", "GUY", "PRY", "PER", 
              "SUR", "URY", "VEN")  # Add as many as needed

# Initialize an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each reporter
for (reporter in reporters) {
  # Filter data for the current reporter
  WTO_data <- dta_WTO_TBT %>% filter(ReporterISO3 == reporter)  # Replacing with TBT data
  UNCTAD_data <- dta_UNCTAD_TBT %>% filter(Reporter == reporter)  # Replacing with TBT data
  
  # Summarize the number of distinct HS codes in each year for WTO and UNCTAD
  hs_combined_coverage_WTO <- WTO_data %>%
    filter(TBT1 > 0) %>%
    group_by(Year) %>%
    summarise(num_hs_TBT_WTO = n_distinct(ProductCode), .groups = 'drop')  # Replacing SPS with TBT
  
  hs_combined_coverage_UNCTAD <- UNCTAD_data %>%
    group_by(Year) %>%
    summarise(num_hs_TBT_UNCTAD = n_distinct(HSCode), .groups = 'drop')  # Replacing SPS with TBT
  
  # Join the WTO and UNCTAD data
  hs_combined <- full_join(hs_combined_coverage_WTO, hs_combined_coverage_UNCTAD, by = "Year")
  
  # Add a column to specify the current reporter
  hs_combined$ReporterISO3 <- reporter
  
  # Append the current data to the combined data frame
  combined_data <- rbind(combined_data, hs_combined)
}

# Now create a single plot with facet_wrap
p <- ggplot() +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_TBT_WTO, color = "WTO")) +  # Replacing SPS with TBT
  geom_point(data = combined_data, aes(x = Year, y = num_hs_TBT_WTO, color = "WTO")) +  # Replacing SPS with TBT
  geom_line(data = combined_data, aes(x = Year, y = num_hs_TBT_UNCTAD, color = "UNCTAD")) +  # Replacing SPS with TBT
  geom_point(data = combined_data, aes(x = Year, y = num_hs_TBT_UNCTAD, color = "UNCTAD")) +  # Replacing SPS with TBT
  labs(x = "Year", y = "Number of distinct HS code covered by a TBT") +  # Updating label for TBT
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("UNCTAD TBT", "WTO TBT")) +  # Replacing SPS with TBT in the legend
  ylim(0, 5000) +
  facet_wrap(~ReporterISO3, scales = "free_y") +  # Use facet_wrap to create separate panels for each reporter
  ggtitle("TBT HS Coverage per Year by Reporter")+  # Updating title for TBT
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),     # Adjust legend text size
    axis.title.x = element_text(size = 10),   # Adjust x-axis label size
    axis.title.y = element_text(size = 10),   # Adjust y-axis label size
    axis.text.x = element_text(size = 8),      # Adjust x-axis tick label size
    axis.text.y = element_text(size = 8)       # Adjust y-axis tick label size
  )
p


ggsave(plot = p, filename = file.path(exp_directory, "dta_comparison/TBT_distinctHS_coverage.png"), width = 8, height = 6, dpi = 300)



# for total count:


combined_data <- data.frame()

# Loop through each reporter
for (reporter in reporters) {
  # Filter data for the current reporter
  WTO_data <- dta_WTO_TBT %>% filter(ReporterISO3 == reporter)  # Adjust variable name if needed
  UNCTAD_data <- dta_UNCTAD_TBT %>% filter(Reporter == reporter)  # Adjust variable name if needed
  
  # Summarize the number of distinct HS codes in each year for WTO and UNCTAD
  hs_combined_coverage_WTO <- WTO_data %>% 
    filter(TBT1 > 0) %>%
    group_by(Year) %>% 
    summarise(num_hs_TBT_WTO = sum(TBT1), .groups = 'drop')
  
  hs_combined_coverage_UNCTAD <- UNCTAD_data %>% 
    group_by(Year) %>%
    summarise(num_hs_TBT_UNCTAD = sum(TBT_count), .groups = 'drop')  # Adjust variable name if needed
  
  # Join the WTO and UNCTAD data
  hs_combined <- full_join(hs_combined_coverage_WTO, hs_combined_coverage_UNCTAD, by = "Year")
  
  # Add a column to specify the current reporter
  hs_combined$ReporterISO3 <- reporter
  
  # Append the current data to the combined data frame
  combined_data <- rbind(combined_data, hs_combined)
}

# Now create a single plot with facet_wrap
p <- ggplot() +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_TBT_WTO, color = "WTO")) +
  geom_point(data = combined_data, aes(x = Year, y = num_hs_TBT_WTO, color = "WTO")) +
  geom_line(data = combined_data, aes(x = Year, y = num_hs_TBT_UNCTAD, color = "UNCTAD")) +
  geom_point(data = combined_data, aes(x = Year, y = num_hs_TBT_UNCTAD, color = "UNCTAD")) +
  labs(x = "Year", y = "Number of total TBT at HS 6 level") +
  scale_color_manual(values = c("blue", "red"), 
                     labels = c("UNCTAD TBT", "WTO TBT")) +
  ylim(0, 200000) +
  facet_wrap(~ReporterISO3, scales = "free_y") +  # Use facet_wrap to create separate panels for each reporter
  ggtitle("Count of Total Number of TBT per Year by Reporter") +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8),     # Adjust legend text size
    axis.title.x = element_text(size = 10),   # Adjust x-axis label size
    axis.title.y = element_text(size = 10),   # Adjust y-axis label size
    axis.text.x = element_text(size = 8),      # Adjust x-axis tick label size
    axis.text.y = element_text(size = 8)       # Adjust y-axis tick label size
  )

print(p)


ggsave(plot = p, filename = file.path(exp_directory, "dta_comparison/TBT_total_HS.png"), width = 8, height = 6, dpi = 300)



###############################################################################


# HS code coverage Brazil 

###############################################################################
rm(list=ls())
Merge_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_UNACTAD_count.csv")
Merge <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_WTO_count.csv")
names(Merge_UNCTAD)

# HS aggregates variables 
Merge_UNCTAD$H2<- substring(Merge_UNCTAD$ProductCode   , first = 1, last = 2)
Merge_UNCTAD$H1<- substring(Merge_UNCTAD$ProductCode   , first = 1, last = 1)
Merge$H1<- substring(Merge$ProductCode   , first = 1, last = 1)
Merge$H2<- substring(Merge$ProductCode   , first = 1, last = 2)
Merge$H3<- substring(Merge$ProductCode   , first = 1, last = 3)
Merge$H4<- substring(Merge$ProductCode   , first = 1, last = 4)


# count at HS levels:
Count_H1_year_UNCTAD <- Merge_UNCTAD %>%
  group_by(H1) %>%
  summarise(ntm_count_SPS_H1 = sum(ntm_count_SPS),
            ntm_count_TBT_H1 = sum(ntm_count_TBT))
Count_H1_year_UNCTAD$source <- "UNCTAD"

Count_H1_year_WTO <- Merge %>%
  group_by(H1) %>%
  summarise(ntm_count_SPS_H1 = sum(ntm_count_SPS),
            ntm_count_TBT_H1 = sum(ntm_count_TBT))

Count_H1_year_WTO$source <- "WTO"


Count_H1_year <- full_join(Count_H1_year_UNCTAD,Count_H1_year_WTO)

names(Count_H1_year)
# for SPS 
plot_SPS <- ggplot(Count_H1_year, aes(x = H1, y = ntm_count_SPS_H1, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "HS1 digit groups", y = "Number of SPS applied at HS6 level", title = "Number of applied SPS to products (at HS 6 digit level) by HS1 groups from 2012 to 2016",
       fill = "Database")
plot_SPS
ggsave("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_Count_time/distinct_HS_SPS.png", plot_SPS, width = 10, height = 6, dpi = 300)

# for TBT
plot_TBT <- ggplot(Count_H1_year, aes(x = H1, y = ntm_count_TBT_H1, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "HS1 digit groups", y = "Number of TBT applied at HS6 level", title = "Number of applied TBT to products (HS 6 digit level) by HS1 groups from 2012 to 2016",
       fill = "Database")
plot_TBT
ggsave("/data/sikeme/TRADE/WTO/result/regression_H1/AVE_Count_time/distinct_HS_TBT.png", plot_TBT, width = 10, height = 6, dpi = 300)



###############################################################################
# look at distinct HS covereage at HS6 for each HS 1 groups 





