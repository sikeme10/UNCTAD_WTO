
# get the country characteristics variables 


rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/WTO/data/country_char")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)




################################################################################


                         # GDP per capita

################################################################################



# install.packages("WDI")
library(WDI)

WDIsearch("GDP")

gdp_per_capita <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1995, end = 2019)

# if want current gdp
gdp <- WDI(country = "all", indicator = "NY.GDP.MKTP.CD", start = 1995, end = 2019)

names(gdp)

# rename some of the variables
gdp_per_capita <- rename(gdp_per_capita, Gdp_per_cap = NY.GDP.PCAP.CD)
gdp <- rename(gdp, Gdp_current_val = NY.GDP.MKTP.CD)

#if take ln of it 
gdp_per_capita$ln_Gdp_per_cap <- log(gdp_per_capita$Gdp_per_cap)

names(gdp_per_capita)

write.csv(gdp_per_capita, "clean_gdp_per_capita.csv", row.names = FALSE)
write.csv(gdp, "clean_gdp.csv", row.names = FALSE)



################################################################################



################################################################################


# RTA

################################################################################


#vhttps://www.ewf.uni-bayreuth.de/en/research/RTA-data/index.html

library(readr)
rta_20221214 <- read_csv("rta_20221214.csv")
rta <- subset(rta_20221214, year>1994)
names(rta)
rta <- rta[c("exporter", "importer" ,"year","rta")]

write.csv(rta, "clean_rta.csv", row.names = FALSE)


################################################################################


# gravity var

################################################################################



# http://www.cepii.fr/DATA_DOWNLOAD/gravity/doc/Gravity_documentation.pdf
# s country_id_o when referring to the origin and country_id_d when referring to the destination


library(readr)
Gravity <- read_csv("Gravity_V202211.csv")
names(Gravity)

Gravity <- subset(Gravity, year>1994)

table(Gravity$year)
table(Gravity$rta_type)

#  rta_coverage: Coverage of the trade agreement. 0 = “no trade agreement”. 1 = “goods
# only”, 2 = “services only”, 3 = “goods and services”, bilateral.

table(Gravity$rta_coverage)
Gravity <- Gravity %>% mutate(rta = ifelse(rta_coverage > 0, 1, 0))
table(Gravity$rta)
# variables of interest are : contig (Dummy equal to 1 if countries are contiguous)
# dist, comlang_off (common language) and "GDPs" and rtas



clean_Gravity <- Gravity[c("year","country_id_o","country_id_d","iso3_o","iso3_d"
                           ,"contig", "dist", "comlang_off")]

table(clean_Gravity $iso3_o)
write.csv(clean_Gravity, "clean_Gravity.csv", row.names = FALSE)



# o for origini and d for destinantion 




################################################################################


# landlock 

################################################################################

# source:
# https://www.usitc.gov/data/gravity/dgd.htm

library(readr)
landlock1 <- read_csv("release_2.1_2010_2014.csv")
names(landlock1)


landlock1 <- select(landlock1, 
                    year,country_o, iso3_o, country_d,iso3_d,landlocked_o,landlocked_d)


landlock2 <- read_csv("release_2.1_2015_2019.csv")
names(landlock2)


landlock2 <- select(landlock2, 
                    year,country_o, iso3_o, country_d,iso3_d,landlocked_o,landlocked_d)


landlock <- rbind(landlock1, landlock2)



write.csv(landlock, "clean_landlock.csv", row.names = FALSE)





################################################################################

                  # Get data for specific country


################################################################################

# 1) GDP per capita

gdp_per_capita <- read_csv("clean_gdp_per_capita.csv")
names(gdp_per_capita)


brazil_ln_GDP <- gdp_per_capita %>%
  filter(iso3c == "BRA") %>%
  select(year, ln_GDP_Brazil = ln_Gdp_per_cap)

# Merge the Brazil data back into the original data by year and country
gdp_per_capita1 <- gdp_per_capita[gdp_per_capita$iso3c != "BRA", ]
gdp_per_capita1 <- gdp_per_capita1 %>% left_join(brazil_ln_GDP, by = "year")

names(gdp_per_capita1)
gdp_per_capita1 <- gdp_per_capita1 %>%
  rename(PartnerISO3 = iso3c)

write.csv(gdp_per_capita1, "/home/sikeme/TRADE/WTO/data/BRA/country_char/BRA_gdp_per_capita.csv", row.names = FALSE)

names(gdp_per_capita1)
################################################################################

# 2) RTA

rta <- read_csv( "clean_rta.csv")
names(rta)
table(rta$year)

rta<- subset(rta, year>2009 & year <2020)

#select RTA between BRA and other countries 

rta1 <- rta[rta$importer == "BRA", ]

rta1 <- rta1 %>%
  rename(PartnerISO3 = exporter,
         ReporterISO3 = importer)
names(rta1)



