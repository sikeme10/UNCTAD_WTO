

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/WTO/data/")
getwd()

library(readr)
library(tidyr)
library(dplyr)
library(DataCombine)


WITS_trade_2012 <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade_2012.csv")
WITS_trade_2013 <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade_2013.csv")
WITS_trade_2014 <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade_2014.csv")
WITS_trade_2015 <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade_2015.csv")
WITS_trade_2016 <- read_csv("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade_2016.csv")


################################################################################

# Merge all trade data 

trade <- rbind(WITS_trade_2012, WITS_trade_2013, WITS_trade_2014,WITS_trade_2015,WITS_trade_2016  )
write.csv(trade, "/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade.csv", row.names = FALSE)

trade <- fread("/data/sikeme/TRADE/WTO/data/dta_WITS/trade/WITS_trade.csv")


# if issues with first column:
trade <-  trade[,-1]

# checks
colSums(is.na(trade))
names(trade)
table(trade$ReporterISO3)
length(unique(trade$ProductCode))
table(trade$Year)
length(unique(trade$ProductCode))
length(unique(trade$PartnerISO3))
length(unique(trade$TradeFlowCode))
table(trade$Nomenclature)
summary(trade$TradeValue)
summary(trade$Quantity)





################################################################################


# 1) create price variable 

# If interested in price based 
################################################################################

#drop 0 values for trade value
trade <- trade[trade$TradeValue != 0, ]
summary(trade$TradeValue)
# Calculate the 'price' variable
trade$price <- (trade$TradeValue * 1000) / trade$Quantity
summary(trade$price)
#remove if NA or inf
trade <- trade[!is.na(trade$price)&trade$price<Inf,]

#truncate top (and bottom) n of prices ( here 5%)
trade <- trade[trade$price<quantile(trade$price,prob=1-5/100),]
trade <- trade[trade$price>quantile(trade$price,prob=5/100),]
summary(trade$price)



######################################################################################


# 2) quantity based 


# if interested in quantity based 
################################################################################
# a) look at HS6 code and make it a character

names(trade)
head(trade)

length(unique(trade$ProductCode))
summary(trade$Quantity)


# observe NA in quantity variable
test <- trade %>% filter(is.na(Quantity)) %>% arrange(ProductCode)
table(trade$QtyToken)



# DROP NA and replace by 0
trade$Quantity[is.na(trade$Quantity)] <- 0



################################################################################

# Convert HS6 code in characters to match other data

class(trade$ProductCode)
# make the variable in character: 
trade$ProductCode <- stri_pad_left(trade$ProductCode,6,0)
num_characters <- nchar(trade$ProductCode)
# Check if all values have 6 characters
all_have_six_characters <- all(num_characters == 6)






################################################################################
# Checks

names(trade)


length(unique(trade$ProductCode))
table(trade$Year)
table(trade$ReporterISO3)
summary(trade$Quantity)

################################################################################


# c) export
# for quant based:
write.csv(trade, "/data/sikeme/TRADE/WTO/data/dta_WITS/trade/clean_WITS_trade_quant.csv", row.names = FALSE)

# for price based
write.csv(trade, "/data/sikeme/TRADE/WTO/data/dta_WITS/trade/clean_WITS_trade_price.csv", row.names = FALSE)




