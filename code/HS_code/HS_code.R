
rm(list=ls())
setwd("/data/sikeme/TRADE/WTO/data")
getwd()




################################################################################
#check HS combined and HS1996 in the data 

WTO_dta <- read_csv("clean_WTO.csv")
WTO_dta$HS1996 <- as.character(WTO_dta$HS1996)
WTO_dta$HS1996 <- ifelse(nchar(WTO_dta$HS1996) == 5 & !is.na(WTO_dta$HS1996), paste0("0", WTO_dta$HS1996), WTO_dta$HS1996)


colSums(is.na(WTO_dta))
58315/nrow(WTO_dta)


HScomb_WTO <- unique(WTO_dta$HScombined)
HScomb_1996 <- length(unique(WTO_dta$HS1996))


names(WTO_dta)

WTO_dta_H4 <- subset(WTO_dta, year>2011 & year < 2017)
HScomb_WTO <- length(unique(WTO_dta_H4$HScombined))




################################################################################
#check wto CONCORDANCE 

HScomb_H4 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-109_Concordance_HS_to_H4.CSV")
HScomb_H4 <- HScomb_H4 %>% rename(HScomb = 'HS - Combined  Product Code',
                                  HS2012 = 'HS 2012 Product Code')


HScomb_H1 <- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-60_Concordance_HS_to_H1.CSV")
HScomb_H1 <- HScomb_H1 %>% rename(HScomb = 'HS - Combined  Product Code',
                                   HS1996 = 'HS 1996 Product Code')
length(unique(HScomb_H1$HS1996))

HS4_H1<- read_csv("/data/sikeme/TRADE/WTO/code/HS_code/JobID-78_Concordance_H4_to_H1.CSV")
HS4_H1 <- HS4_H1 %>% rename(HS2012 = 'HS 2012 Product Code',
                            HS1996 = 'HS 1996 Product Code')
HS4_H1 <- select(HS4_H1,HS2012, HS1996 )
length(unique(HS4_H1$HS1996))

same_values_count <- sum(HS4_H1$HS2012 == HS4_H1$HS1996)
same_values_count <- sum(HS4_H1$HS2012 != HS4_H1$HS1996)
colSums(is.na(HS4_H1))

str(HS4_H1)




################################################################################
# match  the two 



HS1996_WTO <- unique(WTO_dta$HS1996)
HS1996_WITS <- unique(HS4_H1$HS1996)

match_indices <- match(HS1996_WTO, HS1996_WITS, nomatch = 0)  # nomatch = 0 means return 0 for unmatched values
match_status <- any(match_indices != 0)


# perfect match 
match_status <- all(HS1996_WTO %in% HS1996_WITS)
not_matching_count <- sum(!(HS1996_WTO %in% HS1996_WITS))



WTO_dta_new <- left_join(WTO_dta,HS4_H1)
colSums(is.na(WTO_dta_new))



