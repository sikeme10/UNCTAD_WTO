library(tibble)
library(dplyr)
library(readr)
rm(list=ls())
# Load the data
# With IVs estimation:
rm(list=ls())
AVE_TBT_H2_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_TBT_HS_UNCTAD_IV.csv")
AVE_SPS_H2_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_SPS_HS_UNCTAD_IV.csv")
AVE_TBT_H2_WTO <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_TBT_WTO_IV.csv")
AVE_SPS_H2_WTO <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_SPS_WTO_IV.csv")




# for UNCTAD drop the first row
AVE_TBT_H2_UNCTAD <- AVE_TBT_H2_UNCTAD[,-1]
AVE_SPS_H2_UNCTAD <- AVE_SPS_H2_UNCTAD[,-1]
AVE_TBT_H2_WTO <- AVE_TBT_H2_WTO[,-1]
AVE_SPS_H2_WTO <- AVE_SPS_H2_WTO[,-1]


# if HS codes is still there
names(AVE_TBT_H2_UNCTAD)
names(AVE_SPS_H2_UNCTAD)
AVE_TBT_H2_UNCTAD <- AVE_TBT_H2_UNCTAD %>% select(-HS_codes)
AVE_SPS_H2_UNCTAD <- AVE_SPS_H2_UNCTAD %>% select(-HS_codes)

# if combine with WTO data:for SPS 
AVE_SPS_H2_UNCTAD$source <- "UNCTAD"
names(AVE_SPS_H2_UNCTAD)
AVE_SPS_H2_WTO$source <- "WTO"
names(AVE_SPS_H2_WTO)
# Combine the dataframes into a single dataframe
combined_AVE_SPS <- rbind(AVE_SPS_H2_UNCTAD, AVE_SPS_H2_WTO)


# change HS level in number
combined_AVE_SPS$HS_section <- gsub("^HS", "", combined_AVE_SPS$HS_section)
combined_AVE_SPS$HS_section <- as.numeric(combined_AVE_SPS$HS_section)



# select HS of interest 
class(filtered_AVE_SPS$HS_section)
combined_AVE_SPS <- combined_AVE_SPS %>% arrange(HS_section)

# save AVE
write.csv(combined_AVE_SPS, file = "/data/sikeme/TRADE/WTO/result/regression_HS_section/Compared_AVE_HS_SPS_IV.csv")

# select HS of interest to plot 
filtered_AVE_SPS <- combined_AVE_SPS[1:24, ]
filtered_AVE_SPS <- combined_AVE_SPS[24:42, ]
filtered_AVE_SPS <- combined_AVE_SPS


library(ggplot2)
plot_SPS <- ggplot(filtered_AVE_SPS, aes(x = factor(HS_section), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
  geom_pointrange() +
  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
  geom_text(aes(label = round(Estimate, 2), hjust = ifelse(source == "UNCTAD", -0.2, 1.2)),  # Adjust hjust based on source
            vjust = -1, size = 3)+
  labs(title = "Mean Ad Valorem Equivalents of SPS at HS section level from 2012 to 2016",
       x = "HS section level codes",
       y = "Mean Estimate of AVE using IVs") +
  scale_color_manual(name = "Databases",  # Change legend title
                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 
plot_SPS
ggsave("/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_graph/AVE_SPS_plot_1_12.png", plot_SPS, width = 10, height = 6, dpi = 300)
ggsave("/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_graph/AVE_SPS_plot_12_21.png", plot_SPS, width = 10, height = 6, dpi = 300)







#################################################################################
# For TBT 



# if combine with WTO data:for SPS 
AVE_TBT_H2_UNCTAD$source <- "UNCTAD"
names(AVE_TBT_H2_UNCTAD)
AVE_TBT_H2_WTO$source <- "WTO"
names(AVE_TBT_H2_WTO)
# Combine the dataframes into a single dataframe
combined_AVE_TBT <- rbind(AVE_TBT_H2_UNCTAD, AVE_TBT_H2_WTO)


# change HS level in number
combined_AVE_TBT$HS_section <- gsub("^HS", "", combined_AVE_TBT$HS_section)
combined_AVE_TBT$HS_section <- as.numeric(combined_AVE_TBT$HS_section)



# select HS of interest 

combined_AVE_TBT <- combined_AVE_TBT %>% arrange(HS_section)

# save AVE
write.csv(combined_AVE_TBT, file = "/data/sikeme/TRADE/WTO/result/regression_HS_section/Compared_AVE_HS_TBT_IV.csv")

# select HS of interest to plot 
filtered_AVE_TBT <- combined_AVE_TBT[1:24, ]
filtered_AVE_TBT <- combined_AVE_TBT[24:42, ]
filtered_AVE_TBT <- combined_AVE_TBT


filtered_AVE_TBT$HS_section <- filtered_AVE_TBT$HS_section +1

library(ggplot2)
plot_TBT <- ggplot(filtered_AVE_TBT, aes(x = factor(HS_section), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
  geom_pointrange() +
  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
  geom_text(aes(label = round(Estimate, 2), hjust = ifelse(source == "UNCTAD", -0.2, 1.2)),  # Adjust hjust based on source
            vjust = -1, size = 3)+
  labs(title = "Mean Ad Valorem Equivalents of TBT at HS section level from 2012 to 2016",
       x = "HS Section level codes",
       y = "Mean Estimate of AVE using IVs") +
  scale_color_manual(name = "Databases",  # Change legend title
                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 
plot_TBT
ggsave("/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_graph/AVE_TBT_plot_1_12.png", plot_TBT, width = 10, height = 6, dpi = 300)
ggsave("/data/sikeme/TRADE/WTO/result/regression_HS_section/AVE_graph/AVE_TBT_plot_12_21.png", plot_TBT, width = 10, height = 6, dpi = 300)










################################################################################

# if try to aggregate it by chapter 


combined_AVE_SPS <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/Compared_AVE_H2_IV.csv")


names(combined_AVE_SPS)




# create a section variables for each HS chapters


combined_AVE_SPS <- combined_AVE_SPS %>% mutate(
  HS_section = case_when(
    HS_level %in% 1:5 ~ ,
    HS_level %in% 6:14 ~ 2,
    HS_level %in% 15 ~ 3,
  )
)






