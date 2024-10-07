library(tibble)

rm(list=ls())
# Load the data
AVE_TBT_H2_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_TBT_H2_UNCTAD.csv")
AVE_SPS_H2_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_SPS_H2_UNCTAD.csv")
AVE_TBT_H2_WTO <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_TBT_H2_WTO.csv")
AVE_SPS_H2_WTO <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_SPS_H2_WTO.csv")

# With IVs estimation:
rm(list=ls())
AVE_TBT_H2_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_TBT_H2_UNCTAD_IV.csv")
AVE_SPS_H2_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_SPS_H2_UNCTAD_IV.csv")
AVE_TBT_H2_WTO <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_TBT_H2_WTO_IV.csv")
AVE_SPS_H2_WTO <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/AVE_SPS_H2_WTO_IV.csv")









# for UNCTAD drop the first row
AVE_TBT_H2_UNCTAD <- AVE_TBT_H2_UNCTAD[,-1]
AVE_SPS_H2_UNCTAD <- AVE_SPS_H2_UNCTAD[,-1]
AVE_TBT_H2_WTO <- AVE_TBT_H2_WTO[,-1]
AVE_SPS_H2_WTO <- AVE_SPS_H2_WTO[,-1]


# if combine with WTO data:for SPS 
AVE_SPS_H2_UNCTAD$source <- "UNCTAD"
names(AVE_SPS_H2_UNCTAD)
AVE_SPS_H2_WTO$source <- "WTO"
names(AVE_SPS_H2_WTO)
# Combine the dataframes into a single dataframe
combined_AVE_SPS <- rbind(AVE_SPS_H2_UNCTAD, AVE_SPS_H2_WTO)


# change HS level in number
combined_AVE_SPS$HS_level <- gsub("^HS", "", combined_AVE_SPS$HS_level)
combined_AVE_SPS$HS_level <- as.numeric(combined_AVE_SPS$HS_level)



# select HS of interest 
class(filtered_AVE_SPS$HS_level)
combined_AVE_SPS <- combined_AVE_SPS %>% arrange(HS_level)

# save AVE
write.csv(combined_AVE_SPS, file = "/data/sikeme/TRADE/WTO/result/regression_HS2/Compared_AVE_H2_IV.csv")

# select HS of interest to plot 
filtered_AVE_SPS <- combined_AVE_SPS[1:20, ]



library(ggplot2)
plot_SPS <- ggplot(filtered_AVE_SPS, aes(x = factor(HS_level), y = Estimate, ymin = X2.5.., ymax = X97.5.., color = source)) +
  geom_pointrange() +
  geom_errorbar(aes(y = X2.5.., ymin = X2.5.., ymax = X2.5..), width = 0.1, size = 1) +  # Bar at the minimum
  geom_errorbar(aes(y = X97.5.., ymin = X97.5.., ymax = X97.5..), width = 0.1, size = 1) +  # Bar at the maximum
  geom_text(aes(label = round(Estimate, 2), color = source), vjust = -1, hjust = 0.5, size = 3) + 
  labs(title = "Mean Ad Valorem Equivalents of SPS at HS2 level from 2012 to 2016",
       x = "HS1 level codes",
       y = "Mean Estimate of AVE") +
  scale_color_manual(name = "Databases",  # Change legend title
                     values = c("UNCTAD" = "blue", "WTO" = "red")) +  # Specify colors
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5)) 
plot_SPS
ggsave("/data/sikeme/TRADE/WTO/result/regression_H2/AVE_graph/AVE_SPS_plot.png", plot_SPS, width = 10, height = 6, dpi = 300)





################################################################################

# if try to aggregate it by chapter 


combined_AVE_SPS <- read_csv( "/data/sikeme/TRADE/WTO/result/regression_HS2/Compared_AVE_H2_IV.csv")


names(combined_AVE_SPS)




# create a section variables for each HS chapters
table(combined_AVE_SPS$HS_level)

combined_AVE_SPS1 <- combined_AVE_SPS %>% mutate(
  HS_section = case_when(
    HS_level %in% 1:5 ~ 1,
    HS_level %in% 6:14 ~ 2,
    HS_level %in% 15 ~ 3,
    HS_level %in% 16:24 ~ 4,
    HS_level %in% 25:27 ~ 5,
    HS_level %in% 28:38 ~ 6,
    HS_level %in% 39:40 ~ 7,
    HS_level %in% 41:43 ~ 8,
    HS_level %in% 44:46 ~ 9,
    HS_level %in% 47:49 ~ 10,
    HS_level %in% 50:63 ~ 11,
    HS_level %in% 64:67 ~ 12,
    HS_level %in% 68:70 ~ 13,
    HS_level %in% 71 ~ 14,
    HS_level %in% 72:83 ~ 15,
    HS_level %in% 84:85 ~ 16,
    HS_level %in% 86:89 ~ 17,
    HS_level %in% 90:92 ~ 18,
    HS_level %in% 93 ~ 19,
    HS_level %in% 94:96 ~ 20,
    HS_level %in% 97 ~ 21
  )
)

UNCTAD <- combined_AVE_SPS1 %>% filter(source == "UNCTAD")




