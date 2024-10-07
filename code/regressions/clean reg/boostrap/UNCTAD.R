



library(boot)






rm(list = ls())
Merge_UNCTAD <- read_csv( "/data/sikeme/TRADE/WTO/data/BRA/Merge_UNACTAD_count.csv")
names(Merge_UNCTAD)








# create HS2 level var
Merge_UNCTAD$H1<- substring(Merge_UNCTAD$ProductCode   , first = 1, last = 1)
Merge_UNCTAD$H2<- substring(Merge_UNCTAD$ProductCode   , first = 1, last = 2)
Merge_UNCTAD$Year <- as.factor(Merge_UNCTAD$Year)




###############################################################################
# Test 


sub_Merge_UNCTAD <- subset(Merge_UNCTAD, H1 == "0")

# Run the regression
reg <- feols(log(price) ~ log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
               ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
               PartnerISO3,
             data = sub_Merge_UNCTAD)





regression_model <- function(data, indices) {
  sample_data <- data[indices, ]  # Sample with replacement
  model <- feols(log(price) ~ log(dist) + contig + comlang_off + rta + 
                   ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                   PartnerISO3,
                 data = sample_data)  # Fit your regression model
  return(coef(model))  # Return coefficients
}

n_iterations <- 10

# Perform bootstrapping
boot_results <- boot(sub_Merge_UNCTAD, regression_model, R = n_iterations)

print(boot_results)



###############################################################################






#re run the regression for each H2
unique_H2 <- unique(Merge_UNCTAD$H2)
# List to store regression results
regression_summaries <- list()

# Loop over each unique value of H2
for (h2_val in unique_H2) {
  # Subset the data for the current H2 value
  sub_Merge_UNCTAD <- subset(Merge_UNCTAD, H2 == h2_val)
  
  # Run the regression
  reg <- feols(log(price) ~ log(dist) + contig + comlang_off + rta + landlocked_o + landlocked_d +
                 ntm_count_SPS + ntm_count_TBT + log(1 + MFN_tariff) + Year + ProductCode + 
                 PartnerISO3,
               data = sub_Merge_UNCTAD)
  
  # Store the summary of regression
  regression_summaries[[h2_val]] <- summary(reg)
}
# Print the summaries
for (i in seq_along(unique_H2)) {
  cat("Summary for H2 =", unique_H2[i], ":\n")
  print(regression_summaries[[i]])
}
