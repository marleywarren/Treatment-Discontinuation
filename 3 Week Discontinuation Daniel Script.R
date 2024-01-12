# Set WD
# Library
library(readr)
library(tidyr)
library(rtables)
library(tern)
library(dplyr)
library(pwrss)
library(tidyverse)
library(officer)
# library(flextable)
# library(emmeans)
library(MASS)
library(StepReg)
library(sjmisc)
library(fastDummies)
library(caret)
library(janitor) #clean_names

# Import data
original_three_week_iop <- read_csv("/Users/marleywarren/Dropbox (Rush)/Tx Discontuniation Poster/3 Week IOP Discontinuation/3_week_IOP_Daniel.csv", na= "-")
dataset <- original_three_week_iop

# Set naming convention
dataset <- clean_names(dataset,"snake")

# 1. DATA CLEANING
# Rename PCL variables
dataset$pcl_baseline <- dataset$pcl_score
dataset["pcl_score"] <- NULL
dataset <- clean_names(dataset,"snake")

# Create a new column to store the most recent PCL score 
dataset <- dataset %>%
  mutate(most_recent_PCL = coalesce(pcl_day14, pcl_day13, pcl_day11, pcl_day10, pcl_day8, pcl_day6, pcl_day5,
                                    pcl_day3, pcl_day2, pcl_baseline))

# 2. EXCLUSION 
# Calculate the total number of missing values for each participant
missing_counts <- 
  dataset %>%
  group_by(mrn) %>%
  summarize(total_missing = sum(is.na(pcl_baseline), is.na(pcl_day2), is.na(pcl_day3), 
                                is.na(pcl_day5), is.na(pcl_day6), 
                                is.na(pcl_day8), is.na(pcl_day10),
                                is.na(pcl_day11), is.na(pcl_day13),
                                is.na(pcl_day14)))

# Exclude participants with 6 or more missing values into a new column
filtered_participants_1 <- missing_counts %>%
  filter(total_missing >= 10) %>%
  pull(mrn)

dataset <- dataset %>%
  anti_join(data_frame(mrn = filtered_participants_1), by = "mrn")

# Change Models -----------------------------------------------------------
## Calculate CHANGE/INCREASE/DECREASE variables
dataset$pcl_change <- dataset$pcl_baseline - dataset$most_recent_PCL
dataset$pcl_increase <- ifelse(dataset$pcl_change > 0, dataset$pcl_change, NA)
dataset$pcl_decrease <- ifelse(dataset$pcl_change < 0, dataset$pcl_change, NA)

pcl_daily_change_model <- glm(dropout ~ pcl_change, data = dataset, family = "binomial") # Significant result
summary(pcl_daily_change_model)  

change_estimates <- coef(pcl_daily_change_model)
change_odds_ratios <- exp(change_estimates)
1/change_odds_ratios




pcl_daily_increase_model <- glm(dropped_out ~ pcl_increase, data = dataset, family = "binomial") # Significant result
summary(pcl_daily_increase_model)  
change_estimates <- coef(pcl_daily_improv_model)
change_odds_ratios <- exp(change_estimates)
change_odds_ratios

pcl_daily_decrease_model <- glm(dropped_out ~ pcl_decrease, data = dataset, family = "binomial") # Significant result
summary(pcl_daily_decrease_model)  
change_estimates <- coef(pcl_daily_decrease_model)
change_odds_ratios <- exp(change_estimates)
change_odds_ratios


# Clinically SIg PCL Change -----------------------------------------------
STUCK HERE
dataset$non_completers < - dataset[dataset$dropout == 1, ]

## XXX% of (NON)COMPLETERS achieved clinically meaningful PTSD reduction (≥15 PCL points)
dataset_completers <- dataset[!(dataset$dropout == 1), ]
dataset_non_completers <- dataset[dataset$dropout == 1, ]

dataset_completers$pcl_15_reduction <- ifelse(dataset_completers$pcl_increase >= 15, dataset_completers$pcl_increase, NA)
summary(dataset_completers$pcl_15_reduction) 
(567-210)/564 
# 63% completers

dataset_non_completers$pcl_15_reduction <- ifelse(dataset_non_completers$pcl_increase >= 15, dataset_non_completers$pcl_increase, NA)
summary(dataset_non_completers$pcl_15_reduction) 
(44-38)/44 
# 14% non-completers

dataset$pcl_15_reduction <- ifelse(dataset$pcl_increase >= 15, dataset$pcl_increase, NA)
summary(dataset$pcl_15_reduction) 
(611-248)/611 
# 59% overall sample 

# Are these % different? Use Chi squared test of homogeneity
dataset$pcl_increase_binary <- ifelse(is.na(dataset$pcl_increase), 0, 1)
dataset <- dataset %>%
  mutate(across(pcl_increase_binary, as.factor))

# JUST THE TEST
result_2 <- chisq.test(table(dataset$dropped_out, dataset$pcl_increase_binary))

# TEST AND OR
# table_data <- table(dataset$dropped_out, dataset$pcl_increase_binary) #Make contingency table
table_data <- table(dataset$dropped_out, dataset$pcl_increase_binary)
odds_ratio <- fisher.test(table_data)
odds_ratio$estimate

result_2 <- chisq.test(table_data) # Perform chi-squared test
odds_ratio <- (table_data[1, 1] / table_data[1, 2]) / (table_data[2, 1] / table_data[2, 2])
cat("Chi-squared test result:\n")
print(result_2)

cat("\nOdds Ratio:\n")
print(odds_ratio)


# Print the result
print(result_2)
change_estimates_2 <- coef(result_2)
change_odds_ratios <- exp(change_estimates_2)
change_odds_ratios


## RESULTS: 
# logit(p) = -4.143720 + 0.036368*most recent
# Suggests that most_recent_PCL scores DO predict dropped_out, such that higher PCL scores lead to higher rates of drop out
# NEXT: create a factor variable with dop out time point for visualizing and anlysis purposes







