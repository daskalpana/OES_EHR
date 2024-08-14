library(tidyverse)
library(ggplot2)
library(dplyr)
setwd("/home/kalpanad/")

# functions to compute mean, variance, MSE without NAs and Infs
mean_without_na <- function(x) {
  x <- x[!is.na(x) & !is.infinite(x)]
  mean(x)}

var_without_na <- function(x) {
  x <- x[!is.na(x) & !is.infinite(x)]
  var(x)}

mse_without_na <- function(x) {
  x <- x[!is.na(x) & !is.infinite(x)]
  mean(x^2)}

d1 <- data.frame()
d2 <- data.frame()
d3 <- data.frame()
missing_files <- 0 

for (i in 1: 1000) {
  file_path1 <- paste0("Estimation/Outputs/Output1_case8/", i, ".csv")
  file_path2 <- paste0("Estimation/Outputs/Output2_case8/", i, ".csv")
  file_path3 <- paste0("Estimation/Outputs/Output3_case8/", i, ".csv")
  
  if (file.exists(file_path1)) {
    new1 <- read.csv(file_path1)
    new2 <- read.csv(file_path2)
    new3 <- read.csv(file_path3)
    
    d1 <- bind_rows(d1, new1)
    d2 <- bind_rows(d2, new2)
    d3 <- bind_rows(d3, new3)
    
  } else {
    missing_files <- missing_files + 1  
  }
}

missing_files

write.csv(d1, "Estimation/Outputs/results_aggregate.csv")
data1 <- read.csv("Estimation/Outputs/results_aggregate.csv")

write.csv(d2, "Estimation/Outputs/bias_qua_aggregate.csv")
data2 <- read.csv("Estimation/Outputs/bias_qua_aggregate.csv")

write.csv(d3, "Estimation/Outputs/valid_stats_aggregate.csv")
data3 <- read.csv("Estimation/Outputs/valid_stats_aggregate.csv")

Estimate <- c("Validation", "Naive", "Plug-in", "Two-step 1", "Two-step 2", "Joint")

Emprical_bias <- c(mean_without_na(data1$Bias[data1$Estimate == "Validation"]),
                   mean_without_na(data1$Bias[data1$Estimate == "Naive"]),
                   mean_without_na(data1$Bias[data1$Estimate == "Plug-in"]),
                   mean_without_na(data1$Bias[data1$Estimate == "Two-step 1"]),
                   mean_without_na(data1$Bias[data1$Estimate == "Two-step 2"]),
                   mean_without_na(data1$Bias[data1$Estimate == "Joint"]))

Emprical_var <- c(var_without_na(data1$Bias[data1$Estimate == "Validation"]),
                  var_without_na(data1$Bias[data1$Estimate == "Naive"]),
                  var_without_na(data1$Bias[data1$Estimate == "Plug-in"]),
                  var_without_na(data1$Bias[data1$Estimate == "Two-step 1"]),
                  var_without_na(data1$Bias[data1$Estimate == "Two-step 2"]),
                  var_without_na(data1$Bias[data1$Estimate == "Joint"]))

Avg_model_var <- c(mean_without_na(data1$Var[data1$Estimate == "Validation"]),
                   mean_without_na(data1$Var[data1$Estimate == "Naive"]),
                   mean_without_na(data1$Var[data1$Estimate == "Plug-in"]),
                   mean_without_na(data1$Var[data1$Estimate == "Two-step 1"]),
                   mean_without_na(data1$Var[data1$Estimate == "Two-step 2"]),
                   mean_without_na(data1$Var[data1$Estimate == "Joint"]))

Coverage <- c(mean_without_na(data1$Coverage[data1$Estimate == "Validation"]),
              mean_without_na(data1$Coverage[data1$Estimate == "Naive"]),
              mean_without_na(data1$Coverage[data1$Estimate == "Plug-in"]),
              mean_without_na(data1$Coverage[data1$Estimate == "Two-step 1"]),
              mean_without_na(data1$Coverage[data1$Estimate == "Two-step 2"]),
              mean_without_na(data1$Coverage[data1$Estimate == "Joint"]))

Empirical_MSE <- c(mse_without_na(data1$Bias[data1$Estimate == "Validation"]),
                   mse_without_na(data1$Bias[data1$Estimate == "Naive"]),
                   mse_without_na(data1$Bias[data1$Estimate == "Plug-in"]),
                   mse_without_na(data1$Bias[data1$Estimate == "Two-step 1"]),
                   mse_without_na(data1$Bias[data1$Estimate == "Two-step 2"]),
                   mse_without_na(data1$Bias[data1$Estimate == "Joint"]))

Sample_size <- c(mean_without_na(data1$Size[data1$Estimate == "Validation"]),
                          mean_without_na(data1$Size[data1$Estimate == "Naive"]),
                          mean_without_na(data1$Size[data1$Estimate == "Plug-in"]),
                          mean_without_na(data1$Size[data1$Estimate == "Two-step 1"]),
                          mean_without_na(data1$Size[data1$Estimate == "Two-step 2"]),
                          mean_without_na(data1$Size[data1$Estimate == "Joint"]))


Parameter <- c("h1", "h4", "g1", "g4", "c1", "c2", "c3", "c4", "b1", "b2", "b3", "b4", "r1", "r2", "s1", "s2")

Emp_bias1 <- sapply(Parameter, function(p) mean_without_na(data2$Bias1[data2$Parameter == p]))
Emp_bias2 <- sapply(Parameter, function(p) mean_without_na(data2$Bias2[data2$Parameter == p]))
Emp_bias3 <- sapply(Parameter, function(p) mean_without_na(data2$Bias3[data2$Parameter == p]))

Emp_var1 <- sapply(Parameter, function(p) var_without_na(data2$Bias1[data2$Parameter == p]))
Emp_var2 <- sapply(Parameter, function(p) var_without_na(data2$Bias2[data2$Parameter == p]))
Emp_var3 <- sapply(Parameter, function(p) var_without_na(data2$Bias3[data2$Parameter == p]))

Avg_mod_var1 <- sapply(Parameter, function(p) mean_without_na(data2$Var1[data2$Parameter == p]))
Avg_mod_var2 <- sapply(Parameter, function(p) mean_without_na(data2$Var2[data2$Parameter == p]))
Avg_mod_var3 <- sapply(Parameter, function(p) mean_without_na(data2$Var3[data2$Parameter == p]))

Cov1 <- sapply(Parameter, function(p) mean_without_na(data2$Coverage1[data2$Parameter == p]))
Cov2 <- sapply(Parameter, function(p) mean_without_na(data2$Coverage1[data2$Parameter == p]))
Cov3 <- sapply(Parameter, function(p) mean_without_na(data2$Coverage3[data2$Parameter == p]))

Emp_MSE1 <- sapply(Parameter, function(p) mse_without_na(data2$Bias1[data2$Parameter == p]))
Emp_MSE2 <- sapply(Parameter, function(p) mse_without_na(data2$Bias2[data2$Parameter == p]))
Emp_MSE3 <- sapply(Parameter, function(p) mse_without_na(data2$Bias3[data2$Parameter == p]))

Successful_iterations <- as.integer(1000 - missing_files)
Disease_prevalence <- round(mean_without_na(data3$dis_prev), 4)
Exposure_prevalence <- round(mean_without_na(data3$exp_prev), 4)
Sampling_prevalence <- round(mean_without_na(data3$samp_prev), 4)
Disease_sensitivity <- round(mean_without_na(data3$dis_se), 4)
Disease_specificity <- round(mean_without_na(data3$dis_sp), 4)
Exposure_sensitivity <- round(mean_without_na(data3$exp_se), 4)
Exposure_specificity <- round(mean_without_na(data3$exp_sp), 4)

results <- data.frame(Estimate, Emprical_bias, Emprical_var, Avg_model_var, Coverage, Empirical_MSE, Sample_size)
write.csv(results, file = "Estimation/Outputs/case8_results.csv", row.names = FALSE)

bias_qua <- data.frame(Parameter, Emp_bias1, Emp_bias2, Emp_bias3, Emp_var1, Emp_var2, Emp_var3, Avg_mod_var1, Avg_mod_var2, Avg_mod_var3, Cov1, Cov2, Cov3, Emp_MSE1, Emp_MSE2, Emp_MSE3)
write.csv(bias_qua, file = "Estimation/Outputs/case8_bias_qua.csv", row.names = FALSE)

valid_stats <- data.frame(
  Metric = c("Successful Iterations", "Disease Prevalence", "Exposure Prevalence", 
             "Sampling Prevalence", "Disease Sensitivity", "Disease Specificity", 
             "Exposure Sensitivity", "Exposure Specificity"),
  Value = c(paste(Successful_iterations, "/", 1000, sep = ""), Disease_prevalence, Exposure_prevalence, 
            Sampling_prevalence, Disease_sensitivity, Disease_specificity, 
            Exposure_sensitivity, Exposure_specificity)
)
write.csv(valid_stats, file = "Estimation/Outputs/case8_valid_stats.csv", row.names = FALSE)