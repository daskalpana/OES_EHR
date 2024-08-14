# Create data subsets
theta_Z_glm <- data1$Bias[data1$Estimate == "Validation"]
theta_Zstar_simple_glm <- data1$Bias[data1$Estimate == "Naive"]
theta_Z_mle1 <- data1$Bias[data1$Estimate == "Plug-in"]
theta_Z_mle2 <- data1$Bias[data1$Estimate == "Two-step 1"]
theta_Z_mle3 <- data1$Bias[data1$Estimate == "Two-step 2"]
theta_Z_mle4 <- data1$Bias[data1$Estimate == "Joint"]

# Create a combined data frame for ggplot
data_combined <- data.frame(
  Bias = c(theta_Z_glm, theta_Zstar_simple_glm, theta_Z_mle1, theta_Z_mle2, theta_Z_mle3, theta_Z_mle4),
  Estimate = factor(rep(c("Validation", "Naive", "Plug-in", "Two-step 1", "Two-step 2", "Joint"), 
                        times = c(length(theta_Z_glm), length(theta_Zstar_simple_glm), length(theta_Z_mle1), length(theta_Z_mle2), length(theta_Z_mle3), length(theta_Z_mle4))))
)

# Calculate means for vertical lines
means <- data_combined %>% group_by(Estimate) %>% summarize(mean_bias = mean(Bias))

# Create the density plot
density_plot <- ggplot(data_combined, aes(x = Bias, fill = Estimate, color = Estimate)) +
  geom_density(alpha = 0.2, size = 0.2) +
  geom_vline(data = means, aes(xintercept = mean_bias, color = Estimate), linetype = "solid", size = 0.1) +
  labs(title = "Distribution of log OR of exposure estimates",
       x = "Log OR of exposure",
       y = "Density") +
  theme_minimal()

# Create the box plot
box_plot <- ggplot(data_combined, aes(x = Estimate, y = Bias, fill = Estimate, color = Estimate)) +
  geom_boxplot(alpha = 0.2, size = 0.2) +
  geom_hline(data = means, aes(yintercept = mean_bias, color = Estimate), linetype = "solid", size = 0.1) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 1.5, color = "black") +  
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 4)), vjust = -0.5, color = "black", size = 2.5) +  
  labs(title = "Boxplot of log OR of exposure estimates",
       x = "Estimate",
       y = "Log OR of exposure") +
  theme_minimal()

ggsave("Estimation/Outputs/density_plot.png", plot = density_plot, width = 8, height = 6)
ggsave("Estimation/Outputs/box_plot.png", plot = density_plot, width = 8, height = 6)

r1 <- plogis(1.4) # P(S=1|D=1, Z=1)
r2 <- plogis(0.4) # P(S=1|D=1, Z=0)
s1 <- plogis(0.4) # P(S=1|D=0, Z=1)
s2 <- plogis(-0.6) # P(S=1|D=0, Z=0)

# distribution of prevalences

p_Z <- 0.3 # exposure prevalence
p_D <- plogis(-2 + 0.5* 1)* p_Z + plogis(-2 + 0.5* 0)* (1 - p_Z) # disease prevalence
p_S <- r1* plogis(-2 + 0.5* 1)* p_Z + r2* plogis(-2 + 0.5* 0)* (1 - p_Z) + s1* (1 - plogis(-2 + 0.5* 1))* p_Z + s2* (1 - plogis(-2 + 0.5* 0))* (1 - p_Z) # sampling prevalence

avg_exp_prev <- mean_without_na(data3$exp_prev)
avg_dis_prev <- mean_without_na(data3$dis_prev)
avg_samp_prev <- mean_without_na(data3$samp_prev)

disease_prevalence_plot <- ggplot(data3, aes(x = dis_prev)) +
  geom_density(alpha = 0.2, size = 0.5) +
  geom_vline(aes(xintercept = p_D, linetype = "True disease prevalence"), color = "black", size = 0.5, show.legend = TRUE) +
  geom_vline(aes(xintercept = avg_dis_prev, linetype = "Mean prevalence"), color = "black", size = 0.5, show.legend = TRUE) +
  labs(title = "Distribution of disease prevalence",
       x = "Disease prevalence",
       y = "Density") +
  scale_linetype_manual(name = "Legend", values = c("True prevalence" = "dashed", "Mean prevalence" = "solid")) +
  theme_minimal()

exposure_prevalence_plot <- ggplot(data3, aes(x = exp_prev)) +
  geom_density(alpha = 0.2, size = 0.5) +
  geom_vline(aes(xintercept = p_Z, linetype = "True exposure prevalence"), color = "black", size = 0.5, show.legend = TRUE) +
  geom_vline(aes(xintercept = avg_exp_prev, linetype = "Mean prevalence"), color = "black", size = 0.5, show.legend = TRUE) +
  labs(title = "Distribution of exposure prevalence",
       x = "Exposure prevalence",
       y = "Density") +
  scale_linetype_manual(name = "Legend", values = c("True prevalence" = "dashed", "Mean prevalence" = "solid")) +
  theme_minimal()

sampling_prevalence_plot <- ggplot(data3, aes(x = samp_prev)) +
  geom_density(alpha = 0.2, size = 0.5) +
  geom_vline(aes(xintercept = p_S, linetype = "True sampling prevalence"), color = "black", size = 0.5, show.legend = TRUE) +
  geom_vline(aes(xintercept = avg_samp_prev, linetype = "Mean prevalence"), color = "black", size = 0.5, show.legend = TRUE) +
  labs(title = "Distribution of sampling prevalence",
       x = "Sampling prevalence",
       y = "Density") +
  scale_linetype_manual(name = "Legend", values = c("True prevalence" = "dashed", "Mean prevalence" = "solid")) +
  theme_minimal()

ggsave("Estimation/Outputs/exposure_prevalence_plot.png", plot = exposure_prevalence_plot, width = 8, height = 6)
ggsave("Estimation/Outputs/disease_prevalence_plot.png", plot = disease_prevalence_plot, width = 8, height = 6)
ggsave("Estimation/Outputs/sampling_prevalence_plot.png", plot = sampling_prevalence_plot, width = 8, height = 6)
