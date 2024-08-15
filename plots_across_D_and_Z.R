library(gridExtra)

custom_colors <- c("Yellow", "Orange", "Red", "Darkred")
custom_labels <- c("no misclassification", "only D misclassified", "only Z misclassified", "D and Z misclassified")

df1_5_9_13 <- rbind(case1_results, case5_results, case9_results, case13_results)
df1_5_9_13$source <- factor(df1_5_9_13$source, levels = c("1", "5", "9", "13"))
df1_5_9_13 <- df1_5_9_13[df1_5_9_13$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df1_5_9_13, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (S independent of D and Z)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df1_5_9_13, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (S independent of D and Z)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("S_ind_D_and_Z.png", combined_plots, width = 10, height = 6)

df2_6_10_14 <- rbind(case2_results, case6_results, case10_results, case14_results)
df2_6_10_14$source <- factor(df2_6_10_14$source, levels = c("2", "6", "10", "14"))
df2_6_10_14 <- df2_6_10_14[df2_6_10_14$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df2_6_10_14, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (S independent of only Z)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df2_6_10_14, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (S independent of only Z)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("S_ind_Z.png", combined_plots, width = 10, height = 6)

df3_7_11_15 <- rbind(case3_results, case7_results, case11_results, case15_results)
df3_7_11_15$source <- factor(df3_7_11_15$source, levels = c("3", "7", "11", "15"))
df3_7_11_15 <- df3_7_11_15[df3_7_11_15$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df3_7_11_15, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (S independent of only D)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df3_7_11_15, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (S independent of only D)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("S_ind_D.png", combined_plots, width = 10, height = 6)

df4_8_12_16 <- rbind(case4_results, case8_results, case12_results, case16_results)
df4_8_12_16$source <- factor(df4_8_12_16$source, levels = c("4", "8", "12", "16"))
df4_8_12_16 <- df4_8_12_16[df4_8_12_16$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df4_8_12_16, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (S dependent on D and Z)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df4_8_12_16, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (S dependent on D and Z)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Misclassification") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("S_dep_D_and_Z.png", combined_plots, width = 10, height = 6)