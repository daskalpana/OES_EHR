library(gridExtra)

custom_colors <- c("Yellow", "Orange", "Red", "Darkred")
custom_labels <- c("via neither D nor Z", "only via D", "only via Z", "via D and Z")

df1_2_3_4 <- rbind(case1_results, case2_results, case3_results, case4_results)
df1_2_3_4$source <- factor(df1_2_3_4$source, levels = c("1", "2", "3", "4"))
df1_2_3_4 <- df1_2_3_4[df1_2_3_4$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df1_2_3_4, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (no misclassification)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df1_2_3_4, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (no misclassification)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("perfect_D_perfect_Z.png", combined_plots, width = 10, height = 6)

df5_6_7_8 <- rbind(case5_results, case6_results, case7_results, case8_results)
df5_6_7_8$source <- factor(df5_6_7_8$source, levels = c("5", "6", "7", "8"))
df5_6_7_8 <- df5_6_7_8[df5_6_7_8$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df5_6_7_8, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (only D misclassified)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df5_6_7_8, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (only D misclassified)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("imperfect_D_perfect_Z.png", combined_plots, width = 10, height = 6)

df9_10_11_12 <- rbind(case9_results, case10_results, case11_results, case12_results)
df9_10_11_12$source <- factor(df9_10_11_12$source, levels = c("9", "10", "11", "12"))
df9_10_11_12 <- df9_10_11_12[df9_10_11_12$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df9_10_11_12, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (only Z misclassified)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df9_10_11_12, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (only Z misclassified)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("perfect_D_imperfect_Z.png", combined_plots, width = 10, height = 6)

df13_14_15_16 <- rbind(case13_results, case14_results, case15_results, case16_results)
df13_14_15_16$source <- factor(df13_14_15_16$source, levels = c("13", "14", "15", "16"))
df13_14_15_16 <- df13_14_15_16[df13_14_15_16$Estimate != "Two-step 1", ]

slope_bias <- ggplot(df13_14_15_16, aes(x = Estimate, y = Emprical_bias, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_bias, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical bias (D and Z misclassified)",
       x = "Estimate",
       y = "Emprical bias in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

slope_sd <- ggplot(df13_14_15_16, aes(x = Estimate, y = Emprical_var, fill = source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(Emprical_var, 2)), vjust = -0.3, size = 1.8, 
            position = position_dodge(width = 0.8)) +
  labs(title = "Emprical variance (D and Z misclassified)",
       x = "Estimate",
       y = "Emprical variance in estimated log OR of Z",
       fill = "Selection mechanism") +
  scale_fill_manual(values = custom_colors, labels = custom_labels) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_plots <- grid.arrange(slope_bias, slope_sd, ncol = 2)
ggsave("imperfect_D_imperfect_Z.png", combined_plots, width = 10, height = 6)