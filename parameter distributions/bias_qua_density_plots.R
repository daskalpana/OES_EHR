# Create data subsets
h1_est <- data2$Bias1[data2$Parameter == "h1"]
h4_est <- data2$Bias1[data2$Parameter == "h4"]
g1_est <- data2$Bias1[data2$Parameter == "g1"]
g4_est <- data2$Bias1[data2$Parameter == "g4"]
c1_est <- data2$Bias1[data2$Parameter == "c1"]
c2_est <- data2$Bias1[data2$Parameter == "c2"]
c3_est <- data2$Bias1[data2$Parameter == "c3"]
c4_est <- data2$Bias1[data2$Parameter == "c4"]
b1_est <- data2$Bias1[data2$Parameter == "b1"]
b2_est <- data2$Bias1[data2$Parameter == "b2"]
b3_est <- data2$Bias1[data2$Parameter == "b3"]
b4_est <- data2$Bias1[data2$Parameter == "b4"]
r1_est <- data2$Bias1[data2$Parameter == "r1"]
r2_est <- data2$Bias1[data2$Parameter == "r2"]
s1_est <- data2$Bias1[data2$Parameter == "s1"]
s2_est <- data2$Bias1[data2$Parameter == "s2"]

h1_est2 <- data2$Bias2[data2$Parameter == "h1"]
h4_est2 <- data2$Bias2[data2$Parameter == "h4"]
g1_est2 <- data2$Bias2[data2$Parameter == "g1"]
g4_est2 <- data2$Bias2[data2$Parameter == "g4"]
c1_est2 <- data2$Bias2[data2$Parameter == "c1"]
c2_est2 <- data2$Bias2[data2$Parameter == "c2"]
c3_est2 <- data2$Bias2[data2$Parameter == "c3"]
c4_est2 <- data2$Bias2[data2$Parameter == "c4"]
b1_est2 <- data2$Bias2[data2$Parameter == "b1"]
b2_est2 <- data2$Bias2[data2$Parameter == "b2"]
b3_est2 <- data2$Bias2[data2$Parameter == "b3"]
b4_est2 <- data2$Bias2[data2$Parameter == "b4"]
r1_est2 <- data2$Bias2[data2$Parameter == "r1"]
r2_est2 <- data2$Bias2[data2$Parameter == "r2"]
s1_est2 <- data2$Bias2[data2$Parameter == "s1"]
s2_est2 <- data2$Bias2[data2$Parameter == "s2"]

h1_est3 <- data2$Bias3[data2$Parameter == "h1"]
h4_est3 <- data2$Bias3[data2$Parameter == "h4"]
g1_est3 <- data2$Bias3[data2$Parameter == "g1"]
g4_est3 <- data2$Bias3[data2$Parameter == "g4"]
c1_est3 <- data2$Bias3[data2$Parameter == "c1"]
c2_est3 <- data2$Bias3[data2$Parameter == "c2"]
c3_est3 <- data2$Bias3[data2$Parameter == "c3"]
c4_est3 <- data2$Bias3[data2$Parameter == "c4"]
b1_est3 <- data2$Bias3[data2$Parameter == "b1"]
b2_est3 <- data2$Bias3[data2$Parameter == "b2"]
b3_est3 <- data2$Bias3[data2$Parameter == "b3"]
b4_est3 <- data2$Bias3[data2$Parameter == "b4"]
r1_est3 <- data2$Bias3[data2$Parameter == "r1"]
r2_est3 <- data2$Bias3[data2$Parameter == "r2"]
s1_est3 <- data2$Bias3[data2$Parameter == "s1"]
s2_est3 <- data2$Bias3[data2$Parameter == "s2"]

h1_est_density <- density(h1_est)
h1_est2_density <- density(h1_est2)
h1_est3_density <- density(h1_est3)

h1_df <- data.frame(x = h1_est_density$x, y = h1_est_density$y, Estimate = "GLM")
h1_est2_df <- data.frame(x = h1_est2_density$x, y = h1_est2_density$y, Estimate = "Validation MLE")
h1_est3_df <- data.frame(x = h1_est3_density$x, y = h1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(h1_df, h1_est2_df, h1_est3_df)

h1_est_density <- density(h1_est)
h1_est2_density <- density(h1_est2)
h1_est3_density <- density(h1_est3)

h1_df <- data.frame(x = h1_est_density$x, y = h1_est_density$y, Estimate = "GLM")
h1_est2_df <- data.frame(x = h1_est2_density$x, y = h1_est2_density$y, Estimate = "Validation MLE")
h1_est3_df <- data.frame(x = h1_est3_density$x, y = h1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(h1_df, h1_est2_df, h1_est3_df)

h1 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(h1_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(h1_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(h1_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in h1 estimates",
       x = "Bias in h1 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(h1_est), mean(h1_est2), mean(h1_est3)),
                              y = c(max(h1_est_density$y) * 0.8, max(h1_est2_density$y) * 0.8, max(h1_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(h1_est), mean(h1_est2), mean(h1_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/h1.png", plot = h1, width = 8, height = 6)

h4_est_density <- density(h4_est)
h4_est2_density <- density(h4_est2)
h4_est3_density <- density(h4_est3)

h4_df <- data.frame(x = h4_est_density$x, y = h4_est_density$y, Estimate = "GLM")
h4_est2_df <- data.frame(x = h4_est2_density$x, y = h4_est2_density$y, Estimate = "Validation MLE")
h4_est3_df <- data.frame(x = h4_est3_density$x, y = h4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(h4_df, h4_est2_df, h4_est3_df)

h4_est_density <- density(h4_est)
h4_est2_density <- density(h4_est2)
h4_est3_density <- density(h4_est3)

h4_df <- data.frame(x = h4_est_density$x, y = h4_est_density$y, Estimate = "GLM")
h4_est2_df <- data.frame(x = h4_est2_density$x, y = h4_est2_density$y, Estimate = "Validation MLE")
h4_est3_df <- data.frame(x = h4_est3_density$x, y = h4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(h4_df, h4_est2_df, h4_est3_df)

h4 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(h4_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(h4_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(h4_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in h4 estimates",
       x = "Bias in h4 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(h4_est), mean(h4_est2), mean(h4_est3)),
                              y = c(max(h4_est_density$y) * 0.8, max(h4_est2_density$y) * 0.8, max(h4_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(h4_est), mean(h4_est2), mean(h4_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/h4.png", plot = h4, width = 8, height = 6)

g1_est_density <- density(g1_est)
g1_est2_density <- density(g1_est2)
g1_est3_density <- density(g1_est3)

g1_df <- data.frame(x = g1_est_density$x, y = g1_est_density$y, Estimate = "GLM")
g1_est2_df <- data.frame(x = g1_est2_density$x, y = g1_est2_density$y, Estimate = "Validation MLE")
g1_est3_df <- data.frame(x = g1_est3_density$x, y = g1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(g1_df, g1_est2_df, g1_est3_df)

g1_est_density <- density(g1_est)
g1_est2_density <- density(g1_est2)
g1_est3_density <- density(g1_est3)

g1_df <- data.frame(x = g1_est_density$x, y = g1_est_density$y, Estimate = "GLM")
g1_est2_df <- data.frame(x = g1_est2_density$x, y = g1_est2_density$y, Estimate = "Validation MLE")
g1_est3_df <- data.frame(x = g1_est3_density$x, y = g1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(g1_df, g1_est2_df, g1_est3_df)

g1 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(g1_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(g1_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(g1_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in g1 estimates",
       x = "Bias in g1 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(g1_est), mean(g1_est2), mean(g1_est3)),
                              y = c(max(g1_est_density$y) * 0.8, max(g1_est2_density$y) * 0.8, max(g1_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(g1_est), mean(g1_est2), mean(g1_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/g1.png", plot = g1, width = 8, height = 6)

g4_est_density <- density(g4_est)
g4_est2_density <- density(g4_est2)
g4_est3_density <- density(g4_est3)

g4_df <- data.frame(x = g4_est_density$x, y = g4_est_density$y, Estimate = "GLM")
g4_est2_df <- data.frame(x = g4_est2_density$x, y = g4_est2_density$y, Estimate = "Validation MLE")
g4_est3_df <- data.frame(x = g4_est3_density$x, y = g4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(g4_df, g4_est2_df, g4_est3_df)

g4_est_density <- density(g4_est)
g4_est2_density <- density(g4_est2)
g4_est3_density <- density(g4_est3)

g4_df <- data.frame(x = g4_est_density$x, y = g4_est_density$y, Estimate = "GLM")
g4_est2_df <- data.frame(x = g4_est2_density$x, y = g4_est2_density$y, Estimate = "Validation MLE")
g4_est3_df <- data.frame(x = g4_est3_density$x, y = g4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(g4_df, g4_est2_df, g4_est3_df)

g4 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(g4_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(g4_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(g4_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in g4 estimates",
       x = "Bias in g4 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(g4_est), mean(g4_est2), mean(g4_est3)),
                              y = c(max(g4_est_density$y) * 0.8, max(g4_est2_density$y) * 0.8, max(g4_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(g4_est), mean(g4_est2), mean(g4_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/g4.png", plot = g4, width = 8, height = 6)

c1_est_density <- density(c1_est)
c1_est2_density <- density(c1_est2)
c1_est3_density <- density(c1_est3)

c1_df <- data.frame(x = c1_est_density$x, y = c1_est_density$y, Estimate = "GLM")
c1_est2_df <- data.frame(x = c1_est2_density$x, y = c1_est2_density$y, Estimate = "Validation MLE")
c1_est3_df <- data.frame(x = c1_est3_density$x, y = c1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c1_df, c1_est2_df, c1_est3_df)

c1_est_density <- density(c1_est)
c1_est2_density <- density(c1_est2)
c1_est3_density <- density(c1_est3)

c1_df <- data.frame(x = c1_est_density$x, y = c1_est_density$y, Estimate = "GLM")
c1_est2_df <- data.frame(x = c1_est2_density$x, y = c1_est2_density$y, Estimate = "Validation MLE")
c1_est3_df <- data.frame(x = c1_est3_density$x, y = c1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c1_df, c1_est2_df, c1_est3_df)

c1 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(c1_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c1_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c1_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in c1 estimates",
       x = "Bias in c1 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(c1_est), mean(c1_est2), mean(c1_est3)),
                              y = c(max(c1_est_density$y) * 0.8, max(c1_est2_density$y) * 0.8, max(c1_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(c1_est), mean(c1_est2), mean(c1_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/c1.png", plot = c1, width = 8, height = 6)

c2_est_density <- density(c2_est)
c2_est2_density <- density(c2_est2)
c2_est3_density <- density(c2_est3)

c2_df <- data.frame(x = c2_est_density$x, y = c2_est_density$y, Estimate = "GLM")
c2_est2_df <- data.frame(x = c2_est2_density$x, y = c2_est2_density$y, Estimate = "Validation MLE")
c2_est3_df <- data.frame(x = c2_est3_density$x, y = c2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c2_df, c2_est2_df, c2_est3_df)

c2_est_density <- density(c2_est)
c2_est2_density <- density(c2_est2)
c2_est3_density <- density(c2_est3)

c2_df <- data.frame(x = c2_est_density$x, y = c2_est_density$y, Estimate = "GLM")
c2_est2_df <- data.frame(x = c2_est2_density$x, y = c2_est2_density$y, Estimate = "Validation MLE")
c2_est3_df <- data.frame(x = c2_est3_density$x, y = c2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c2_df, c2_est2_df, c2_est3_df)

c2 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(c2_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c2_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c2_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in c2 estimates",
       x = "Bias in c2 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(c2_est), mean(c2_est2), mean(c2_est3)),
                              y = c(max(c2_est_density$y) * 0.8, max(c2_est2_density$y) * 0.8, max(c2_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(c2_est), mean(c2_est2), mean(c2_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/c2.png", plot = c2, width = 8, height = 6)

c3_est_density <- density(c3_est)
c3_est2_density <- density(c3_est2)
c3_est3_density <- density(c3_est3)

c3_df <- data.frame(x = c3_est_density$x, y = c3_est_density$y, Estimate = "GLM")
c3_est2_df <- data.frame(x = c3_est2_density$x, y = c3_est2_density$y, Estimate = "Validation MLE")
c3_est3_df <- data.frame(x = c3_est3_density$x, y = c3_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c3_df, c3_est2_df, c3_est3_df)

c3_est_density <- density(c3_est)
c3_est2_density <- density(c3_est2)
c3_est3_density <- density(c3_est3)

c3_df <- data.frame(x = c3_est_density$x, y = c3_est_density$y, Estimate = "GLM")
c3_est2_df <- data.frame(x = c3_est2_density$x, y = c3_est2_density$y, Estimate = "Validation MLE")
c3_est3_df <- data.frame(x = c3_est3_density$x, y = c3_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c3_df, c3_est2_df, c3_est3_df)

c3 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(c3_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c3_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c3_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in c3 estimates",
       x = "Bias in c3 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(c3_est), mean(c3_est2), mean(c3_est3)),
                              y = c(max(c3_est_density$y) * 0.8, max(c3_est2_density$y) * 0.8, max(c3_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(c3_est), mean(c3_est2), mean(c3_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/c3.png", plot = c3, width = 8, height = 6)

c4_est_density <- density(c4_est)
c4_est2_density <- density(c4_est2)
c4_est3_density <- density(c4_est3)

c4_df <- data.frame(x = c4_est_density$x, y = c4_est_density$y, Estimate = "GLM")
c4_est2_df <- data.frame(x = c4_est2_density$x, y = c4_est2_density$y, Estimate = "Validation MLE")
c4_est3_df <- data.frame(x = c4_est3_density$x, y = c4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c4_df, c4_est2_df, c4_est3_df)

c4_est_density <- density(c4_est)
c4_est2_density <- density(c4_est2)
c4_est3_density <- density(c4_est3)

c4_df <- data.frame(x = c4_est_density$x, y = c4_est_density$y, Estimate = "GLM")
c4_est2_df <- data.frame(x = c4_est2_density$x, y = c4_est2_density$y, Estimate = "Validation MLE")
c4_est3_df <- data.frame(x = c4_est3_density$x, y = c4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(c4_df, c4_est2_df, c4_est3_df)

c4 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(c4_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c4_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(c4_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in c4 estimates",
       x = "Bias in c4 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(c4_est), mean(c4_est2), mean(c4_est3)),
                              y = c(max(c4_est_density$y) * 0.8, max(c4_est2_density$y) * 0.8, max(c4_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(c4_est), mean(c4_est2), mean(c4_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/c4.png", plot = c4, width = 8, height = 6)

b1_est_density <- density(b1_est)
b1_est2_density <- density(b1_est2)
b1_est3_density <- density(b1_est3)

b1_df <- data.frame(x = b1_est_density$x, y = b1_est_density$y, Estimate = "GLM")
b1_est2_df <- data.frame(x = b1_est2_density$x, y = b1_est2_density$y, Estimate = "Validation MLE")
b1_est3_df <- data.frame(x = b1_est3_density$x, y = b1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b1_df, b1_est2_df, b1_est3_df)

b1_est_density <- density(b1_est)
b1_est2_density <- density(b1_est2)
b1_est3_density <- density(b1_est3)

b1_df <- data.frame(x = b1_est_density$x, y = b1_est_density$y, Estimate = "GLM")
b1_est2_df <- data.frame(x = b1_est2_density$x, y = b1_est2_density$y, Estimate = "Validation MLE")
b1_est3_df <- data.frame(x = b1_est3_density$x, y = b1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b1_df, b1_est2_df, b1_est3_df)

b1 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(b1_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b1_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b1_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in b1 estimates",
       x = "Bias in b1 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(b1_est), mean(b1_est2), mean(b1_est3)),
                              y = c(max(b1_est_density$y) * 0.8, max(b1_est2_density$y) * 0.8, max(b1_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(b1_est), mean(b1_est2), mean(b1_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/b1.png", plot = b1, width = 8, height = 6)

b2_est_density <- density(b2_est)
b2_est2_density <- density(b2_est2)
b2_est3_density <- density(b2_est3)

b2_df <- data.frame(x = b2_est_density$x, y = b2_est_density$y, Estimate = "GLM")
b2_est2_df <- data.frame(x = b2_est2_density$x, y = b2_est2_density$y, Estimate = "Validation MLE")
b2_est3_df <- data.frame(x = b2_est3_density$x, y = b2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b2_df, b2_est2_df, b2_est3_df)

b2_est_density <- density(b2_est)
b2_est2_density <- density(b2_est2)
b2_est3_density <- density(b2_est3)

b2_df <- data.frame(x = b2_est_density$x, y = b2_est_density$y, Estimate = "GLM")
b2_est2_df <- data.frame(x = b2_est2_density$x, y = b2_est2_density$y, Estimate = "Validation MLE")
b2_est3_df <- data.frame(x = b2_est3_density$x, y = b2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b2_df, b2_est2_df, b2_est3_df)

b2 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(b2_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b2_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b2_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in b2 estimates",
       x = "Bias in b2 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(b2_est), mean(b2_est2), mean(b2_est3)),
                              y = c(max(b2_est_density$y) * 0.8, max(b2_est2_density$y) * 0.8, max(b2_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(b2_est), mean(b2_est2), mean(b2_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/b2.png", plot = b2, width = 8, height = 6)

b3_est_density <- density(b3_est)
b3_est2_density <- density(b3_est2)
b3_est3_density <- density(b3_est3)

b3_df <- data.frame(x = b3_est_density$x, y = b3_est_density$y, Estimate = "GLM")
b3_est2_df <- data.frame(x = b3_est2_density$x, y = b3_est2_density$y, Estimate = "Validation MLE")
b3_est3_df <- data.frame(x = b3_est3_density$x, y = b3_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b3_df, b3_est2_df, b3_est3_df)

b3_est_density <- density(b3_est)
b3_est2_density <- density(b3_est2)
b3_est3_density <- density(b3_est3)

b3_df <- data.frame(x = b3_est_density$x, y = b3_est_density$y, Estimate = "GLM")
b3_est2_df <- data.frame(x = b3_est2_density$x, y = b3_est2_density$y, Estimate = "Validation MLE")
b3_est3_df <- data.frame(x = b3_est3_density$x, y = b3_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b3_df, b3_est2_df, b3_est3_df)

b3 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(b3_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b3_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b3_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in b3 estimates",
       x = "Bias in b3 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(b3_est), mean(b3_est2), mean(b3_est3)),
                              y = c(max(b3_est_density$y) * 0.8, max(b3_est2_density$y) * 0.8, max(b3_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(b3_est), mean(b3_est2), mean(b3_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/b3.png", plot = b3, width = 8, height = 6)

b4_est_density <- density(b4_est)
b4_est2_density <- density(b4_est2)
b4_est3_density <- density(b4_est3)

b4_df <- data.frame(x = b4_est_density$x, y = b4_est_density$y, Estimate = "GLM")
b4_est2_df <- data.frame(x = b4_est2_density$x, y = b4_est2_density$y, Estimate = "Validation MLE")
b4_est3_df <- data.frame(x = b4_est3_density$x, y = b4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b4_df, b4_est2_df, b4_est3_df)

b4_est_density <- density(b4_est)
b4_est2_density <- density(b4_est2)
b4_est3_density <- density(b4_est3)

b4_df <- data.frame(x = b4_est_density$x, y = b4_est_density$y, Estimate = "GLM")
b4_est2_df <- data.frame(x = b4_est2_density$x, y = b4_est2_density$y, Estimate = "Validation MLE")
b4_est3_df <- data.frame(x = b4_est3_density$x, y = b4_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(b4_df, b4_est2_df, b4_est3_df)

b4 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(b4_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b4_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(b4_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in b4 estimates",
       x = "Bias in b4 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(b4_est), mean(b4_est2), mean(b4_est3)),
                              y = c(max(b4_est_density$y) * 0.8, max(b4_est2_density$y) * 0.8, max(b4_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(b4_est), mean(b4_est2), mean(b4_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/b4.png", plot = b4, width = 8, height = 6)

r1_est_density <- density(r1_est)
r1_est2_density <- density(r1_est2)
r1_est3_density <- density(r1_est3)

r1_df <- data.frame(x = r1_est_density$x, y = r1_est_density$y, Estimate = "GLM")
r1_est2_df <- data.frame(x = r1_est2_density$x, y = r1_est2_density$y, Estimate = "Validation MLE")
r1_est3_df <- data.frame(x = r1_est3_density$x, y = r1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(r1_df, r1_est2_df, r1_est3_df)

r1_est_density <- density(r1_est)
r1_est2_density <- density(r1_est2)
r1_est3_density <- density(r1_est3)

r1_df <- data.frame(x = r1_est_density$x, y = r1_est_density$y, Estimate = "GLM")
r1_est2_df <- data.frame(x = r1_est2_density$x, y = r1_est2_density$y, Estimate = "Validation MLE")
r1_est3_df <- data.frame(x = r1_est3_density$x, y = r1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(r1_df, r1_est2_df, r1_est3_df)

r1 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(r1_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(r1_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(r1_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in r1 estimates",
       x = "Bias in r1 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(r1_est), mean(r1_est2), mean(r1_est3)),
                              y = c(max(r1_est_density$y) * 0.8, max(r1_est2_density$y) * 0.8, max(r1_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(r1_est), mean(r1_est2), mean(r1_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/r1.png", plot = r1, width = 8, height = 6)

r2_est_density <- density(r2_est)
r2_est2_density <- density(r2_est2)
r2_est3_density <- density(r2_est3)

r2_df <- data.frame(x = r2_est_density$x, y = r2_est_density$y, Estimate = "GLM")
r2_est2_df <- data.frame(x = r2_est2_density$x, y = r2_est2_density$y, Estimate = "Validation MLE")
r2_est3_df <- data.frame(x = r2_est3_density$x, y = r2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(r2_df, r2_est2_df, r2_est3_df)

r2_est_density <- density(r2_est)
r2_est2_density <- density(r2_est2)
r2_est3_density <- density(r2_est3)

r2_df <- data.frame(x = r2_est_density$x, y = r2_est_density$y, Estimate = "GLM")
r2_est2_df <- data.frame(x = r2_est2_density$x, y = r2_est2_density$y, Estimate = "Validation MLE")
r2_est3_df <- data.frame(x = r2_est3_density$x, y = r2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(r2_df, r2_est2_df, r2_est3_df)

r2 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(r2_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(r2_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(r2_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in r2 estimates",
       x = "Bias in r2 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(r2_est), mean(r2_est2), mean(r2_est3)),
                              y = c(max(r2_est_density$y) * 0.8, max(r2_est2_density$y) * 0.8, max(r2_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(r2_est), mean(r2_est2), mean(r2_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/r2.png", plot = r2, width = 8, height = 6)

s1_est_density <- density(s1_est)
s1_est2_density <- density(s1_est2)
s1_est3_density <- density(s1_est3)

s1_df <- data.frame(x = s1_est_density$x, y = s1_est_density$y, Estimate = "GLM")
s1_est2_df <- data.frame(x = s1_est2_density$x, y = s1_est2_density$y, Estimate = "Validation MLE")
s1_est3_df <- data.frame(x = s1_est3_density$x, y = s1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(s1_df, s1_est2_df, s1_est3_df)

s1_est_density <- density(s1_est)
s1_est2_density <- density(s1_est2)
s1_est3_density <- density(s1_est3)

s1_df <- data.frame(x = s1_est_density$x, y = s1_est_density$y, Estimate = "GLM")
s1_est2_df <- data.frame(x = s1_est2_density$x, y = s1_est2_density$y, Estimate = "Validation MLE")
s1_est3_df <- data.frame(x = s1_est3_density$x, y = s1_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(s1_df, s1_est2_df, s1_est3_df)

s1 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(s1_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(s1_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(s1_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in s1 estimates",
       x = "Bias in s1 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(s1_est), mean(s1_est2), mean(s1_est3)),
                              y = c(max(s1_est_density$y) * 0.8, max(s1_est2_density$y) * 0.8, max(s1_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(s1_est), mean(s1_est2), mean(s1_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/s1.png", plot = s1, width = 8, height = 6)

s2_est_density <- density(s2_est)
s2_est2_density <- density(s2_est2)
s2_est3_density <- density(s2_est3)

s2_df <- data.frame(x = s2_est_density$x, y = s2_est_density$y, Estimate = "GLM")
s2_est2_df <- data.frame(x = s2_est2_density$x, y = s2_est2_density$y, Estimate = "Validation MLE")
s2_est3_df <- data.frame(x = s2_est3_density$x, y = s2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(s2_df, s2_est2_df, s2_est3_df)

s2_est_density <- density(s2_est)
s2_est2_density <- density(s2_est2)
s2_est3_density <- density(s2_est3)

s2_df <- data.frame(x = s2_est_density$x, y = s2_est_density$y, Estimate = "GLM")
s2_est2_df <- data.frame(x = s2_est2_density$x, y = s2_est2_density$y, Estimate = "Validation MLE")
s2_est3_df <- data.frame(x = s2_est3_density$x, y = s2_est3_density$y, Estimate = "Joint MLE")
density_df <- bind_rows(s2_df, s2_est2_df, s2_est3_df)

s2 <- ggplot(density_df, aes(x = x, y = y, color = Estimate, linetype = Estimate)) +
  geom_line(size = 0.5) + 
  geom_vline(aes(xintercept = mean(s2_est)), color = "darkblue", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(s2_est2)), color = "darkgreen", linetype = "solid", size = 0.5) +
  geom_vline(aes(xintercept = mean(s2_est3)), color = "darkred", linetype = "solid", size = 0.5) +
  scale_color_manual(values = c("GLM" = "darkblue", "Validation MLE" = "darkgreen", "Joint MLE" = "darkred")) +
  scale_linetype_manual(values = c("GLM" = "solid", "Validation MLE" = "solid", "Joint MLE" = "solid")) +
  labs(title = "Distribution of bias in s2 estimates",
       x = "Bias in s2 estimates",
       y = "Density") +
  theme_minimal() +
  theme(legend.position = "top") + # Position legend at the top
  geom_text(data = data.frame(x = c(mean(s2_est), mean(s2_est2), mean(s2_est3)),
                              y = c(max(s2_est_density$y) * 0.8, max(s2_est2_density$y) * 0.8, max(s2_est3_density$y) * 0.8),
                              labels = paste(round(c(mean(s2_est), mean(s2_est2), mean(s2_est3)), 2)),
                              Estimate = c("GLM", "Validation MLE", "Joint MLE")),
            aes(x = x, y = y, label = labels, color = Estimate), size = 3.5, hjust = 0) 

ggsave("Estimation/Outputs/s2.png", plot = s2, width = 8, height = 6)
