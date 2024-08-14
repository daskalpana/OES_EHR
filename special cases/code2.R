i <- Sys.getenv("SLURM_ARRAY_TASK_ID"); set.seed(i)

# load necessary packages
library(MASS)
library(dplyr)
library(kableExtra)
library(rootSolve)
library(nleqslv)

# selection mechanism parameters
r1 <- plogis(0.4) # P(S=1|D=1)
s1 <- plogis(-0.6) # P(S=1|D=0)

# exposure misclassification parameters
h1 <- 1 # se when D=1
g1 <- 1 # se when D=0
h4 <- 1 # sp when D=1
g4 <- 1 # sp when D=0

# outcome misclassification parameters
c1 <- 1
c2 <- 1
c3 <- 1
c4 <- 1

b1 <- 1
b2 <- 1
b3 <- 1
b4 <- 1

# function for selection mechanism
generate_selection_bias <- function(Z, D, nobs) {
  U3 <- runif(nobs)
  select <- plogis(-0.6 + D)
  S <- ifelse(select > U3, 1, 0)
  return(S)
}

# function for exposure misclassification
generate_exposure_misclassification <- function(Z, D, nobs, h1, h4, g1, g4) {
  se_Z <- rep(NA, nobs)
  sp_Z <- rep(NA, nobs)
  Zstar <- rep(NA, nobs)
  se_Z[D == 1] <- h1
  sp_Z[D == 1] <- h4
  se_Z[D == 0] <- g1
  sp_Z[D == 0] <- g4
  U1 <- runif(nobs)
  Zstar <- ifelse(Z == 1, ifelse(U1 <= se_Z, 1, 0), ifelse(U1 <= sp_Z, 0, 1))
  return(Zstar)
}

# function for disease misclassification
generate_disease_misclassification <- function(Z, D, Zstar, nobs, c1, b1, c2, b2, c3, b3, c4, b4) {
  se_D <- rep(NA, nobs)
  sp_D <- rep(NA, nobs)
  Dstar <- rep(NA, nobs)
  se_D[Z == 1 & Zstar == 1] <- c1
  sp_D[Z == 1 & Zstar == 1] <- b1
  se_D[Z == 1 & Zstar == 0] <- c2
  sp_D[Z == 1 & Zstar == 0] <- b2
  se_D[Z == 0 & Zstar == 1] <- c3
  sp_D[Z == 0 & Zstar == 1] <- b3
  se_D[Z == 0 & Zstar == 0] <- c4
  sp_D[Z == 0 & Zstar == 0] <- b4
  U2 <- runif(nobs)
  Dstar <- ifelse(D == 1, ifelse(U2 <= se_D, 1, 0), ifelse(U2 <= sp_D, 0, 1))
  return(Dstar)
}

# functions for estimate and variance of bias quantifiers
valid_mle_est <- function(a, b) {
  a / (a + b)
}

valid_mle_var <- function(a, b) {
  a * b / ((a + b)^2 * (a - b))
}

# function for calculating the 95% confidence interval
conf_int <- function(est, var) {
  z_value <- qnorm(0.975) # approximately 1.96
  lb <- est - z_value* sqrt(var)
  ub <- est + z_value* sqrt(var)
  return(c(lb, ub))
}

# function for checking the 95% coverage
coverage <- function(truth, est, var) {
  CI <- conf_int(est, var)
  return(truth >= CI[1] && truth <= CI[2])
}

# function to calculate sensitivity
calculate_sensitivity <- function(true_values, observed_values) {
  true_positives <- sum(true_values == 1 & observed_values == 1)
  total_actual_positives <- sum(true_values == 1)
  
  sensitivity <- true_positives / total_actual_positives
  return(sensitivity)
}

# function to calculate specificity
calculate_specificity <- function(true_values, observed_values) {
  true_negatives <- sum(true_values == 0 & observed_values == 0)
  total_actual_negatives <- sum(true_values == 0)
  
  specificity <- true_negatives / total_actual_negatives
  return(specificity)
}

# simulation settings
nobs <- 10000 # number of observations per data set
p_Z <- 0.3 # exposure prevalence
p_D <- plogis(-2 + 0.5* 1)* p_Z + plogis(-2 + 0.5* 0)* (1 - p_Z) # disease prevalence
theta_0 <- -2 # true intercept
theta_Z <- 0.5 # true slope

ID <- 1: nobs

# generate binary exposure status
Z <- rbinom(nobs, 1, p_Z)

# generate binary disease status based on logit(P(D = 1|Z)) = −2 + 0.5 Z
U4 <- runif(nobs)
disease <- plogis(theta_0 + theta_Z* Z)
D <- ifelse(disease > U4, 1, 0)

# generate misclassified exposure
Zstar <- generate_exposure_misclassification(Z, D, nobs, h1, h4, g1, g4)

# generate misclassified outcome
Dstar <- generate_disease_misclassification(Z, D, Zstar, nobs, c1, b1, c2, b2, c3, b3, c4, b4)

# generate sampling status
S <- generate_selection_bias(Z, D, nobs)

# data set
dataset <- data.frame(ID, Z, D, Zstar, Dstar, S)

# sampled data set
data_samp <- dataset[dataset$S == 1, ]

# create a random sample of row indices for the validation data set
valid_index <- sample(1: nobs, size = round(0.25* nobs), replace = FALSE)

# create the validation and non-validated data sets
valid_data <- dataset[valid_index, ]
nonvalid_data <- dataset[-valid_index, ]

# sampled validation data set
valid_samp <- valid_data[valid_data$S == 1, ]

# sampled non-validated data set
nonvalid_samp <- nonvalid_data[nonvalid_data$S == 1, ]

# estimated disease prevalence
p_d <- nrow(valid_data[valid_data$D == 1, ])/ nrow(valid_data)

# estimated exposure prevalence
p_z <- nrow(valid_data[valid_data$Z == 1, ])/ nrow(valid_data)

# conceptual (small sample) model
conceptual_model <- glm(D ~ Z, binomial(), data = valid_data) # logit(P(D=1|Z)) = theta_0 + theta_Z Z
theta_0_glm <- coef(summary(conceptual_model))[1]
theta_Z_glm <- coef(summary(conceptual_model))[2]
theta_0_glm_var <- (coef(summary(conceptual_model))[3])^2
theta_Z_glm_var <- (coef(summary(conceptual_model))[4])^2

# naive model
naive_model <- glm(Dstar ~ Zstar, binomial(), data = data_samp) # logit(P(D*=1|Z*, S=1)) = theta_0_simple + theta_Zstar_simple Z*
theta_0_simple_glm <- coef(summary(naive_model))[1]
theta_Zstar_simple_glm <- coef(summary(naive_model))[2]
theta_0_simple_glm_var <- (coef(summary(naive_model))[3])^2
theta_Zstar_simple_glm_var <- (coef(summary(naive_model))[4])^2

# Bias quantifier estimates - METHOD 1

# fitting saturated GLM model (16 parameters in total) for the misclassification and bias mechanisms
# estimates > variance-covariance matrix > jacobian > delta method
# estimates both misclasssification and selection mechanism parameters
# equivalent to the direct probability estimates

# estimating the exposure misclassification parameters
exp_model <- glm(Zstar ~ Z* D, binomial(), data = valid_samp) # logit(P(Z*=1|Z, D, S=1)) = a_0 + a_1 Z + a_2 D + a_3 Z D
exp_cov_matrix <- vcov(exp_model)

h1_est <- plogis(coef(summary(exp_model))[1] + coef(summary(exp_model))[2] + coef(summary(exp_model))[3] + coef(summary(exp_model))[4])
g1_est <- plogis(coef(summary(exp_model))[1] + coef(summary(exp_model))[2])
h4_est <- 1 - plogis(coef(summary(exp_model))[1] + coef(summary(exp_model))[3])
g4_est <- 1 - plogis(coef(summary(exp_model))[1])

grad_h1 <- h1_est* (1 - h1_est) * c(1, 1, 1, 1)
grad_g1 <- g1_est* (1 - g1_est) * c(1, 1, 0, 0)
grad_h4 <- h4_est* (1 - h4_est) * c(1, 0, 1, 0)
grad_g4 <- g4_est* (1 - g4_est) * c(1, 0, 0, 0)

h1_est_var <- (t(grad_h1) %*% exp_cov_matrix %*% grad_h1)
g1_est_var <- (t(grad_g1) %*% exp_cov_matrix %*% grad_g1)
h4_est_var <- (t(grad_h4) %*% exp_cov_matrix %*% grad_h4)
g4_est_var <- (t(grad_g4) %*% exp_cov_matrix %*% grad_g4)

# estimating the outcome misclassification parameters
out_model <- glm(Dstar ~ D* Z* Zstar, binomial(), data = valid_samp) # logit(P(D*=1|D, Z, Zstar, S=1)) = b_0 + b_1 D + b_2 Z + b_3 Zstar + b_4 D Z + b_5 D Zstar + b_6 Z Zstar + b_7 D Z Zstar
out_cov_matrix <- vcov(out_model)

c1_est <- plogis(coef(summary(out_model))[1] + coef(summary(out_model))[2] + coef(summary(out_model))[3] + coef(summary(out_model))[4] + coef(summary(out_model))[5] + coef(summary(out_model))[6] + coef(summary(out_model))[7] + coef(summary(out_model))[8])
c2_est <- plogis(coef(summary(out_model))[1] + coef(summary(out_model))[2] + coef(summary(out_model))[3] + coef(summary(out_model))[5])
c3_est <- plogis(coef(summary(out_model))[1] + coef(summary(out_model))[2] + coef(summary(out_model))[4] + coef(summary(out_model))[6])
c4_est <- plogis(coef(summary(out_model))[1] + coef(summary(out_model))[2])

b1_est <- 1 - plogis(coef(summary(out_model))[1] + coef(summary(out_model))[3] + coef(summary(out_model))[4] + coef(summary(out_model))[7])
b2_est <- 1 - plogis(coef(summary(out_model))[1] + coef(summary(out_model))[3])
b3_est <- 1 - plogis(coef(summary(out_model))[1] + coef(summary(out_model))[4])
b4_est <- 1 - plogis(coef(summary(out_model))[1])

grad_c1 <- c1_est* (1 - c1_est) * c(1, 1, 1, 1, 1, 1, 1, 1)
grad_c2 <- c2_est* (1 - c2_est) * c(1, 1, 1, 0, 1, 0, 0, 0)
grad_c3 <- c3_est* (1 - c3_est) * c(1, 1, 0, 1, 0, 1, 0, 0)
grad_c4 <- c4_est* (1 - c4_est) * c(1, 1, 0, 0, 0, 0, 0, 0)

grad_b1 <- b1_est* (1 - b1_est) * c(1, 0, 1, 1, 0, 0, 1, 0)
grad_b2 <- b2_est* (1 - b2_est) * c(1, 0, 1, 0, 0, 0, 0, 0)
grad_b3 <- b3_est* (1 - b3_est) * c(1, 0, 0, 1, 0, 0, 0, 0)
grad_b4 <- b4_est* (1 - b4_est) * c(1, 0, 0, 0, 0, 0, 0, 0)

c1_est_var <- (t(grad_c1) %*% out_cov_matrix %*% grad_c1)
c2_est_var <- (t(grad_c2) %*% out_cov_matrix %*% grad_c2)
c3_est_var <- (t(grad_c3) %*% out_cov_matrix %*% grad_c3)
c4_est_var <- (t(grad_c4) %*% out_cov_matrix %*% grad_c4)

b1_est_var <- (t(grad_b1) %*% out_cov_matrix %*% grad_b1)
b2_est_var <- (t(grad_b2) %*% out_cov_matrix %*% grad_b2)
b3_est_var <- (t(grad_b3) %*% out_cov_matrix %*% grad_b3)
b4_est_var <- (t(grad_b4) %*% out_cov_matrix %*% grad_b4)

# estimating the selection mechanism parameters
sel_model <- glm(S ~ D, binomial(), data = valid_data) # logit(P(S=1|Z, D)) = c_0 + c_1 D
sel_cov_matrix <- vcov(sel_model)

r1_est <- plogis(coef(summary(sel_model))[1] + coef(summary(sel_model))[2])
s1_est <- plogis(coef(summary(sel_model))[1])

grad_r1 <- r1_est* (1 - r1_est) * c(1, 1)
grad_s1 <- s1_est* (1 - s1_est) * c(1, 0)

r1_est_var <- (t(grad_r1) %*% sel_cov_matrix %*% grad_r1)
s1_est_var <- (t(grad_s1) %*% sel_cov_matrix %*% grad_s1)

# Bias quantifier estimates - METHOD 2

# maximizing the validation data likelihood 
# hand calculated MLE estimates and variances
# estimates both misclasssification and selection mechanism parameters

# estimating the misclasssification parameters
u_1 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])
u_2 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])
u_3 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])
u_4 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])

u_5 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])
u_6 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])
u_7 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])
u_8 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])

u_9  <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])
u_10 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])
u_11 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])
u_12 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])

u_13 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])
u_14 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])
u_15 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])
u_16 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])

h1_est2 <- valid_mle_est((u_1 + u_2), (u_5 + u_6))
h4_est2 <- valid_mle_est((u_13 + u_14), (u_9 + u_10))
g1_est2 <- valid_mle_est((u_3 + u_4), (u_7 + u_8))
g4_est2 <- valid_mle_est((u_15 + u_16), (u_11 + u_12))

c1_est2 <- valid_mle_est(u_1, u_2)
c2_est2 <- valid_mle_est(u_5, u_6)
c3_est2 <- valid_mle_est(u_9, u_10)
c4_est2 <- valid_mle_est(u_13, u_14)

b1_est2 <- valid_mle_est(u_4, u_3)
b2_est2 <- valid_mle_est(u_8, u_7)
b3_est2 <- valid_mle_est(u_12, u_11)
b4_est2 <- valid_mle_est(u_16, u_15)

h1_est2_var <- valid_mle_var((u_1 + u_2), (u_5 + u_6))
h4_est2_var <- valid_mle_var((u_13 + u_14), (u_9 + u_10))
g1_est2_var <- valid_mle_var((u_3 + u_4), (u_7 + u_8))
g4_est2_var <- valid_mle_var((u_15 + u_16), (u_11 + u_12))

c1_est2_var <- valid_mle_var(u_1, u_2)
c2_est2_var <- valid_mle_var(u_5, u_6)
c3_est2_var <- valid_mle_var(u_9, u_10)
c4_est2_var <- valid_mle_var(u_13, u_14)

b1_est2_var <- valid_mle_var(u_4, u_3)
b2_est2_var <- valid_mle_var(u_8, u_7)
b3_est2_var <- valid_mle_var(u_12, u_11)
b4_est2_var <- valid_mle_var(u_16, u_15)

# estimating the selection mechanism parameters
p_s <- nrow(valid_data[valid_data$S == 1, ])/nrow(valid_data)
eq_1 <- eq_2 <- eq_3 <- eq_4 <- 0

samp_prob_eq <- function(var) {
  eq_1 <<- ((u_1 + u_2 + u_5 + u_6)/ var[1]) - plogis(theta_0_glm + theta_Z_glm)* p_z* nrow(valid_samp)/ p_s
  eq_2 <<- ((u_9 + u_10 + u_13 + u_14)/ var[1]) - plogis(theta_0_glm) * (1 - p_z)* nrow(valid_samp)/ p_s
  eq_3 <<- ((u_3 + u_4 + u_7 + u_8)/ var[2]) - (1 - plogis(theta_0_glm + theta_Z_glm))* p_z* nrow(valid_samp)/ p_s
  eq_4 <<- ((u_11 + u_12 + u_15 + u_16)/ var[2]) - (1 - plogis(theta_0_glm))* (1 - p_z)* nrow(valid_samp)/ p_s
  return(c(eq_1 + eq_2, eq_3 + eq_4))
}

samp_prob <- nleqslv(c(r1_est, s1_est), samp_prob_eq, jacobian = TRUE)
samp_prob_var <- diag(solve(t(samp_prob$jac) %*% samp_prob$jac))

r1_est2 <- samp_prob$x[1]
s1_est2 <- samp_prob$x[2]

r1_est2_var <- samp_prob_var[1]
s1_est2_var <- samp_prob_var[2]

# bias quantifier values for plug-in and two-step MLE estimates
y_truth <- c(h1, h4, g1, g4, c1, c2, c3, c4, b1, b2, b3, b4, r1, s1)
y_est <- c(h1_est, h4_est, g1_est, g4_est, c1_est, c2_est, c3_est, c4_est, b1_est, b2_est, b3_est, b4_est, r1_est, s1_est)
y_est2 <- c(h1_est2, h4_est2, g1_est2, g4_est2, c1_est2, c2_est2, c3_est2, c4_est2, b1_est2, b2_est2, b3_est2, b4_est2, r1_est2, s1_est2)
y_joint <- c(r1_est, s1_est, theta_0_glm, theta_Z_glm)

# function for calculating the plug-in and two-step MLE estimates
t_1 <- t_2 <- t_3 <- t_4 <- 0
two_step_mle <- function(x, y, data) {
  f_1 <<- nrow(data[data$Dstar == 1 & data$Zstar == 1, ])
  f_2 <<- nrow(data[data$Dstar == 1 & data$Zstar == 0, ])
  f_3 <<- nrow(data[data$Dstar == 0 & data$Zstar == 1, ])
  f_4 <<- nrow(data[data$Dstar == 0 & data$Zstar == 0, ])
  
  t_1 <<- (y[5] * y[1] * y[13] * plogis(x[1] + x[2]) * p_z + y[7] * (1 - y[2]) * y[13] * plogis(x[1]) * (1 - p_z) + (1 - y[9]) * y[3] * y[14] * (1 - plogis(x[1] + x[2])) * p_z + (1 - y[11]) * (1 - y[4]) * y[14] * (1 - plogis(x[1])) * (1 - p_z)) / 
    (y[13] * p_d + y[14] * (1 - p_d))
  
  t_2 <<- (y[6] * (1 - y[1]) * y[13] * plogis(x[1] + x[2]) * p_z + y[8] * y[2] * y[13] * plogis(x[1]) * (1 - p_z) + (1 - y[10]) * (1 - y[3]) * y[14] * (1 - plogis(x[1] + x[2])) * p_z + (1 - y[12]) * y[4] * y[14] * (1 - plogis(x[1])) * (1 - p_z)) / 
    (y[13] * p_d + y[14] * (1 - p_d))
  
  t_3 <<- ((1 - y[5]) * y[1] * y[13] * plogis(x[1] + x[2]) * p_z + (1 - y[7]) * (1 - y[2]) * y[13] * plogis(x[1]) * (1 - p_z) + y[9] * y[3] * y[14] * (1 - plogis(x[1] + x[2])) * p_z + y[11] * (1 - y[4]) * y[14] * (1 - plogis(x[1])) * (1 - p_z)) / 
    (y[13] * p_d + y[14] * (1 - p_d))
  
  t_4 <<- ((1 - y[6]) * (1 - y[1]) * y[13] * plogis(x[1] + x[2]) * p_z + (1 - y[8]) * y[2] * y[13] * plogis(x[1]) * (1 - p_z) + y[10] * (1 - y[3]) * y[14] * (1 - plogis(x[1] + x[2])) * p_z + y[12] * y[4] * y[14] * (1 - plogis(x[1])) * (1 - p_z)) / 
    (y[13] * p_d + y[14] * (1 - p_d))
  
  return(- (f_1 * log(t_1) + f_2 * log(t_2) + f_3 * log(t_3) + f_4 * log(t_4)))
}

# Log OR estimates - METHOD 1

# plug-in MLE estimates
# negative of "entire" observed data log-likelihood
t_1 <- t_2 <- t_3 <- t_4 <- 0
mle1_result <- optim(c(theta_0_glm, theta_Z_glm), two_step_mle, y = y_truth, data = data_samp, hessian = TRUE)
theta_0_mle1 <- mle1_result$par[1]
theta_Z_mle1 <- mle1_result$par[2]

mle1_hessian <- diag(solve(mle1_result$hessian))
theta_0_mle1_var <- mle1_hessian[1]
theta_Z_mle1_var <- mle1_hessian[2]

# Log OR estimates - METHOD 2

# two-step MLE estimates using the first set of estimates of the bias quantifiers
# negative of the "non-validated" observed data log-likelihood
t_1 <- t_2 <- t_3 <- t_4 <- 0
mle2_result <- optim(c(theta_0_glm, theta_Z_glm), two_step_mle, y = y_est, data = nonvalid_samp, hessian = TRUE)
theta_0_mle2 <- mle2_result$par[1]
theta_Z_mle2 <- mle2_result$par[2]

mle2_hessian <- diag(solve(mle2_result$hessian))
theta_0_mle2_var <- mle2_hessian[1]
theta_Z_mle2_var <- mle2_hessian[2]

# Log OR estimates - METHOD 3

# two-step MLE estimates using the second set of estimates of the bias quantifiers
# negative of the "non-validated" observed data log-likelihood
t_1 <- t_2 <- t_3 <- t_4 <- 0
mle3_result <- optim(c(theta_0_glm, theta_Z_glm), two_step_mle, y = y_est2, data = nonvalid_samp, hessian = TRUE)
theta_0_mle3 <- mle3_result$par[1]
theta_Z_mle3 <- mle3_result$par[2]

mle3_hessian <- diag(solve(mle3_result$hessian))
theta_0_mle3_var <- mle3_hessian[1]
theta_Z_mle3_var <- mle3_hessian[2]

# Log OR estimates - METHOD 4

# joint MLE estimation
# maximizing the product of two likelihoods:
# one for the validation data and the other for observed non-validated data
# estimates both misclasssification and selection mechanism parameters 
u_1 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])
u_2 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])
u_3 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])
u_4 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 1, ])

u_5 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])
u_6 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])
u_7 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])
u_8 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 1 & valid_samp$Zstar == 0, ])

u_9  <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])
u_10 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])
u_11 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])
u_12 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 1, ])

u_13 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])
u_14 <- nrow(valid_samp[valid_samp$D == 1 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])
u_15 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 1 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])
u_16 <- nrow(valid_samp[valid_samp$D == 0 & valid_samp$Dstar == 0 & valid_samp$Z == 0 & valid_samp$Zstar == 0, ])

w_1 <<- nrow(nonvalid_samp[nonvalid_samp$Dstar == 1 & nonvalid_samp$Zstar == 1, ])
w_2 <<- nrow(nonvalid_samp[nonvalid_samp$Dstar == 1 & nonvalid_samp$Zstar == 0, ])
w_3 <<- nrow(nonvalid_samp[nonvalid_samp$Dstar == 0 & nonvalid_samp$Zstar == 1, ])
w_4 <<- nrow(nonvalid_samp[nonvalid_samp$Dstar == 0 & nonvalid_samp$Zstar == 0, ])

h1_est3 <- valid_mle_est((u_1 + u_2 + w_1 + w_3), (u_5 + u_6 + w_2 + w_4))
h4_est3 <- valid_mle_est((u_13 + u_14 + w_2 + w_4), (u_9 + u_10 + w_1 + w_3))
g1_est3 <- valid_mle_est((u_3 + u_4 + w_1 + w_3), (u_7 + u_8 + w_2 + w_4))
g4_est3 <- valid_mle_est((u_15 + u_16 + w_2 + w_4), (u_11 + u_12 + w_1 + w_3))

c1_est3 <- valid_mle_est(u_1 + w_1, u_2 + w_3)
c2_est3 <- valid_mle_est(u_5 + w_2, u_6 + w_4)
c3_est3 <- valid_mle_est(u_9 + w_1, u_10 + w_3)
c4_est3 <- valid_mle_est(u_13 + w_2, u_14 + w_4)

b1_est3 <- valid_mle_est(u_4 + w_3, u_3 + w_1)
b2_est3 <- valid_mle_est(u_8 + w_4, u_7 + w_2)
b3_est3 <- valid_mle_est(u_12 + w_3, u_11 + w_1)
b4_est3 <- valid_mle_est(u_16 + w_4, u_15 + w_2)

h1_est3_var <- valid_mle_var((u_1 + u_2 + w_1 + w_3), (u_5 + u_6 + w_2 + w_4))
h4_est3_var <- valid_mle_var((u_13 + u_14 + w_2 + w_4), (u_9 + u_10 + w_1 + w_3))
g1_est3_var <- valid_mle_var((u_3 + u_4 + w_1 + w_3), (u_7 + u_8 + w_2 + w_4))
g4_est3_var <- valid_mle_var((u_15 + u_16 + w_2 + w_4), (u_11 + u_12 + w_1 + w_3))

c1_est3_var <- valid_mle_var(u_1 + w_1, u_2 + w_3)
c2_est3_var <- valid_mle_var(u_5 + w_2, u_6 + w_4)
c3_est3_var <- valid_mle_var(u_9 + w_1, u_10 + w_3)
c4_est3_var <- valid_mle_var(u_13 + w_2, u_14 + w_4)

b1_est3_var <- valid_mle_var(u_4 + w_3, u_3 + w_1)
b2_est3_var <- valid_mle_var(u_8 + w_4, u_7 + w_2)
b3_est3_var <- valid_mle_var(u_12 + w_3, u_11 + w_1)
b4_est3_var <- valid_mle_var(u_16 + w_4, u_15 + w_2)

y_joint <- c(r1_est, s1_est, theta_0_glm, theta_Z_glm)

p <- rep(0, nrow(data_samp))

p_1 <- p_2 <- p_3 <- p_4 <- p_5 <- p_6 <- p_7 <- p_8 <- p_9 <- p_10 <- p_11 <- p_12 <- p_13 <- p_14 <- p_15 <- p_16 <- 0
q_1 <- q_2 <- q_3 <- q_4 <- 0

joint_mle <- function(x) {
  for (v in 1: nrow(data_samp)) {
    if (data_samp$ID[v] %in% valid_index) {
      
      p_1 <<- c1_est3* h1_est3* x[1]* plogis(theta_0_glm + theta_Z_glm)* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_2 <<- (1 - c1_est3)* h1_est3* x[1]* plogis(theta_0_glm + theta_Z_glm)* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_3 <<- (1 - b1_est3)* g1_est3* x[2]* (1 - plogis(theta_0_glm + theta_Z_glm))* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_4 <<- b1_est3* g1_est3* x[2]* (1 - plogis(theta_0_glm + theta_Z_glm))* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      p_5 <<- c2_est3* (1 - h1_est3)* x[1]* plogis(theta_0_glm + theta_Z_glm)* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_6 <<- (1 - c2_est3)* (1 - h1_est3)* x[1]* plogis(theta_0_glm + theta_Z_glm)* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_7 <<- (1 - b2_est3)* (1 - g1_est3)* x[2]* (1 - plogis(theta_0_glm + theta_Z_glm))* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_8 <<- b2_est3* (1 - g1_est3)* x[2]* (1 - plogis(theta_0_glm + theta_Z_glm))* p_z/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      p_9 <<- c3_est3* (1 - h4_est3)* x[1]* plogis(theta_0_glm)* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_10 <<- (1 - c3_est3)* (1 - h4_est3)* x[1]* plogis(theta_0_glm)* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_11 <<- (1 - b3_est3)* (1 - g4_est3)* x[2]* (1 - plogis(theta_0_glm))* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_12 <<- b3_est3* (1 - g4_est3)* x[2]* (1 - plogis(theta_0_glm))* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      p_13 <<- c4_est3* h4_est3* x[1]* plogis(theta_0_glm)* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_14 <<- (1 - c4_est3)* h4_est3* x[1]* plogis(theta_0_glm)* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_15 <<- (1 - b4_est3)* g4_est3* x[2]* (1 - plogis(theta_0_glm))* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      p_16 <<- b4_est3* g4_est3* x[2]* (1 - plogis(theta_0_glm))* (1 - p_z)/ 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      p[v] <<- (data_samp$D[v]* data_samp$Dstar[v]* data_samp$Z[v]* data_samp$Zstar[v]* log(p_1) + 
                  data_samp$D[v]* (1 - data_samp$Dstar[v])* data_samp$Z[v]* data_samp$Zstar[v]* log(p_2) +
                  (1 - data_samp$D[v])* data_samp$Dstar[v]* data_samp$Z[v]* data_samp$Zstar[v]* log(p_3) +
                  (1 - data_samp$D[v])* (1 - data_samp$Dstar[v])* data_samp$Z[v]* data_samp$Zstar[v]* log(p_4) +
                  
                  data_samp$D[v]* data_samp$Dstar[v]* data_samp$Z[v]* (1 - data_samp$Zstar[v])* log(p_5) + 
                  data_samp$D[v]* (1 - data_samp$Dstar[v])* data_samp$Z[v]* (1 - data_samp$Zstar[v])* log(p_6) +
                  (1 - data_samp$D[v])* data_samp$Dstar[v]* data_samp$Z[v]* (1 - data_samp$Zstar[v])* log(p_7) +
                  (1 - data_samp$D[v])* (1 - data_samp$Dstar[v])* data_samp$Z[v]* (1 - data_samp$Zstar[v])* log(p_8) +
                  
                  data_samp$D[v]* data_samp$Dstar[v]* (1 - data_samp$Z[v])* data_samp$Zstar[v]* log(p_9) + 
                  data_samp$D[v]* (1 - data_samp$Dstar[v])* (1 - data_samp$Z[v])* data_samp$Zstar[v]* log(p_10) +
                  (1 - data_samp$D[v])* data_samp$Dstar[v]* (1 - data_samp$Z[v])* data_samp$Zstar[v]* log(p_11) +
                  (1 - data_samp$D[v])* (1 - data_samp$Dstar[v])* (1 - data_samp$Z[v])* data_samp$Zstar[v]* log(p_12) +
                  
                  data_samp$D[v]* data_samp$Dstar[v]* (1 - data_samp$Z[v])* (1 - data_samp$Zstar[v])* log(p_13) + 
                  data_samp$D[v]* (1 - data_samp$Dstar[v])* (1 - data_samp$Z[v])* (1 - data_samp$Zstar[v])* log(p_14) +
                  (1 - data_samp$D[v])* data_samp$Dstar[v]* (1 - data_samp$Z[v])* (1 - data_samp$Zstar[v])* log(p_15) +
                  (1 - data_samp$D[v])* (1 - data_samp$Dstar[v])* (1 - data_samp$Z[v])* (1 - data_samp$Zstar[v])* log(p_16))
      
    } else {
      q_1 <<- (c1_est3 * h1_est3 * x[1] * plogis(x[3] + x[4]) * p_z + c3_est3 * (1 - h4_est3) * x[1] * plogis(x[3]) * (1 - p_z) + (1 - b1_est3) * g1_est3 * x[2] * (1 - plogis(x[3] + x[4])) * p_z + (1 - b3_est3) * (1 - g4_est3) * x[2] * (1 - plogis(x[3])) * (1 - p_z)) / 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      q_2 <<- (c2_est3 * (1 - h1_est3) * x[1] * plogis(x[3] + x[4]) * p_z + c4_est3 * h4_est3 * x[1] * plogis(x[3]) * (1 - p_z) + (1 - b2_est3) * (1 - g1_est3) * x[2] * (1 - plogis(x[3] + x[4])) * p_z + (1 - b4_est3) * g4_est3 * x[2] * (1 - plogis(x[3])) * (1 - p_z)) / 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      q_3 <<- ((1 - c1_est3) * h1_est3 * x[1] * plogis(x[3] + x[4]) * p_z + (1 - c3_est3) * (1 - h4_est3) * x[1] * plogis(x[3]) * (1 - p_z) + b1_est3 * g1_est3 * x[2] * (1 - plogis(x[3] + x[4])) * p_z + b3_est3 * (1 - g4_est3) * x[2] * (1 - plogis(x[3])) * (1 - p_z)) / 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      q_4 <<- ((1 - c2_est3) * (1 - h1_est3) * x[1] * plogis(x[3] + x[4]) * p_z + (1 - c4_est3) * h4_est3 * x[1] * plogis(x[3]) * (1 - p_z) + b2_est3 * (1 - g1_est3) * x[2] * (1 - plogis(x[3] + x[4])) * p_z + b4_est3 * g4_est3 * x[2] * (1 - plogis(x[3])) * (1 - p_z)) / 
        (x[1] * p_d + x[2] * (1 - p_d))
      
      p[v] <<- (data_samp$Dstar[v]* data_samp$Zstar[v]* log(q_1) + 
                  data_samp$Dstar[v]* (1 - data_samp$Zstar[v])* log(q_2) +
                  (1 - data_samp$Dstar[v])* data_samp$Zstar[v]* log(q_3) +
                  (1 - data_samp$Dstar[v])* (1 - data_samp$Zstar[v])* log(q_4))
    }
  }
  return(- sum(p))
}

mle4_result <- optim(y_joint, joint_mle, hessian = TRUE, control = list(maxit = 10000))
theta_0_mle4 <- mle4_result$par[3]
theta_Z_mle4 <- mle4_result$par[4]

mle4_hessian <- diag(solve(mle4_result$hessian))
theta_0_mle4_var <- mle4_hessian[3]
theta_Z_mle4_var <- mle4_hessian[4]

r1_est3 <- mle4_result$par[1]
s1_est3 <- mle4_result$par[2]

r1_est3_var <- mle4_hessian[1]
s1_est3_var <- mle4_hessian[2]

bias_qua <- bind_rows(
  data.frame(
    Parameter = "h1",
    Bias1 = round(mean(h1_est - h1), 4),
    Bias2 = round(mean(h1_est2 - h1), 4),
    Bias3 = round(mean(h1_est3 - h1), 4),
    Var1 = round(mean(h1_est_var), 4),
    Var2 = round(mean(h1_est2_var), 4),
    Var3 = round(mean(h1_est3_var), 4),
    Coverage1 = round(coverage(h1, h1_est, h1_est_var), 4),
    Coverage2 = round(coverage(h1, h1_est2, h1_est2_var), 4),
    Coverage3 = round(coverage(h1, h1_est3, h1_est3_var), 4)
  ),
  data.frame(
    Parameter = "h4",
    Bias1 = round(mean(h4_est - h4), 4),
    Bias2 = round(mean(h4_est2 - h4), 4),
    Bias3 = round(mean(h4_est3 - h4), 4),
    Var1 = round(mean(h4_est_var), 4),
    Var2 = round(mean(h4_est2_var), 4),
    Var3 = round(mean(h4_est3_var), 4),
    Coverage1 = round(coverage(h4, h4_est, h4_est_var), 4),
    Coverage2 = round(coverage(h4, h4_est2, h4_est2_var), 4),
    Coverage3 = round(coverage(h4, h4_est3, h4_est3_var), 4)
  ),
  data.frame(
    Parameter = "g1",
    Bias1 = round(mean(g1_est - g1), 4),
    Bias2 = round(mean(g1_est2 - g1), 4),
    Bias3 = round(mean(g1_est3 - g1), 4),
    Var1 = round(mean(g1_est_var), 4),
    Var2 = round(mean(g1_est2_var), 4),
    Var3 = round(mean(g1_est3_var), 4),
    Coverage1 = round(coverage(g1, g1_est, g1_est_var), 4),
    Coverage2 = round(coverage(g1, g1_est2, g1_est2_var), 4),
    Coverage3 = round(coverage(g1, g1_est3, g1_est3_var), 4)
  ),
  data.frame(
    Parameter = "g4",
    Bias1 = round(mean(g4_est - g4), 4),
    Bias2 = round(mean(g4_est2 - g4), 4),
    Bias3 = round(mean(g4_est3 - g4), 4),
    Var1 = round(mean(g4_est_var), 4),
    Var2 = round(mean(g4_est2_var), 4),
    Var3 = round(mean(g4_est3_var), 4),
    Coverage1 = round(coverage(g4, g4_est, g4_est_var), 4),
    Coverage2 = round(coverage(g4, g4_est2, g4_est2_var), 4),
    Coverage3 = round(coverage(g4, g4_est3, g4_est3_var), 4)
  ),
  data.frame(
    Parameter = "c1",
    Bias1 = round(mean(c1_est - c1), 4),
    Bias2 = round(mean(c1_est2 - c1), 4),
    Bias3 = round(mean(c1_est3 - c1), 4),
    Var1 = round(mean(c1_est_var), 4),
    Var2 = round(mean(c1_est2_var), 4),
    Var3 = round(mean(c1_est3_var), 4),
    Coverage1 = round(coverage(c1, c1_est, c1_est_var), 4),
    Coverage2 = round(coverage(c1, c1_est2, c1_est2_var), 4),
    Coverage3 = round(coverage(c1, c1_est3, c1_est3_var), 4)
  ),
  data.frame(
    Parameter = "c2",
    Bias1 = round(mean(c2_est - c2), 4),
    Bias2 = round(mean(c2_est2 - c2), 4),
    Bias3 = round(mean(c2_est3 - c2), 4),
    Var1 = round(mean(c2_est_var), 4),
    Var2 = round(mean(c2_est2_var), 4),
    Var3 = round(mean(c2_est3_var), 4),
    Coverage1 = round(coverage(c2, c2_est, c2_est_var), 4),
    Coverage2 = round(coverage(c2, c2_est2, c2_est2_var), 4),
    Coverage3 = round(coverage(c2, c2_est3, c2_est3_var), 4)
  ),
  data.frame(
    Parameter = "c3",
    Bias1 = round(mean(c3_est - c3), 4),
    Bias2 = round(mean(c3_est2 - c3), 4),
    Bias3 = round(mean(c3_est3 - c3), 4),
    Var1 = round(mean(c3_est_var), 4),
    Var2 = round(mean(c3_est2_var), 4),
    Var3 = round(mean(c3_est3_var), 4),
    Coverage1 = round(coverage(c3, c3_est, c3_est_var), 4),
    Coverage2 = round(coverage(c3, c3_est2, c3_est2_var), 4),
    Coverage3 = round(coverage(c3, c3_est3, c3_est3_var), 4)
  ),
  data.frame(
    Parameter = "c4",
    Bias1 = round(mean(c4_est - c4), 4),
    Bias2 = round(mean(c4_est2 - c4), 4),
    Bias3 = round(mean(c4_est3 - c4), 4),
    Var1 = round(mean(c4_est_var), 4),
    Var2 = round(mean(c4_est2_var), 4),
    Var3 = round(mean(c4_est3_var), 4),
    Coverage1 = round(coverage(c4, c4_est, c4_est_var), 4),
    Coverage2 = round(coverage(c4, c4_est2, c4_est2_var), 4),
    Coverage3 = round(coverage(c4, c4_est3, c4_est3_var), 4)
  ),
  data.frame(
    Parameter = "b1",
    Bias1 = round(mean(b1_est - b1), 4),
    Bias2 = round(mean(b1_est2 - b1), 4),
    Bias3 = round(mean(b1_est3 - b1), 4),
    Var1 = round(mean(b1_est_var), 4),
    Var2 = round(mean(b1_est2_var), 4),
    Var3 = round(mean(b1_est3_var), 4),
    Coverage1 = round(coverage(b1, b1_est, b1_est_var), 4),
    Coverage2 = round(coverage(b1, b1_est2, b1_est2_var), 4),
    Coverage3 = round(coverage(b1, b1_est3, b1_est3_var), 4)
  ),
  data.frame(
    Parameter = "b2",
    Bias1 = round(mean(b2_est - b2), 4),
    Bias2 = round(mean(b2_est2 - b2), 4),
    Bias3 = round(mean(b2_est3 - b2), 4),
    Var1 = round(mean(b2_est_var), 4),
    Var2 = round(mean(b2_est2_var), 4),
    Var3 = round(mean(b2_est3_var), 4),
    Coverage1 = round(coverage(b2, b2_est, b2_est_var), 4),
    Coverage2 = round(coverage(b2, b2_est2, b2_est2_var), 4),
    Coverage3 = round(coverage(b2, b2_est3, b2_est3_var), 4)
  ),
  data.frame(
    Parameter = "b3",
    Bias1 = round(mean(b3_est - b3), 4),
    Bias2 = round(mean(b3_est2 - b3), 4),
    Bias3 = round(mean(b3_est3 - b3), 4),
    Var1 = round(mean(b3_est_var), 4),
    Var2 = round(mean(b3_est2_var), 4),
    Var3 = round(mean(b3_est3_var), 4),
    Coverage1 = round(coverage(b3, b3_est, b3_est_var), 4),
    Coverage2 = round(coverage(b3, b3_est2, b3_est2_var), 4),
    Coverage3 = round(coverage(b3, b3_est3, b3_est3_var), 4)
  ),
  data.frame(
    Parameter = "b4",
    Bias1 = round(mean(b4_est - b4), 4),
    Bias2 = round(mean(b4_est2 - b4), 4),
    Bias3 = round(mean(b4_est3 - b4), 4),
    Var1 = round(mean(b4_est_var), 4),
    Var2 = round(mean(b4_est2_var), 4),
    Var3 = round(mean(b4_est3_var), 4),
    Coverage1 = round(coverage(b4, b4_est, b4_est_var), 4),
    Coverage2 = round(coverage(b4, b4_est2, b4_est2_var), 4),
    Coverage3 = round(coverage(b4, b4_est3, b4_est3_var), 4)
  ),
  data.frame(
    Parameter = "r1",
    Bias1 = round(mean(r1_est - r1), 4),
    Bias2 = round(mean(r1_est2 - r1), 4),
    Bias3 = round(mean(r1_est3 - r1), 4),
    Var1 = round(mean(r1_est_var), 4),
    Var2 = round(mean(r1_est2_var), 4),
    Var3 = round(mean(r1_est3_var), 4),
    Coverage1 = round(coverage(r1, r1_est, r1_est_var), 4),
    Coverage2 = round(coverage(r1, r1_est2, r1_est2_var), 4),
    Coverage3 = round(coverage(r1, r1_est3, r1_est3_var), 4)
  ),
  data.frame(
    Parameter = "s1",
    Bias1 = round(mean(s1_est - s1), 4),
    Bias2 = round(mean(s1_est2 - s1), 4),
    Bias3 = round(mean(s1_est3 - s1), 4),
    Var1 = round(mean(s1_est_var), 4),
    Var2 = round(mean(s1_est2_var), 4),
    Var3 = round(mean(s1_est3_var), 4),
    Coverage1 = round(coverage(s1, s1_est, s1_est_var), 4),
    Coverage2 = round(coverage(s1, s1_est2, s1_est2_var), 4),
    Coverage3 = round(coverage(s1, s1_est3, s1_est3_var), 4)
  )
)

results <- bind_rows(
  data.frame(
    Estimate = "Validation",
    Bias = round(theta_Z_glm - theta_Z, 4),
    Var = round(theta_Z_glm_var, 4),
    Coverage = round(coverage(theta_Z, theta_Z_glm, theta_Z_glm_var), 4),
    Size = nrow(valid_data)
  ),
  data.frame(
    Estimate = "Naive",
    Bias = round(theta_Zstar_simple_glm - theta_Z, 4),
    Var = round(theta_Zstar_simple_glm_var, 4),
    Coverage = round(coverage(theta_Z, theta_Zstar_simple_glm, theta_Zstar_simple_glm_var), 4),
    Size = nrow(data_samp)
  ),
  data.frame(
    Estimate = "Plug-in",
    Bias = round(theta_Z_mle1 - theta_Z, 4),
    Var = round(theta_Z_mle1_var, 4),
    Coverage = round(coverage(theta_Z, theta_Z_mle1, theta_Z_mle1_var), 4),
    Size = nrow(data_samp)
  ),
  data.frame(
    Estimate = "Two-step 1",
    Bias = round(theta_Z_mle2 - theta_Z, 4),
    Var = round(theta_Z_mle2_var, 4),
    Coverage = round(coverage(theta_Z, theta_Z_mle2, theta_Z_mle2_var), 4),
    Size = nrow(nonvalid_samp)
  ),
  data.frame(
    Estimate = "Two-step 2",
    Bias = round(theta_Z_mle3 - theta_Z, 4),
    Var = round(theta_Z_mle3_var, 4),
    Coverage = round(coverage(theta_Z, theta_Z_mle3, theta_Z_mle3_var), 4),
    Size = nrow(nonvalid_samp)
  ),
  data.frame(
    Estimate = "Joint",
    Bias = round(theta_Z_mle4 - theta_Z, 4),
    Var = round(theta_Z_mle4_var, 4),
    Coverage = round(coverage(theta_Z, theta_Z_mle4, theta_Z_mle4_var), 4),
    Size = nrow(data_samp)
  )
)

valid_stats <- bind_rows(
  data.frame(
    dis_prev = round(mean(p_d), 4)
  ),
  data.frame(
    exp_prev = round(mean(p_z), 4)
  ),
  data.frame(
    samp_prev = round(mean(p_s), 4)
  ),
  data.frame(
    dis_se = round(calculate_sensitivity(valid_data$D, valid_data$Dstar), 4)
  ),
  data.frame(
    dis_sp = round(calculate_specificity(valid_data$D, valid_data$Dstar), 4)
  ),
  data.frame(
    exp_se = round(calculate_sensitivity(valid_data$Z, valid_data$Zstar), 4)
  ),
  data.frame(
    exp_sp = round(calculate_specificity(valid_data$Z, valid_data$Zstar), 4)
  )
)

write.csv(results, file = paste0("Outputs/Output1_case2/", i, ".csv"), row.names = FALSE)
write.csv(bias_qua, file = paste0("Outputs/Output2_case2/", i, ".csv"), row.names = FALSE)
write.csv(valid_stats, file = paste0("Outputs/Output3_case6/", i, ".csv"), row.names = FALSE)