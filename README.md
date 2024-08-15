# Simulations for bias adjustment in Electronic Health Records

OES_EHR repository contains the code developed to account for Outcome misclassification, Exposure misclassification, and Selection bias in electronic health records (EHRs). The focus of this code is on conducting simulations to understand and adjust for these biases simultaneously via maximum likelihood approaches.

Contents
- The "code.R" file contains the code for generating a dataset of size 2000, with a 25% validation subset, iterated 1000 times. The exposure prevalence is fixed at 0.30. The disease mechanism is modeled as logit(P(D=1|Z)) = -2 + 0.5 Z, and the selection mechanism as logit(P(S=1|D, Z)) = -0.6 + D + Z. The true misclassification parameters are set to h_1 = g_1 = 0.85, h_4 = g_4 = 0.90, c_1 = c_2 = c_3 = c_4 = 0.90, and b_1 = b_2 = b_3 = b_4 = 0.95.

- The "results_table.R" file contains the code for generating a summary of results on the estimated log odds ratio of exposure, and the estimated misclassification and selection bias parameters. It also tells how many of the 1000 iterations were successful and gives a summary of the characteristics of the validation dataset.

- The "parameter distributions" folder contains the code for generating the sampling distributions of the misclassification and selection bias parameters, "bias_qua_density_plots.R". It includes plots of these distributions, created from a dataset of size 2000, with a 25% validation subset, iterated 1000 times. The exposure prevalence is fixed at 0.30. The disease mechanism is modeled as logit(P(D=1|Z)) = -2 + 0.5 Z, and the selection mechanism as logit(P(S=1|D, Z)) = -0.6 + D + Z. The true misclassification parameters are set to h_1 = g_1 = 0.85, h_4 = g_4 = 0.90, c_1 = c_2 = c_3 = c_4 = 0.90, and b_1 = b_2 = b_3 = b_4 = 0.95.

- The "other plots" folder contains the code for generating the sampling distributions of the disease prevalence, exposure prevalence, and the probability of being sampled, "other_plots.R". It also contains the code for generating the density and box plots of the sampling distributions of the log odds ratio of exposure (the disease model coefficient) for each of the different estimation methods. It includes plots of these distributions, created from a dataset of size 2000, with a 25% validation subset, iterated 1000 times. The exposure prevalence is fixed at 0.30. The disease mechanism is modeled as logit(P(D=1|Z)) = -2 + 0.5 Z, and the selection mechanism as logit(P(S=1|D, Z)) = -0.6 + D + Z. The true misclassification parameters are set to h_1 = g_1 = 0.85, h_4 = g_4 = 0.90, c_1 = c_2 = c_3 = c_4 = 0.90, and b_1 = b_2 = b_3 = b_4 = 0.95. 

- The "binding.SLURM" file is a SLURM script used to run 1000 iterations of the simulations on the Biostat cluster at the University of Michigan. You can edit this file to adjust the time limit, memory allocation, number of CPUs per task, number of iterations, input file, and output file.

- The "cluster_commands.R" file contains basic cluster interface commands that can be useful for beginners.

- The "special cases" folder contains a seperate code file for generating each of the 16 special cases. It also contains another folder called "output", which has all the output tables relating to each case. It also contains the "cases.R" file which lists all 16 possible special case scenarios. Each file generates created a dataset of size 10000, with a 25% validation subset, iterated 1000 times. The exposure prevalence is fixed at 0.30. The disease mechanism is modeled as logit(P(D=1|Z)) = -2 + 0.5 Z, and the selection mechanism as logit(P(S=1|D, Z)) = -0.6 + D + Z. The true misclassification parameters are set to h_1 = g_1 = 0.85, h_4 = g_4 = 0.90, c_1 = c_2 = c_3 = c_4 = 0.90, and b_1 = b_2 = b_3 = b_4 = 0.95.

- The "plots_across_S.R" file contains the code to plot the empirical bias and empirical variance of the estimates of different cases compared across different selection profiles.

- The "plots_across_D_and_Z.R" file contains the code to plot the empirical bias and empirical variance of the estimates of different cases compared across different misclassification profiles.

Usage

To use the simulation code, clone the repository and follow the instructions in the individual scripts. The code is designed to be flexible, allowing for modifications to the parameters of the simulation based on the specific needs of your study.

Acknowledgments

This work was developed as part of a larger effort to improve the accuracy of analyses using EHR data by attempting to address three common sources of bias.
