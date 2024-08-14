# Simulations for bias adjustment in Electronic Health Records

OES_EHR repository contains the code developed to account for Outcome misclassification, Exposure misclassification, and Selection bias in electronic health records (EHRs). The focus of this code is on conducting simulations to understand and adjust for these biases simultaneously via maximum likelihood approaches.

Contents

The folder "parameter distributions" contains the code for generating the sampling distributions of the misclassification and selection bias parameters. It includes plots of these distributions, created from a dataset of size 2000, with a 25% validation subset, iterated 1000 times. The exposure prevalence is fixed at 0.30. The disease mechanism is modeled as logit(P(D=1|Z)) = -2 + 0.5 Z, and the selection mechanism as logit(P(S=1|D, Z)) = -0.6 + D + Z. The true misclassification parameters are set to h_1 = g_1 = 0.85, h_4 = g_4 = 0.90, c_1 = c_2 = c_3 = c_4 = 0.90, and b_1 = b_2 = b_3 = b_4 = 0.95.

Usage

To use the simulation code, clone the repository and follow the instructions in the individual scripts. The code is designed to be flexible, allowing for modifications to the parameters of the simulation based on the specific needs of your study.

Acknowledgments

This work was developed as part of a larger effort to improve the accuracy of analyses using EHR data by attempting to address three common sources of bias.
