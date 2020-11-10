# setwd("/home/julien/cloud/cours/bayesian_workflow/")

## Setup ----
library(rstan)
options(mc.cores = parallel::detectCores())

## Simulate data ----
N = 50
theta = 0.7
y = rnorm(N,theta,1)
input_data = list(N=N,y=y)

## Sample ----
fit = stan(file='example_linear/model_linear.stan', 
           data=input_data,
           chains=4,
           iter=1000)
print(fit)
check_hmc_diagnostics(fit)