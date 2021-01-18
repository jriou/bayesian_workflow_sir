setwd("/home/julien/cloud/cours/bayesian_workflow_sir/example_linear")

## Setup ----
library(rstan)
options(mc.cores = parallel::detectCores())

## Simulate data ----
N = 50
theta = 0.7
y = rnorm(N,theta,1)
input_data = list(N=N,y=y)

## Sample ----
fit = stan(file='model_linear.stan', 
           data=input_data,
           chains=4,
           iter=1000)

## Check diagnostics ----
check_hmc_diagnostics(fit) 

stan_trace(fit,pars="theta",inc_warmup=TRUE) +
  geom_hline(yintercept=theta,linetype=2)
ggsave(file="trace_theta.pdf",width=4,height=3)

stan_trace(fit,pars="theta",inc_warmup=TRUE) +
  geom_hline(yintercept=theta,linetype=2) +
  scale_x_log10()
ggsave(file="trace_theta_init.pdf",width=4,height=3)

## Show results ----
print(fit)
stan_dens(fit,pars="theta",separate_chains=TRUE) +
  geom_vline(xintercept=theta,linetype=2) 
ggsave(file="post_theta.pdf",width=4,height=3)


