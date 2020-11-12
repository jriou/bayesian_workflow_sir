setwd("/home/julien/cloud/cours/bayesian_workflow_sir/boarding_school/")

## Setup ----
library(rstan)
options(mc.cores = parallel::detectCores())
library(outbreaks)
library(tidyverse)
theme_set(theme_bw())

## See data ----
ggplot(influenza_england_1978_school) +
  geom_point(aes(x=date,y=in_bed),shape=21,size=2.3) +
  labs(x="Date",y="Number of students in bed")
ggsave(file="inbed.pdf",width=4,height=3)

## Format input ----
# prevalence data
cases = influenza_england_1978_school$in_bed
N = 763
n_days = 14
t0 = 0
t = 1:n_days

# initial conditions
i0 = 1
s0 = N - i0
r0 = 0
y0 = c(s0, i0, r0)

# put into list
input_data = list(T = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases, switch_likelihood = 0)

## Sample ----
fit = stan(file='sir_negbin.stan', 
           data=input_data,
           chains=4,
           iter=1000)

## Check diagnostics ----
check_hmc_diagnostics(fit) 

stan_trace(fit,pars=c("beta","gamma","phi"),inc_warmup=TRUE)
ggsave(file="trace_theta.pdf",width=7,height=2.5)

## Posterior predictive check ----
summary(fit,pars="pred_cases")[[1]] %>%
  as.data.frame() %>%
  bind_cols(cases=input_data$cases,
            date=input_data$ts) %>%
  ggplot() +
  geom_ribbon(aes(x=date,ymin=`2.5%`,ymax=`97.5%`),alpha=.5,fill="darkcyan") +
  geom_line(aes(x=date,y=`50%`)) +
  geom_point(aes(x=date,y=cases),shape=21,size=2.3) +
  labs(x="Date",y="Number of students in bed")
ggsave(file="inbed_fit.pdf",width=4,height=3)

## Show results ----
print(fit,pars=c("beta","gamma","phi","R0","recovery_time"))

## Prior predictive check ----
inout_data$switch_likelihood == 0
fit_prior = stan(file='sir_negbin.stan', 
                 data=input_data,
                 chains=4,
                 iter=1000)
