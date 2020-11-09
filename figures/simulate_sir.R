# setwd("/home/julien/cloud/cours/bayesian_workflow_transmission_model/figures/")
theme_set(theme_bw())


## Setup ----
library(deSolve)
library(tidyverse)

## Set parameters ----
pars = c(beta = 0.8,
         gamma = 1/7
)

## Set initial values ----
N_0 = 100000
I_0 = 50
inits = c(
  S = N_0 - I_0,
  I = I_0,
  R = 0
)

## Set model ----
seir = function(t, x, parms, ...) {
  with(as.list(c(parms, x)), {
    dS = - beta*S*I/(S+I+R)
    dI = beta*S*I/(S+I+R) - gamma*I
    dR = gamma*I
    list(c(dS, dI, dR))
  })
}

## Simulate ----
times = seq(0,50,by=1)
sim_data = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data)

## Plot ----
ggplot(sim_data) +
  geom_line(aes(x=time,y=S),colour="chartreuse4") +
  geom_line(aes(x=time,y=I),colour="firebrick") +
  geom_line(aes(x=time,y=R),colour="steelblue") +
  labs(x="Time",y="Volume")


