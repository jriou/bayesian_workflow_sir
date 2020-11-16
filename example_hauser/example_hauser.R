setwd("/home/julien/cloud/cours/bayesian_workflow_sir/example_hauser/")

## Setup ----
library(deSolve)
library(rstan)
options(mc.cores = parallel::detectCores())
library(outbreaks)
library(tidyverse)
theme_set(theme_bw())


## Logistic ----

logfunc = function(t,tc,eta,nu,xi) eta + (1-eta)/(1+exp(xi*(t-tc-nu)))


tc = 10
eta = 0.25
nu = 3
xi = 0.5

base = tibble(t=seq(0,30,by=.1)) %>%
  mutate(f=logfunc(t,tc,eta,nu,xi))
ggplot(base) +
  geom_line(aes(x=t,y=f),colour="orange",size=.8) +
  geom_vline(xintercept=tc,linetype=2) + 
  scale_y_continuous(limits=c(0,1),labels=scales::percent) +
  labs(x="t",y="f(t)")
ggsave("example_logfunc1.pdf",width=3,height=2.3)

tc = 10
eta = 0.5
nu = 3
xi = 0.5

tibble(t=seq(0,30,by=.1)) %>%
  mutate(f=logfunc(t,tc,eta,nu,xi)) %>%
  ggplot() +
  geom_line(data=base,aes(x=t,y=f),colour="orange",size=.8) +
  geom_line(aes(x=t,y=f),colour="orange",size=.8,linetype=2) +
  geom_vline(xintercept=tc,linetype=2) + 
  scale_y_continuous(limits=c(0,1),labels=scales::percent) +
  labs(x="t",y="f(t)")
ggsave("example_logfunc2.pdf",width=3,height=2.3)

tc = 10
eta = 0.25
nu = 8
xi = 0.5

tibble(t=seq(0,30,by=.1)) %>%
  mutate(f=logfunc(t,tc,eta,nu,xi)) %>%
  ggplot() +
  geom_line(data=base,aes(x=t,y=f),colour="orange",size=.8) +
  geom_line(aes(x=t,y=f),colour="orange",size=.8,linetype=2) +
  geom_vline(xintercept=tc,linetype=2) + 
  scale_y_continuous(limits=c(0,1),labels=scales::percent) +
  labs(x="t",y="f(t)")
ggsave("example_logfunc3.pdf",width=3,height=2.3)

tc = 10
eta = 0.25
nu = 3
xi = 1.5

tibble(t=seq(0,30,by=.1)) %>%
  mutate(f=logfunc(t,tc,eta,nu,xi)) %>%
  ggplot() +
  geom_line(data=base,aes(x=t,y=f),colour="orange",size=.8) +
  geom_line(aes(x=t,y=f),colour="orange",size=.8,linetype=2) +
  geom_vline(xintercept=tc,linetype=2) + 
  scale_y_continuous(limits=c(0,1),labels=scales::percent) +
  labs(x="t",y="f(t)")
ggsave("example_logfunc4.pdf",width=3,height=2.3)


