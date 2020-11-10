# setwd("/home/julien/cloud/cours/bayesian_workflow/simple_sir/")

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
## Simulate ----
times = seq(0,50,by=1)
sim_data = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data)

## Plot ----
sim_data %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot(sim_data) +
  geom_line(aes(x=time,y=value,color=name),size=1) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir1.pdf",width=4,height=2.6)

## Change parameters ----
pars = c(beta = 1.1,
         gamma = 1/7
)

## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir2.pdf",width=4,height=2.6)


## Change parameters ----
pars = c(beta = 0.6,
         gamma = 1/7
)

## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir3.pdf",width=4,height=2.6)

## Change parameters ----
pars = c(beta = 0.8,
         gamma = 1/14
)

## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir4.pdf",width=4,height=2.6)

## Change parameters ----
pars = c(beta = 0.8,
         gamma = 1/4
)

## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir5.pdf",width=4,height=2.6)


## Change parameters ----
pars = c(beta = 0.8,
         gamma = 1/7
)
## Set initial values ----
N_0 = 100000
I_0 = 500
inits = c(
  S = N_0 - I_0,
  I = I_0,
  R = 0
)
## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir6.pdf",width=4,height=2.6)

## Change parameters ----
pars = c(beta = 0.8,
         gamma = 1/7
)
## Set initial values ----
N_0 = 100000
I_0 = 5
inits = c(
  S = N_0 - I_0,
  I = I_0,
  R = 0
)
## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir7.pdf",width=4,height=2.6)

## Change parameters ----
pars = c(beta = 0.8,
         gamma = 1/7
)
## Set initial values ----
N_0 = 100000
I_0 = 50
R_0 = N_0 *.2
inits = c(
  S = N_0 - I_0 - R_0,
  I = I_0,
  R = R_0
)
## Simulate ----
sim_data2 = ode(inits, times, seir, pars,method="rk4")
tibble(sim_data2)

## Plot ----
sim_data2 %<>%
  as.data.frame() %>%
  pivot_longer(2:4) %>%
  mutate(name=factor(name,levels=c("S","I","R"),labels=c("S(t)","I(t)","R(t)")))
ggplot() +
  geom_line(data=sim_data,aes(x=time,y=value,color=name),size=1) +
  geom_line(data=sim_data2,aes(x=time,y=value,color=name),size=1,linetype=2) +
  scale_color_manual(values=c("chartreuse4","firebrick","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
ggsave(file="example_sir8.pdf",width=4,height=2.6)
