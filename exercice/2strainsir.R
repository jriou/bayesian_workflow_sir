
## Setup ----
library(deSolve)
library(tidyverse)

## Set model ----
sir2 = function(t, x, parms, ...) {
  with(as.list(c(parms, x)), {
    dS = - beta1*S*I1/(S+I1+I2+R) - beta2*S*I2/(S+I1+I2+R)
    dI1 = beta1*S*I1/(S+I1+I2+R) - gamma*I1
    dI2 = beta2*S*I2/(S+I1+I2+R) - gamma*I2
    dR = gamma*I1 + gamma*I2
    list(c(dS, dI1, dI2, dR))
  })
}

## Set parameters ----
pars = c(beta1 = 0.2, 
         beta2 = 0.2*1.5, 
         gamma = 1/7)

## Set initial values ----
N_0 = 100000
I1_0 = 500
I2_0 = 5
inits = c(S = N_0 - I1_0 - I2_0,
          I1 = I1_0,
          I2 = I2_0,
          R = 0)

## Simulate ----
times = seq(0,200,by=1)
sim_data = ode(inits, times, sir2, pars,method="rk4")
tibble(sim_data)

## Plot ----
sim_data2 = sim_data %>%
  as.data.frame() %>%
  pivot_longer(2:5) %>%
  mutate(name=factor(name,levels=c("S","I1","I2","R"),labels=c("S(t)","I1(t)","I2(t)","R(t)")))

g1 = ggplot(sim_data2) +
  geom_line(aes(x=time,y=value,color=name),size=1) +
  scale_color_manual(values=c("chartreuse4","firebrick","orange","steelblue")) +
  labs(x="Time",y="Volume",color=NULL) +
  theme(legend.position = c(.8,.5),
        legend.background = element_blank())

g2 = sim_data %>%
  as.data.frame() %>%
  mutate(prop_variant=I2/(I1+I2)) %>%
  ggplot() +
  geom_line(aes(x=time,y=prop_variant),color="orange",size=1) +
  scale_y_continuous(labels=scales::percent,limits=c(0,1)) +
  labs(x="Time",y="Proportion of strain 2",color=NULL) +
  theme(legend.position = c(.8,.6),
        legend.background = element_blank())
cowplot::plot_grid(g1,g2)
