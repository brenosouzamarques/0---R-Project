library(tidyverse)
library(mixedpower)
library(lme4)
library(simr)
library(emmeans)

## CREATE VARIABLES FOR SIMULATED DATASET
## FROM A COLLAPSED CROSS-OVER TRIAL

## subjects

subj <- rep(1:30, 2)

## time

time <- rep(c("Baseline",
              "Post"), each = 30)

## treatment

treat <- c(rep("Midazolam", 15),
           rep("Esketamine", 15),
           rep("Midazolam", 15),
           rep("Esketamine", 15))

## MADRS scores

madrs <- c(rnorm(n = 30, mean = 40, sd = 2),
           rnorm(n = 15, mean = 35, sd = 1),
           rnorm(n = 15, mean = 30, sd = 1))

## Create DATASET

df <- data.frame(subj, time, treat, madrs)

## Run Mixed Effects model

md1_sim <- lme4::lmer(madrs ~ time * treat + (1 | subj),
           data = df)

emm.md1 <- emmeans(md1_sim,
                   ~ treat | time)

emm.md1
pairs(emm.md1)



#### RUN POWER ANALYSIS ####

powermodel <- mixedpower(md1_sim,
                         data = df,
                         fixed_effects = c("time",
                                           "treat"),
                         simvar = "subj",
                         steps = c(5, 10, 15, 20),
                         critical_value = 2)

mixedpower::multiplot(powermodel)


