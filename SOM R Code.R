########### Association Between Target Depression Symptoms and Emotional Communication Dynamics ########
#                                                                                                      #
# Code included details the central manuscript analyses as well as the supplemental materials analyses #
# Code creating the figures included in the manuscript was also provided.                              #
# With further questions about the analyses published in this manuscript, please contact Jennifer Bartz#
# at jennifer.bartz@mcgill.ca.                                                                         #
#                                                                                                      #
########################################################################################################

#SOM IV. R Code for Core Analyses

# Libraries and Data Cleaning ---------------------------------------------

library(readr)
library(haven)
library(dplyr)
library(lme4)
library(lmerTest)
library(effects)
library(reghelper)


#Data Cleaning
xpresdata$p_ID <- as.factor(xpresdata$p_ID)
xpresdata$TargetID <- as.factor(xpresdata$TargetID)
xpresdata$t_Gender <- as.factor(xpresdata $t_Gender)
xpresdata$t_Age <- as.numeric(xpresdata $t_Age)
xpresdata$p_age <- as.numeric(xpresdata $p_age)
xpresdata$p_gender <- as.factor(xpresdata $p_gender)
xpresdata$Valence <- as.factor(xpresdata$Emotion)
xpresdata$t_depression_ctrZ <- scale(xpresdata $t_depression_ctr, s=T, c=T)
xpresdata$t_alexi_ctrZ <- scale(xpresdata $t_alexi_ctr, s=T, c=T)

options(scipen=99)

# Core Frequentist Models -------------------------------------------------

#Emotional Expressive Accuracy Model

mod1 <- lmer(local_ea_vid ~ 
               1 + t_Gender + t_Age + Valence*t_depression_ctrZ + t_alexi_ctrZ +
               (1|p_ID) + (1|TargetID), 
             data=xpresdata)

summary(mod1)
confint(mod1)
simple_slopes(mod1, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot"))

#Target Impression Ratings Model

mod2 <- lmer(personratings_vid ~ 
               1 + t_Gender + t_Age + Valence*t_depression_ctrZ + t_alexi_ctrZ + 
               (1|p_ID) + (1|TargetID), 
             data=xpresdata)

summary(mod2)
confint(mod2)
simple_slopes(mod2, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot")) 


# SOM Individual Impressions Ratings  -------------------------------------

##care
mod.care <- lmer(Care_vid ~ 1 + t_Gender + t_Age +  Valence*t_depression_ctrZ + t_alexi_ctrZ + (1|p_ID) + (1|TargetID), data=xpresdata)
summary(mod.care)
confint(mod.care)
simple_slopes(mod.care, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot"))

##likeability
mod.like <- lmer(Likeability_vid ~ 1 + t_Gender + t_Age +  Valence*t_depression_ctrZ + t_alexi_ctrZ + (1|p_ID) + (1|TargetID), data=xpresdata)
summary(mod.like)
confint(mod.like)
simple_slopes(mod.like, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot"))

##importance

mod.imp <- lmer(Importance_vid ~ 1 + t_Gender + t_Age +  Valence*t_depression_ctrZ + t_alexi_ctrZ + (1|p_ID) + (1|TargetID), data=xpresdata)
summary(mod.imp)
confint(mod.imp)
simple_slopes(mod.imp, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot"))

##similarity

mod.sim <- lmer(Similarity_vid ~ 1 + t_Gender + t_Age +  Valence*t_depression_ctrZ + t_alexi_ctrZ + (1|p_ID) + (1|TargetID), data=xpresdata)
summary(mod.sim)
confint(mod.sim)
simple_slopes(mod.sim, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot")) 

##IOS

mod.ios <- lmer(IOS_vid ~ 1 + t_Gender + t_Age +  Valence*t_depression_ctrZ + t_alexi_ctrZ + (1|p_ID) + (1|TargetID), data=xpresdata)
summary(mod.ios)
confint(mod.ios)
simple_slopes(mod.ios, levels=NULL, confint = TRUE, ci.width = 0.95, confint.method = c("Wald", "profile", "boot"))


# SOM Bayesian Mixed Effects Models ---------------------------------------

# Load packages
library(tidyverse)
library(skimr)
library(brms)
library(tidybayes)
library(sjstats)
library(pacman)
p_load(haven, brms)

#Emotional Expressive Accuracy Model

bayestestR::model_to_priors(brms.m0)

## Weakly informed priors with a normal distribution
weakly.priors<-
  prior(normal(0.30, 1), class = "Intercept") + 
  prior(normal(0, 2), class = "b") + 
  prior(cauchy(0, 2), class = "sd") + 
  prior(cauchy(0, 2), class = "sigma")

brms.m1 <- 
  brm(local_ea_vid ~ 
        t_Gender + t_Age + Valence+t_depression_ctrZ + t_alexi_ctrZ + 
        Valence:t_depression_ctrZ +
        (Valence:t_depression_ctrZ|p_ID) + (VidID|TargetID),
      prior = weakly.priors,
      iter = 8000, warmup= 4000, chains=4, cores=4,
      control = list(adapt_delta = 0.99), 
      seed=19730105,data = xpresdata)

print(brms.m1, digits = 4)


#Impressions Ratings Composite Model

weakly.priors.person<-
  prior(normal(0, 2), class = "Intercept") + 
  prior(normal(0, 2), class = "b") +
  prior(cauchy(0, 2), class = "sd") + 
  prior(cauchy(0, 2), class = "sigma")

brms.person.m1 <- 
  brm(personratings_vid ~
        t_Gender + t_Age + Emotion+t_depression_ctrZ + t_alexi_ctrZ + 
        Emotion:t_depression_ctrZ +
        (Emotion:t_depression_ctrZ|p_ID) + (VidID|TargetID),
      prior = weakly.priors.person,
      iter = 8000, warmup= 4000, chains=4, cores=8,
      control = list(adapt_delta = 0.99), 
      seed=19730105,data = xpresdata)

print(brms.person.m1, digits = 4)

