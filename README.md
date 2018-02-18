# EINoRandomEffectsFreq
---
title: "Random"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Only seems to test the total random effect: http://rcompanion.org/handbook/G_03.html

Does not give estiamtes for the indiviudal random effects.

So we want to know the estiamtes for each individual person over time.

Seems to be testing the variance that is output in the model for the random effects

https://www.ssc.wisc.edu/sscc/pubs/MM/MM_TestEffects.html
```{r}
library(lmerTest)
library(nlme)

#lmer model with correlation between intercept and slopes
#in the random part
dat = cbind(id = rep(1:20, each = 5), time = rep(1:5, 20), outcome = rnorm(100), covar = rnorm(100))
dat = data.frame(dat)
m = lmer(outcome ~ 1 + (time | id), data = dat, REML = FALSE)
coef(m)

m1 = lme(outcome ~ 1, random = ~ time | id, data = dat)
m1["coef.lme"]
test =  coef(m1)
intervals(coef(m1))
```
Another package
```{r}
library(lme4)

# Run model with lme4 example data

fit = lmer(angle ~ recipe + temp + (1|recipe:replicate), cake)

# Model summary
summary(fit)

# lme4 profile method confidence intervals
confint(fit)

# Bootstrapped parametric p-values
boot.out = bootMer(fit, fixef, nsim=1000) #nsim determines p-value decimal places 
p = rbind(
  (1-apply(boot.out$t<0, 2, mean))*2,
  (1-apply(boot.out$t>0, 2, mean))*2)
apply(p, 2, min)

# Alternative "pipe" syntax
library(magrittr)

lmer(angle ~ recipe + temp + (1|recipe:replicate), cake) %>% 
  bootMer(fixef, nsim=100) %$% 
  rbind(
  (1-apply(t<0, 2, mean))*2,
  (1-apply(t>0, 2, mean))*2) %>% 
  apply(2, min)
```
Another way
```{r}
load("Examples.RData")
require(lme4)
# fit the model
m.sem <- lmer(Semantic.error ~ TestTime * Diagnosis + (TestTime | SubjectID),
    data = NamingRecovery, REML = FALSE)
# extract coefficients
coefs <- data.frame(coef(summary(m.sem)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs
```

