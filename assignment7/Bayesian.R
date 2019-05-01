################
#Bayesian analysis
#Fitting my data to the class examples
################

library(dplyr)
library(readr)
library(R2jags)
library(coda)
library(broom.mixed)

water <- read_csv("September2018 MeHgFieldresults.csv")
## BMB: NOT reproducible. you seem to have log-transformed variables?

##I did this because it was close to the example code and it was how I got it to work
N <- 18
## a <- water$logpH
## y <- water$logMeHg
## water2 <- data.frame(y,a)
water2 <- with(water,data.frame(y=log(pH), a=log(MeHg)))
## BMB: might want to consider using log10() for more interpretable
## values ...
## BMB: OK. you can rename variables within the data=list() argument
##  to jags ...

#Linear model
summary(lm(y~a,data=water2))

##Bayesian analysis
## not reproducible (no library() statement)
library(arm)
summary(bayesglm(y~a,data=water2))

jags1 <- jags(model.file="bayes2.bug",
              parameters=c("ma","int"),
              data = list('a' = a, 'N'=N, 'y'=y),
              n.chains = 4,
              inits=NULL)
## BMB: where is your bayes2.bug file? you didn't upload it to the repo
## I can *imagine* what bayes2.bug looks like, but ...

bb <- jags1$BUGSoutput  
mm <- as.mcmc.bugs(bb)  
plot(jags1) 

## plot(mm)               
xyplot(mm,layout=c(1,3))  

densityplot(mm,layout=c(1,3)) 

print(dwplot(jags1))              ## estimate + credible interval plot

## what are you conclusions/thoughts about any of these plots?

source("named_list.R")

#using pH data 
waterdat1 <- with(water,
                  named_list(N=nrow(water),
                             ## BMB: changed
                             pH,
                             MeHg))

## BMB: why switch from log to non-log scale here??
## BMB: MeHg doesn't seem to be a count response, so Poisson doesn't make sense?

## model with the main effect of pH
## parameterized by group means
pHmodel1 <- function() {
  for (i in 1:N) {
    ## Poisson model
    logmean[i] <- b_pH[pH[i]]    ## predicted log(counts)
    pred[i] <- exp(logmean[i])       ## predicted counts
    MeHg[i] ~ dpois(pred[i])    
  }
  ## define priors in a loop
  for (i in 1:pH) {
    b_pH[i] ~ dnorm(0,0.001)
  }
}

##Error here using my own data, the code for the pHmodel1 ran fine?
j1 <- jags(data=waterdat1,
           inits=NULL,
           parameters=c("b_pH"),
           model.file=pHmodel1)
## BMB: when you get an error, please copy it so we know what it is/that
## we're getting the same error

tidy(jags1,conf.int=TRUE, conf.method="quantile")

## model with the main effect of pH
## Getting errors because they're non-integers
## BMB: yup. (Although I get "Non-conforming 
pHmodel2 <- function() {
  for (i in 1:N) {
    ## Poisson model
    logmean[i] <- b_pH[1] + ifelse(pH[i]==1, 0, b_pH[pH[i]])
    pred[i] <- exp(logmean[i])       ## predicted counts
    MeHg[i] ~ dpois(pred[i])
  }
  ## define priors in a loop
  for (i in 1:pH) {
    b_pH[i] ~ dnorm(0,0.001)
  }
}
j2 <- jags(data=waterdat1,
           inits=NULL,
           parameters=c("b_pH"),
           model.file=pHmodel2)
tidy(j2, conf.int=TRUE, conf.method="quantile")


## BMB: this is making very little sense to me, and I can't get
## it to run.  Your models are set up for categorical predictors,
## but your covariate is continuous??

## compare with generalized linear models
m1 <- glm(MeHg ~ pH, data=water, family=quasipoisson)
m2 <- glm(MeHg ~ pH-1, data=water, family=quasipoisson)

## score: 1.5
