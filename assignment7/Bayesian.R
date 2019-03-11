################
#Bayseian analysis
#Fitting my data to the class examples
################

library(dplyr)
library(readr)
library(R2jags)
library(coda)
library(broom.mixed)

water <- read_csv("September2018 MeHgFieldresults.csv")

##I did this because it was close to the example code and it was how I got it to work
N <- 18
a <- water$logpH
y <- water$logMeHg
water2 <- data.frame(y,a)

#Linear model
summary(lm(y~a,data=water2))

#Bayesian analysis 
summary(bayesglm(y~a,data=water2))

jags1 <- jags(model.file="bayes2.bug",
              parameters=c("ma","int"),
              data = list('a' = a, 'N'=N, 'y'=y),
              n.chains = 4,
              inits=NULL)

bb <- jags1$BUGSoutput  
mm <- as.mcmc.bugs(bb)  
plot(jags1) 

## plot(mm)               
xyplot(mm,layout=c(1,3))  

densityplot(mm,layout=c(1,3)) 

print(dwplot(jags1))              ## estimate + credible interval plot

source("named_list.R")

#using pH data 
waterdat1 <- with(water,
                named_list(N=nrow(water),
                           pH=pH,
                           y=MeHg))


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

tidy(jags1,conf.int=TRUE, conf.method="quantile")

## model with the main effect of pH
## Getting errors because they're non-integers 
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


## compare with generalized linear models
m1 <- glm(MeHg ~ pH, data=water, family=quasipoisson)
m2 <- glm(MeHg ~ pH-1, data=water, family=quasipoisson)

