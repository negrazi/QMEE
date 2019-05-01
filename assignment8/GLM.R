###################
#GLM
#Assignment8
###################
library(dplyr)
library(readr)
library(ggplot2)
library(broom)
library(dotwhisker)

## rm(list=ls())  ## BMB: don't do this, you should always
                  ## start from a clean session anyway
water <- read_csv("September2018 MeHgFieldresults.csv")
## BMB: please avoid spaces in file names
water$logpH <- log(water$pH,10) ## BMB: can also use log10()
water$logMeHg <- log(water$MeHg,10)

## BMB: it doesn't make sense to treat MeHg (or log10(MeHg))
##  as a Poisson variable -- **never** (almost never) use
## values that aren't actual discrete counts in a Poisson model
## quasi-Poisson avoids the worst problems, but is still hard to motivate

ggplot(water, aes(MeHg,pH))+
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson(link="log")))

m1 <- glm(MeHg~pH,
          family=quasipoisson(link="log"),
          data=water)

tidy(m1, exponentiate=TRUE)

#Making dot whisker plot
dwplot(m1)
summary(m1)

## BMB: this doesn't change anything since you used quasipoisson in the
## first place??
m1Q <- update(m1, family = quasipoisson(link="log"))
summary(m1Q)

##add to plot:
## BMB: again, these regression lines are identical???
## BMB: not reproducible. What is water2/where does it come from?
## (code originally had "water2" rather than "water" -- seems to
## work with "water")
ggplot(water, aes(MeHg,pH))+
  geom_point()+
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson(link="log")), fill="blue")+
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson(link="log")), fill="red")

drop1(m1Q, test="Chisq") #use chi-squared if you are using binomial or poisson, use F if you are fitting a model with a separate variance parameter (like here, with a quasi model)
predict(m1Q)
predict(m1Q, type="response") #to get predictions on response scale

#I want to predict what the MeHg would be ar 6.3
pframe <- data.frame(pH=6.3)
predict(m1Q, type="response", newdata=pframe)
plot(m1Q) 

#The outcome to the prediction is 0.11338 ng/L of MeHg if the water's pH is 6.3
#Chisqr results Model: deviance= 0.50034, p=0.01989
#dispersion paramter is 0.36434/16=0.02277

## BMB: there's a fair amount of code here that doesn't make sense:
##     did you notice ... ?

## score: 1.5
