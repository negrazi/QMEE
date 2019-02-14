########
#Testing if my data are normal
#etc..
########

#load libraries 
library(dplyr)
library(ggplot2)
library(ggfortify)
library(gtools)

#I want to change the display outside of alpha order

waterMeHg <-read.csv("NB-MeHg-Results-UofT2018.csv")

#trimming out the values we don't want (too low for an accurate read). trim values >0.10 for NBi and NBR
waterMeHg <- waterMeHg %>%
  filter(MeHg > 0.007)

#viewing data grouped by category using a box and whisker plot 
ggplot(waterMeHg, aes(x=watershed, y=MeHg))+
  geom_boxplot()+
  theme_bw()

#checking if data is normal, does not look normal  
myhist <- hist(waterMeHg$MeHg)
multiplier <- myhist$counts / myhist$density
mydensity <- density(waterMeHg$MeHg)

plot(myhist)
lines(mydensity)


#Using the Kruskal Wallis test since it's the one-way ANOVA equivalent test for nonparametric data
kwt <- kruskal.test(MeHg~watershed, data=waterMeHg)
kwt

######################
#Now starting to actually do the permutations 
#Looking at the means 
######################
set.seed(101) ## for reproducibility
nsim <- 1000
res <- numeric(nsim) ## set aside space for results
for (i in 1:nsim) {
  ## standard approach: scramble response value
  perm <- sample(nrow(waterMeHg))
  bdat <- transform(waterMeHg,MeHg=MeHg[perm])
  ## compute & store difference in means; store the value
  res[i] <- mean(bdat[bdat$watershed=="NBi","MeHg"])-
    mean(bdat[bdat$watershed=="NBR","MeHg"])-
    mean(bdat[bdat$watershed=="NBE","MeHg"])
}
obs <- mean(waterMeHg[waterMeHg$watershed=="NBi","MeHg"])-
  mean(waterMeHg[waterMeHg$watershed=="NBR","MeHg"])-
  mean(waterMeHg[waterMeHg$watershed=="NBE","MeHg"])
  
## append the observed value to the list of results
res <- c(res,obs)

#picture of results 
hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")


