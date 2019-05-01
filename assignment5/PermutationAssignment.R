########
#Testing if my data are normal
#etc..
########

#load libraries 
library(dplyr)
library(ggplot2)

## BMB: curious what these are for (I'll see)
## (If I'm going to use only one or two functions from a package, I'll
## often specify that in a comment)
library(ggfortify)
library(gtools)

#I want to change the display outside of alpha order

waterMeHg <-read.csv("NB-MeHg-Results-UofT2018.csv")

#trimming out the values we don't want (too low for an accurate read). trim values >0.10 for NBi and NBR
waterMeHg <- waterMeHg %>%
  filter(MeHg > 0.007)
## BMB: could do this on the same step with the read.csv?

#viewing data grouped by category using a box and whisker plot 
gg0 <- ggplot(waterMeHg, aes(x=watershed, y=MeHg))+
  geom_boxplot()+
    theme_bw()
print(gg0)

## BMB: OK. I often theme_set(theme_bw()) globally
## log-scaling seems to help a lot
gg0 + scale_y_log10() + geom_point(position=position_jitter(width=0.1))

#checking if data is normal, does not look normal  
myhist <- hist(waterMeHg$MeHg)
multiplier <- myhist$counts / myhist$density
mydensity <- density(waterMeHg$MeHg)

## BMB: as discussed in class on Weds, this (i.e. the marginal
## distribution of the response variable) is *not* what we care about.
## I would guess that the residuals of the log-scaled data are pretty
## well-behaved. (The bimodality in the data comes from the different
## treatment groups.)

plot(myhist)
lines(mydensity)


#Using the Kruskal Wallis test since it's the one-way ANOVA equivalent test for nonparametric data
kwt <- kruskal.test(MeHg~watershed, data=waterMeHg)
kwt

## BMB: OK, reasonable (should give similar results to the permutation test)

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
## BMB: this summary statistic doesn't really make sense IMO.
## Dealing with three groups is admittedly a bit harder than
## two ... something like the sum of squared differences among
## the group means (or more robustly the sum of differences between
## the group medians and the overall median) would be more sensible.
## Consider: what would happen if NBR and NBE mercury levels were
## both about half the NBi mercury level???

obs <- mean(waterMeHg[waterMeHg$watershed=="NBi","MeHg"])-
  mean(waterMeHg[waterMeHg$watershed=="NBR","MeHg"])-
  mean(waterMeHg[waterMeHg$watershed=="NBE","MeHg"])
  
## append the observed value to the list of results
res <- c(res,obs)

#picture of results 
hist(res,col="gray",las=1,main="")
abline(v=obs,col="red")

## BMB: so ... what do you conclude? what's the permutation p-value?

## score: 2


