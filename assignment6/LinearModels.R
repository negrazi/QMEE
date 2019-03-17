######
#Assignment 6
#Linear models 
######
#Hypothesis: forestry in NB impacts MeHg in water
#libraries
library(emmeans)
library(dplyr) ## JD make sure your code runs without help; I had to add this

waterMeHg<-read.csv("NB-MeHg-Results-UofT2018.csv")
waterMeHg$MeHgLog <- log(waterMeHg$MeHg, 10)

#I took the log of the data becasue it smooths out the marginal
#distribution of the response variable.The bimofaliy from the treatment
#groups is gone an the residuals of the log-scaled data 
#I would guess that the residuals of the log-scaled data look better.

#arranging the sites from least harvested to most intensely harvested. 
waterMeHg <- mutate(waterMeHg,
                  watershed=factor(watershed,
                              levels=c("NBR","NBE","NBi")))
print(levels(waterMeHg$watershed))

#diagnostic plot, NBR being Intercept 
waterMeHg
m1 <- lm(MeHgLog~watershed, data=waterMeHg)
par(mfrow=c(2,2),mar=c(2,3,1.5,1),mgp=c(2,1,0))
plot(m1,id.n=4)

summary(m1)
##The normal Q-Q plot doesn't have any crazy tails, which it did have before the data
#was log-scaled
#the left plots seem okay, they're in 3 groups because of the 3 watersheds

#From the summary. F=21.81 and the P value is very small (<0.0001)
#the difference in mean MeHg level in NBE and NBi compared to NBi is 0.54 and 0.18, respectively.
#the P-value comparing NBR to NBi is 0.042, so a less clear relationship

#inferential plot
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)

#making my model comparing the average MeHg values in each watershed
pr(lm1 <- lm(MeHgLog~watershed,data=waterMeHg))

predict(lm1,newdata=data.frame(watershed=c("NBR","NBE","NBi")),
        interval="confidence")

#using emmeans
emmeans(lm1,specs=~watershed)

#graph summary
plot(allEffects(lm1))

