######
#Assignment 6
#Linear models 
######
#Hypothesis: Does forestry in NB impact MeHg levels in streamwater in watersheds with
#varying levels of forestry intensity?  
#libraries
library(readr)
library(effects)
library(emmeans)
library(dplyr)
library(ggplot2)
library(arm)
library(car)
library(stringr)

waterMeHg<-read_csv("SeptemberOctober2018MeHgFieldresults.csv")
waterMeHg$MeHgLog <- log(waterMeHg$MeHg, 10)

#arranging the sites from least harvested to most intensely harvested 
waterMeHg <- mutate(waterMeHg,
                  watershed=factor(watershed,
                              levels=c("NBR","NBE","NBi")))
print(levels(waterMeHg$watershed))

#diagnostic plot 
m1 <- lm(MeHgLog~watershed, data=waterMeHg)
plot(m1)

#inferential plot (e.g., a coefficient plot, or something from emmeans or effects)
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE)

## coeff plot 
#making factors
waterMeHg$month <- factor(waterMeHg$month)
waterMeHg$watershed <- factor(waterMeHg$watershed)

m1 = lm(MeHg ~ watershed + month, data=waterMeHg)
coefplot(m1)

#making my model comparing the average MeHg values in each watershed
pr(lm1 <- lm(MeHg~watershed-1,data=waterMeHg))

predict(lm1,newdata=data.frame(watershed=c("NBR","NBE","NBi")),
        interval="confidence")

#using emmeans
emmeans(lm1,specs=~watershed)

#graph summary
plot(allEffects(lm1))

####
#combining data frames of the MeHg data and the water chemistry data 
###
#use this to combine information from different .csv files.
waterdata <- read.csv("SeptemberOctober2018 MeHgFieldresults.csv")

aveMeHg<- data.frame(waterMeHg %>%
  group_by(site)%>%
  summarise(aveMeHg=mean(MeHg))
)

waterdata<- data.frame(waterdata %>%
                       group_by(site)%>%
                       summarise(aveo2=mean(oxygen.mgL))
)

#adding watershed column to this data frame, I know this isn't the best option, but it works 
waterdata$watershed1 <-str_extract(waterdata$site, "NBE")
waterdata$watershed2 <-str_extract(waterdata$site, "NBR")
waterdata$watershed3 <-str_extract(waterdata$site, "NBI")

waterdata2 <- waterdata %>% mutate(watershed = coalesce(watershed1, watershed2, watershed3)) %>%
  dplyr::select(watershed, site, aveo2)

MeHg <- c(aveMeHg$aveMeHg) 
site  <- c(waterdata2$site)
watershed <- c(waterdata2$watershed)
oxygen <- c(waterdata2$aveo2)

water <- data.frame(
  watershed,
  site,
  MeHg,
  oxygen
)

water <- mutate(water,
                    watershed=factor(watershed,
                                     levels=c("NBR","NBE","NBI")))
print(levels(waterMeHg$watershed))

#didn't work
pr(lm(MeHg~watershed,data=water,contrasts=list(oxygen=contr.sum)))        
