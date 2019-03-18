#############
#Assignment 9
#############
#'Hypothesis: testing if there is spatial variation in MeHg measurements between sites
#'within a watershed.  

library(ggplot2)
library(emmeans)
library(car) 
library(arm)
library(dplyr)
library(lme4)
library(readr)
library(cowplot)
library(stringr)

water <- read_csv("SeptemberOctober2018 MeHgFieldresults.csv")
water$logpH <- log(water$pH,10)
water$logMeHg <- log(water$MeHg.pgL,10)

#releveling the watersheds from least to most harvesting
water2 <- (water
           %>% mutate(watershed=factor(watershed,levels=c("NBR","NBE","NBI")))
           %>% mutate_if(is.character,factor)
)

#making average columns of site log MeHg and log pH
water3 <- water2 %>%
  group_by(site)%>%
  summarise(meanMeHg=mean(logMeHg),
            meanpH=mean(logpH))

#adding watershed column to this data frame, I know this isn't the best option, but it works 
water3$watershed1 <-str_extract(water3$site, "NBE")
water3$watershed2 <-str_extract(water3$site, "NBR")
water3$watershed3 <-str_extract(water3$site, "NBI")

water4 <- water3 %>% mutate(watershed = coalesce(watershed1, watershed2, watershed3)) %>%
  select(watershed, site, meanMeHg, meanpH)

water4$site.no <-str_extract_all(water4$site, "\\d")

#site.no made chars, converting them to numbers
water4$site.no <- as.numeric(as.character(water4$site.no))
#factoring
water4$watershed <- factor(water4$watershed)

#makng this plot for the later part of the analysis 
q0 <- (ggplot(water4, aes(site.no, meanMeHg, colour = watershed))
       + geom_point())  
print(q0+geom_line())

## per-group fit (fixed)
lm1 <- lmList(meanMeHg~site.no|watershed, data=water4) 

## random intercept
lm2 <- lmer(meanMeHg~site.no+(1|watershed),
            data=water4)
## random slopes
lm3 <- lmer(meanMeHg~site.no+(1+site.no|watershed), data=water4) 

#compute pred
pp0 <- expand.grid(water4,site.no=1:6,watershed=levels(water4$watershed))
pp1 <- cbind(pp0,meanMeHg=predict(lm1,newdata=pp0))
pp2 <- cbind(pp0,meanMeHg=predict(lm2,newdata=pp0))
pp3 <- cbind(pp0,meanMeHg=predict(lm3,newdata=pp0))

#plot pred
theme_set(theme_classic()+theme(legend.position="none"))
plot_grid(q0+geom_line(data=pp2)+ggtitle("random intercept"),
          q0+geom_line(data=pp3)+ggtitle("random int&slope"),
          q0+geom_line(data=pp1)+ggtitle("fixed effects"),
          nrow=1)

plot(lm3)  ## fitted vs residual
## scale-location
plot(lm3, sqrt(abs(resid(.))) ~ fitted(.),
     type=c("p","smooth"), col.line="red")

#making the model to use in emmeans
w2<- glm(meanMeHg~watershed, data=water4)

#breaking apart the inital table to do comparisons of the mean MeHg between sites within watersheds
ws <- split(water2, water2$watershed)
NBR <- ws$NBR
NBE <- ws$NBE
NBI <- ws$NBI

#making lm
NBR1<- lm(MeHg.pgL ~ site, data = NBR)
NBE1<- lm(MeHg.pgL ~ site, data = NBE)
NBI1<- lm(MeHg.pgL ~ site, data = NBI)

#comparing MeHg between watershed 
emmeans(w2, "watershed", data = water4) 
grpMeans <- emmeans(w2, "watershed", data = water4)
grpMeans
pairs(grpMeans)
plot(grpMeans, comparisons = TRUE)

#seeing if MeHg changes in sites within watershed NBR
emmeans(NBR1, "site", data = NBR) 
grpMeans1 <- emmeans(NBR1, "site", data = NBR)
grpMeans1
pairs(grpMeans1)
plot(grpMeans1, comparisons = TRUE)

#seeing if MeHg changes in sites within watershed NBE
emmeans(NBE1, "site", data = NBE) 
grpMeans2 <- emmeans(NBE1, "site", data = NBE)
grpMeans2
pairs(grpMeans2)
plot(grpMeans2, comparisons = TRUE)

#seeing if MeHg changes in sites within watershed NBI
emmeans(NBI1, "site", data = NBI) 
grpMeans3 <- emmeans(NBI1, "site", data = NBI)
grpMeans3
pairs(grpMeans3)
plot(grpMeans3, comparisons = TRUE)