#################
#EcoStats 708
#Assignent 3
#Lauren Negrazis 
#################

#Plotting MeHg in water samples taken from New Brunswick
#seeing if there are any patterns between the three different cathcements that have undergone three differnt forestry practices 
#NBi being most intensely forested, NBE being moderate and NBR being the minimally harvested

library(dplyr)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)


water <- read.csv ("NB-MeHg-Results-UofT2018.csv")
str(water)

#filtering out the results that were too low to be give confident results 
water2 <- water %>% filter(MeHg > 0.007)

###############
#Looking at the average mean of MeHg in each site since duplicates were taken in some sites
#############
aveMeHg <- water2 %>%
  group_by(site) %>%
  summarise(Hg.mean=mean(MeHg))
head(aveMeHg)

#adding watershed column to this data frame, I know this isn't the best option, but it works 
aveMeHg$watershed1 <-str_extract(aveMeHg$site, "NBE")
aveMeHg$watershed2 <-str_extract(aveMeHg$site, "NBR")
aveMeHg$watershed3 <-str_extract(aveMeHg$site, "NBi")

aveMeHg2 <- aveMeHg %>% mutate(watershed = coalesce(watershed1, watershed2, watershed3)) %>%
  select(watershed, site, Hg.mean)

aveMeHg2$site.no <-str_extract_all(aveMeHg2$site, "\\d")

#site.no made chars, converting them to numbers
aveMeHg2$site.no <- as.numeric(as.character(aveMeHg2$site.no))
#head(aveMeHg2) just checking they got changed 

#plotting the averages of the sites and colour coding by watershed
ggplot(aveMeHg2, aes(x=site, y=Hg.mean, colour=watershed))+
  geom_point() +
  theme_bw()+
  theme(axis.text.x=element_text(angle=90))

###############
#using facet wrap to seperate the watersheds and compare the different sites 
#################
ggplot(aveMeHg2,aes(x=site.no,y=Hg.mean))+
  geom_point()+
  labs(x="Site in Catchement",y="MeHg in water (ng/L)")+
  theme(axis.text.x=element_text(angle=90))+
  facet_wrap(~watershed)
#How can I keep the site names within the watersheds? If I set x=site then all the sites show up in each watershed

#################
#showing boxplots for the cathement overall MeHg
#this will let me see which site has the highest average MeHg, expecting it to be NBi
#################
ggplot(water2,aes(x=watershed,y=MeHg))+
  geom_boxplot()+
  geom_point(size=2, colour= "purple", alpha=0.5)+
  labs(x="Catchement",y="MeHg in water (ng/L)")+
  theme(axis.text.x=element_text(angle=90))
#this plot above is interesting, the most heavily forested site (NBi), does not contain the highest average level of MeHg

#######
#overlaying the mean MeHg for each site by catchement on one graph using lines to compare the MeHg trend as we go from headwater (1) to downstream (6)
########
#I use the values that weren't averaged so we could see more points on the plot
ggplot(water2,aes(x=site.no,y=MeHg, colour=watershed))+
  labs(x="Site along water shed",y="MeHg in water (ng/L)")+
  geom_point()+
  geom_smooth(aes(y=MeHg),se=FALSE)

#this uses the average data there are fewer points and the trends are bigger, but it spits out a lot of warning messages 
ggplot(aveMeHg2,aes(x=site.no,y=Hg.mean, colour=watershed))+
  labs(x="Site along water shed",y="MeHg in water (ng/L)")+
  geom_point()+
  geom_smooth(aes(y=Hg.mean),se=FALSE)




