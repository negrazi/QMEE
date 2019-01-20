#################
#Assignment 2
#2017 fish isotope data
#################
library(dplyr)
library(readr)
library(stringr)

fish <- read.csv("sculpinisotopedata2017.csv")
fish

#I want to make a column for the sites based off the Sample.ID.ID, use somehow greater or equal to NBE1
#for each occurence of NBE1.. fill site with NBE1 etc. 


#number of rows in Sample.ID.id for fish
x <- nrow(fish)
x

#basic table with just the Sample.ID ID from the data frame to use a small data set while trouble shooting 
fish2<-data.frame(number=1:x, Sample.ID= (fish$Sample.ID.ID), stringsAsFactors = F)
              #rows^      column population^

#need to change the one header name 
fish2 <- fish

#got only NBE1! YAY, can I make it find many things to extract of a vector? 
fish2$site1 <-str_extract(fish2$Sample.ID, "NBE1")
fish2$site2 <-str_extract(fish2$Sample.ID, "NBE2")
fish2$site3 <-str_extract(fish2$Sample.ID, "NBE3")
fish2$site4 <-str_extract(fish2$Sample.ID, "NBE4")
fish2$site5 <-str_extract(fish2$Sample.ID, "NBE5")
fish2$site6 <-str_extract(fish2$Sample.ID, "NBE6")
fish2$site7 <-str_extract(fish2$Sample.ID, "NBR1")
fish2$site8 <-str_extract(fish2$Sample.ID, "NBR2")
fish2$site9 <-str_extract(fish2$Sample.ID, "NBR3")
fish2$site10 <-str_extract(fish2$Sample.ID, "NBR4")
fish2$site11 <-str_extract(fish2$Sample.ID, "NBR5")
fish2$site12 <-str_extract(fish2$Sample.ID, "NBR6")
fish2$site13 <-str_extract(fish2$Sample.ID, "NBI1")
fish2$site14 <-str_extract(fish2$Sample.ID, "NBI2")
fish2$site15 <-str_extract(fish2$Sample.ID, "NBI3")
fish2$site16 <-str_extract(fish2$Sample.ID, "NBI4")
fish2$site17 <-str_extract(fish2$Sample.ID, "NBI5")
fish2$site18 <-str_extract(fish2$Sample.ID, "NBI6")

#combining them all
fish3 <- fish2

fish4 <- fish2 %>% mutate(site = coalesce(site1,site2,site3,site4,site5,site6,site7,site8,site9,site10,	site11,	site12,	site13,	site14,	site15,	site16,	site17,	site18)) %>%
  select(Sample.ID, site)


##################
#Using the same procedre adding a water shed column
##################
fish4$watershed1 <-str_extract(fish4$site, "NBE")
fish4$watershed2 <-str_extract(fish4$site, "NBR")
fish4$watershed3 <-str_extract(fish4$site, "NBI")

fish5 <- fish4 %>% mutate(watershed = coalesce(watershed1, watershed2, watershed3)) %>%
  select(Sample.ID, site, watershed)

names(fish)
fish <- fish %>% select (Sample.ID, Amount, CO2.AMPL, N2.AMPL, d13C, d15N ,X.C, X.N, C.N )

fish6 <- fish %>%
  full_join(fish5,by="Sample.ID") 

#rearranging 
names(fish6)
fish6 <-fish6[,c(1,10,11,2,3,4,5,6,7,8,9)]
names(fish6)

#structure 
str(fish6)

#removing values that are too low (highlighed in red on the spreadsheet)
## All the numbers that highlighted NBE-28,NBE3-04, NBE3-34, NBE2-13, NBE2-05, NBE1-24, NBE1-16
#NBE1-16 and NBE1-25 low/poor isotope values
fish7 <- fish6[ !(fish6$Sample.ID %in% c("NBE1-16","NBE1-25")), ]

##############
#plots
#############
library(dplyr)
library(ggplot2)

#Scatter plot seeing how many fish were collected from each site 
ggplot(fish7, aes(x=Sample.ID, y=Amount , colour= site))+
  geom_point() +
  theme_bw()

#Seeing the spread of C/N ratio across the sites, get an idea of anomolies 
ggplot(fish7, aes(x=site, y=C.N , colour= site))+
  geom_point() +
  theme_bw()

#Spread of amount of tissue collected from each site, 
ggplot(fish7, aes(x=site, y=Amount , colour= site))+
  geom_point() +
  theme_bw()

#mean of tissue based on amount
tissuemean <- fish7 %>%
  group_by (site) %>%
  summarise (meanA= mean(Amount))

##Plot mean amount of tissue to see how much mass we were able to collect
ggplot(tissuemean, aes(x=site, y=meanA , colour= site))+
  geom_point() +
  theme_bw()

#making a histogram and layering raw data onto box-and-whisker to see the distribution
#C/N ratio is an indicator of lipid content and gives us an idea of fish health 
#comparing between watersheds
ggplot(fish7, aes(x= watershed, y= C.N))+
  geom_boxplot()+
  geom_point(size=4, colour= "tan", alpha=0.5)+
  xlab("Watersheds in New Brunswick")+
  ylab( "Carbon to Nitrogen Ratio (C/N)")+
  theme_bw()

#bar plot of C isotopes and N in each site 

