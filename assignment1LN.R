#############
#Bio 708
#First Assignment, Lauren Negrazis
#Different chemistry measurements of water taken during sculpin sampling 
##############

## BMB: you probably don't want *any* of these lines in working code 
getwd()            ##  irrelevant if you're not running the code line-by-line
dir.create("data") ## you should do this once, not every time you run the code
## this is bad practice: see https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
rm(list=ls())

library(dplyr)
library(ggplot2)

watervar <- read.csv ("sculpinblitz2017watervar.csv")
watervar ## this line shouldn't be here either (only in interactive code)


##want to make a box and whisker plot of the water conductivity, TDS and DO between the 3 watersheds, showing all the data points across the set
#These are important water chemistry variables that can change with disturbance and alter a stream ecosystem

##For Specific conductance 
#Make ggplot picture and removing grey background, just to see if any patterns apparent
ggplot(watervar, aes(x=Sample_Code, y= SpCond_uS.cm, colour= Site))+
  geom_point() +
    theme_bw()

## BMB: I like to use theme_set(theme_bw()) in my script to set the theme
## globally
## I'd consider coord_flip(), plus a better y-axis label


#making a histogram and layering raw data onto box-and-whisker
ggplot(watervar, aes(x= Site, y= SpCond_uS.cm))+
  geom_boxplot()+
  geom_point(size=4, colour='lightblue', alpha=0.5)+
  xlab("Watershed in New Brunswick")+
  ylab( "Specific Conductance (uS/cm)")+
    theme_bw()

## BMB: where's the histogram? but this looks good

##For TDS
#affects the movement of water into cells

#scatter plot
ggplot(watervar, aes(x=Sample_Code, y= TDS_mg.L, colour= Site))+
  geom_point() +
  theme_bw()

#histogram
ggplot(watervar, aes(x= Site, y= TDS_mg.L))+
  geom_boxplot()+
  geom_point(size=4, colour='lightgreen', alpha=0.5)+
  xlab("Watershed in New Brunswick")+
  ylab( "Total Dissolved Solutes (mg/L)")+
  theme_bw()

##For Dissolved oxygen 
#scatter plot
ggplot(watervar, aes(x=Sample_Code, y= DO_mg.L, colour= Site))+
  geom_point() +
  theme_bw()

#histogram
ggplot(watervar, aes(x= Site, y= DO_mg.L))+
  geom_boxplot()+
  geom_point(size=4, colour='lightgrey', alpha=0.5)+
  xlab("Watershed in New Brunswick")+
  ylab( "Dissolved Oxygen (mg/L)")+
  theme_bw()
## BMB: again, this is totally not a histogram!

## BMB: bottom line, this looks fine. score=2
## (where 1=poor, 2=fine, 3=amazing)
