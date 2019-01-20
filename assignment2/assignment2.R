#################
#Assignment 2
#2017 fish isotope data
#################
library(dplyr)
library(readr)
library(stringr)

fish <- read.csv("sculpinisotopedata2017.csv")
fish

#I want to make a column for the sites based off the Sample.ID, use somehow greater or equal to NBE1
#for each occurence of NBE1.. fill site with NBE1 etc. 

##############
#converting all NBE1 sample.IDs into 1 in column for 'site'
##############
#number of rows in sample.id for fish
x <- nrow(fish)
x

#basic table with just the sample ID from the data frame
fish2<-data.frame(number=1:x, sample= (fish$Sample.ID), stringsAsFactors = F)
              #rows^      column population^

fish2<-data.frame(number=1:x, sample= (fish$Sample.ID))
#rows^      column population^

# Adding a column for site and populating it with 1 for every occurence of 'NBE1's in each element of string, 0 if not containing NBE1
#how can I make it count the other things?
fish2$site <- str_count(fish2$sample, 'NBE1') 
fish2

###########internet stuff

#got only NBE1! YAY, can I make it find many things to extract of a vector? 
fish2$site1 <-str_extract(fish2$sample, "NBE1")
fish2$site2 <-str_extract(fish2$sample, "NBE2")
#only a few things taken out 
fish2$site4 <-str_extract(fish2$sample, "NBE1"| "NBE2" |"NBE3")
#merging not working 
merge(fish2$site1,fish2$site2)

##############
#trying to use a vector to extract all the site names only at one time but keep getting errors
###############
sitename <- c("NBE1", "NBE2", "NBE3", "NBE4", "NBE5", "NBE6", "NBR1", "NBR2", "NBR3", "NBR4", "NBR5", "NBR6", "NBI1", "NBI2", "NBI3", "NBI4", "NBI5", "NBI6")
#trying to extract all using a vector, only takes some out, white spaces? 
string <- fish2$sample %>% trimws()
#removing white spaces
str_squish(string)
###for some reason it will pull out some of the other data, but leave lots of blanks
fish2$site2 <-str_extract(string, sitename) 




#converting the 1's into NBE1 only
fish3 <- fish2
site<- fish3$site

ifelse (fish3$site ==1, "NBE1", NA)

#trying to get NBE2


##################
#Using the same procedre adding a water shed column for only NBE
##################

#make a column for watershed
#now I have 1's in the watershed column next to all the NBE sites 
fish3$watershed<- str_count(fish2$sample, "NBE")
fish3

count(fish3, c("watershed", "site"))

#make a R table to just consider the watershed, site, amount, d13C, d15N, X.C, X.N, C.N
#plot the data for each site 