# QMEE
Repo made for Wed, Jan 9, 2019 class
This is a line from RStudio
This is a line from GitHub

##############
#Update on Assignment 3
##############
I want to explore the relationship of methlymerury (MeHg) levels in the three catchements in New Brunswick. I collected samples from three different catchements that are harvested at three different levels.
NBi is the most intensely harvested and we predict that we should see higher average MeHg in this cathcement. 
NBE is the middle level of harvesting and NBR is minimally harvested and expected to have the lowest levels of Hg.

The first plot is the average MeHg from each site and all plotted along one axis, colour coded to catchement for a quick look at the averages across the sites and catchements. 
The second plot I wanted to use facet wrap to really make it clear the different catchements. I should have ordered them based on harvesting intensity but for now they are in alphabetical order. 
The third is a boxplot for each total catchment MeHg, NBE appears to have the highest overall MeHg in the water, which we weren't expecting. The variation in MeHg is greater in NBE, but the average looks significantly higher. NBR has the lowest, which was expected. 
The last plot is a geom_smooth plot of the overlayinig the three catchments to compare the differences between the sites. Site 1 was always the headwater of the catchment and site 6 was the furthest downstream. This gives an idea of the spatial varaiation of MeHg in the catchments. 

It just occured to me that I can check for temporal variation since we collected samples in Septeber and October of this year. 

#######
#Update on Assignment 4
#######
Part A in the folder answers the quesiton about MMV.
Part B in the folder talks about the statistical tests I would use for my data and the hypothesis that I would be testing. 

## JD: Are you sure you've checked your assumptions about your MMV friend?

## Thanks for teaching me new chemistry notation; I don't know if I can get used to it, though.

## You should actually have a stats plan before you start doing things like graphing response vs. predictory

## It would be good to be clear about your questions before you decide which comparisons to do. Make good decisions before you spend power

## Week grade: Good (2/3)

#########
#Assignment 5, Permutations
#########
Uploaded a folder with the R script and .csv file.

#########
#Assignment 6, Linear Models
#########
Uploaded folder with R script and .csv file. I recently got some data in about the water chemistry about the sites and it
would be neat to see if any of those factors have any significant effect on my data. Especially since there is literature that says
several things can affect the amount of MeHg in aquatic systems. 

############
#Assignmet 7
############
I couldn't get the code to work, but I set it up the best I could to reflect the codes given in class. 
I wanted to use pH to build my prior probability because it's been shown in the literature to have a inverse relationship to the amount of MeHg in stream water.

###########
#Assignment 8
###########
I made a predicition of the amount of MeHg in the stream water based off my results if the pH was 6.3 in the stream water. The outcome to the prediction is 0.11338 ng/L. I would have expected the result to be higher because water at a pH of 6.35 has a MeHg of 0.137265816.

Chisqr results Model: deviance= 0.50034, p=0.01989.
Dispersion paramter is 0.36434/16=0.02277.
