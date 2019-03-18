#############
#Assignment 9
#############
#'Hypothesis: testing if there is spatial variation in MeHg measurements between sites
#'within a watershed. Tested all three of the watersheds. 

#'Maximal model: 
#'MeHg~pH+site.no+watershed 
#'The interacting factors I think are important is the watershed the measurements are being taken from, since it's what's experiencing the "treatment", and 
#'for this analysis the site within the watershed it came from if I suspect there's a spatial effect.
#'We measured water chemistry at each site and pH is one of the most likely to have an influence on MeHg so I could consider that also as a fixed effect.
#'
#'I focused on MeHg and site only because I wanted to see if there is spatial variation between upstream and downstream MeHg levels within a watershed. 

#' Results: 
#' Note: headwater denotes as site 6, furthest site downstream as 1 

#'There was no significant difference in MeHg measurements from headwater sites compared to downstream sites within the same watershed.
#' I would take away from that we didn't see a dilution or accumulation effect with space. However, one watershed showed downstream dilution 
#(NBE2 had less MeHg than upstream NBE4 and NBE5, but NBE1 had greater)  with p<0.05. But trend wasn't consitent across all the sites within the watershed. 

#' On average, the measurements averaged in the whole watershed and compared to one another does show a sig diff in MeHg levels in NBE when compared to NBI and NBR (MeHg levels higher than the intensively and least intensively harvested site).
#' The results being: 
#' contrast  estimate    SE  df z.ratio p.value
NBE - NBI    0.400 0.109 Inf 3.686   0.0007 
NBE - NBR    0.514 0.109 Inf 4.738   <.0001 
NBI - NBR    0.114 0.109 Inf 1.052   0.5440 

