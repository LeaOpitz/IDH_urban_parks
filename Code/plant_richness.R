#group project

#Lea Opitz (s1757963)
#Alessandra Cianfanelli (s1751650)



## THis is Alison's code
## This is to set up R for reading the file if it is just in
## documents ##

library(tidyverse)

setwd("M:/")

## "oban richness is your file name"
## prich is what you want to name it ##

prich <- read.csv("Oban_Richness_1.csv")

## in the box to the left you can click your data 
## and view it to make sure it is correct ##

## below is just making a graph ##

plot(x=prich$Distance, y=prich$Richness, )
abline(lm(Richness~Distance, data = prich))


## Model 1 - GLM ##
prichglm <- glm(Richness~Distance, family = Gamma(link = "log"),data = prich)
summary(prichglm)
## click = 4 times after running plot to get 4 graphs this will
## tell us what the distubution is so we can pick the best family ##
plot(prichglm)





