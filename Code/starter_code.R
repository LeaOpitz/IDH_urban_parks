# Field course in EES 2020
# University of Edinburgh

# Lea Opitz (s1757963)
# Alessandra Cianfanelli (s1751650)
# Lynsey Thomson (s1745313)

# Install libraries ----
install.packages("tidyverse")
install.packages("wesanderson")
install.packages("vegan")
install.packages("goeveg")


# Libraries ----
library(tidyverse)
library(wesanderson)
library(vegan)
library(goeveg)


#load data ----
b_plants <- read.csv("Data/raw_data_b_plants.csv") %>% 
  rename(q_species = q.species) #rename variable
c_plants <- read.csv("Data/raw_data_c_plants.csv") 
soil <- read.csv("Data/raw_data_soil.csv")

p_div <- read.csv("Data/PlantD.csv")
i_div <- read.csv("Data/InsectD1.csv")


#write.csv2(species, file="test.csv")


#join data
plants <- bind_rows(b_plants, c_plants) %>% #join
  dplyr::select(1:36) #remove last columns

plants = plants[1:56,]


species <-  dplyr::select(plants,2:36) 
species[is.na(species)] <- 0
species = species[1:56,]


### boxplot----
ggplot(data= p_div, aes(x= as.factor(distance), y=Shannon, fill= Site))+
      geom_boxplot(size=0.7) +
      theme_classic()+ 
  scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
                    values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\nDistance [m]") +
  scale_y_discrete(name = "Shannon's Diversity Index\n")
                      
                      
 #wes_palette(name = "Darjeeling1", n = 2))


ggplot(data= i_div, aes(x= as.factor(distance), y=Shannon, fill= Site))+
  geom_boxplot(size=0.7) +
  theme_classic()+ 
  scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
    values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\nDistance [m]") +
  scale_y_discrete(name = "Shannon's Diversity Index\n")



### NMDS -----
#check dimension
dimcheckMDS(as.matrix(species), distance = "bray", k = 6, trymax = 20,
            autotransform = TRUE)

# Run the NMDS
set.seed(2)  # Because the final result depends on the initial random placement of the points, we`ll set a seed to make the results reproducible
NMDS3 <- metaMDS(species, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
NMDS3

# Check the stress
stressplot(NMDS3)

# Look at the results!
plot(NMDS3)

# nicer plot
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

## sites
# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Blackford", 28), rep("Craigmillar", 28))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("orange", 28), rep("purple", 28))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")

for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",28),  rep("blue", 28)), air = 0.01, cex = 1.25)

## Distance plot, with nice colours

group = rep(c("0m", "1m", "7m", "14m"), 14)

# Create a vector of color values with same length as the vector of group values
colors = rep(c("yellow", "orange", "red", "purple"), 14)

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS3, type = "n")

for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

# orditorp(NMDS3, display = "species", col = "red", air = 0.01)
# orditorp(NMDS3, display = "sites", labels = substr(plants$q_species,1,1), air = 0.01, cex = 1.25)

#take species off graph






##lms ----

#plants
plantlm1 <- lm(Shannon~distance*Site, data = p_div)
summary(plantlm1)
anova(plantlm1)
plot(plantlm1)

#differance between site and distance (continous) significant

# distance as factor
plantlm2 <- lm(Shannon~as.factor(distance)*Site, data = p_div)
summary(plantlm2)
anova(plantlm2) #less significant
plot(plantlm2)

#insects
insectlm <- lm(Shannon~distance*Site, data = i_div)
summary(insectlm)
anova(insectlm)
plot(insectlm)

insectlm2 <- lm(Shannon~as.factor(distance)*Site, data = i_div)
summary(insectlm2)
anova(insectlm2) #less significant
plot(insectlm2)
