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
install.packages("emmeans") #for tukey


# Libraries ----
library(tidyverse)
library(wesanderson)
library(vegan)
library(goeveg)
library(emmeans)


#load data ----
b_plants <- read.csv("Data/raw_data_b_plants.csv") %>% 
  rename(q_species = q.species) #rename variable
c_plants <- read.csv("Data/raw_data_c_plants.csv") 
soil <- read.csv("Data/raw_data_soil.csv")

p_div <- read.csv("Data/PlantD.csv")
i_div <- read.csv("Data/InsectD1.csv") %>% 
  drop_na()

ali_ins <-  read.csv("Data/raw_insect_data_ali.csv")
da_ins_c <- read.csv("Data/raw_insect_data_daphnebC.csv")
da_ins_b <- read.csv("Data/raw_insect_data_daphneB.csv")
#write.csv2(species, file="test.csv")





#join data
plants <- bind_rows(b_plants, c_plants) %>% #join
  dplyr::select(1:36) #remove last columns

plants = plants[1:56,]


species <-  dplyr::select(plants,2:36) 
species[is.na(species)] <- 0
species = species[1:56,]


#insects
insect <- full_join(da_ins_b, da_ins_c, by = "Order" )
insect1 = insect[1:19,]

# switch rows and columns
names <- insect1$Order
insect2 <- as.data.frame(t(insect1[,-1]))
colnames(insect2) <- names
insect2$q_plots <- factor(row.names(insect2))
insect2[is.na(insect2)] <- 0
insect2 = insect2[1:51,] #delete empty columns
insect2 <- filter(insect2, !(q_plots %in% c("X.1.x","X.x")))

insect3 <- dplyr::select(insect2,1:19)

insect3 <- dplyr::select(insect2,1:19)



### boxplot----
ggplot(data= p_div, aes(x= as.factor(distance), y = Shannon, fill = Site))+
      geom_boxplot(size = 0.3) +
      theme_classic()+ 
  scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
                    values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\nDistance [m]") +
  scale_y_continuous(name = "Shannon's Diversity Index\n")+
  theme(text=element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5))
                      
ggsave("plantbox.pdf", plot = last_plot(), width = 6, height = 5, units = "cm", scale = 7)                      
 


ggplot(data= i_div, aes(x = as.factor(distance), y = Shannon, fill = Site))+
  geom_boxplot(size = 0.3) +
  theme_classic()+ 
  scale_fill_manual(  #scale_fill_manual controls the colours of the 'fill' you specified in the 'ggplot' function.
    values = c("#FEB96C", "#CC92C2"))+
  scale_x_discrete(name = "\nDistance [m]") +
  scale_y_continuous(name = "Shannon's Diversity Index\n", limits = c(0,4))+
  theme(text = element_text(size = 18), axis.line = element_line(size = 0.5), axis.ticks = element_line(size = 0.5))

ggsave("invertbox_newaxis.pdf", plot = last_plot(), width = 6, height = 5, units = "cm", scale = 7)                      



### NMDS -----

##plants
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


## Distance plot, with nice colours PLANTS NMDS --------------

group = rep(c("0m", "1m", "7m", "14m"), 14)
group.fac <- factor(group, levels = c("0m", "1m", "7m", "14m"))

# Create a vector of color values with same length as the vector of group values
colors = rep(c("yellow", "orange", "red", "purple"), 14)

# Plot convex hulls with colors based on the group identity
par(mar=c(5,5,1,1))
ordiplot(NMDS3, type = "n", cex.lab = 2, cex.axis = 2)

for(i in unique(group)) {
  ordihull(NMDS3$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

# orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", labels = F, air = 0.01, cex = 1.25)
legend("topright", legend=levels(group.fac), bty= "n",
       col = colors, pch = 21, pt.bg = colors, cex = 1.5)
#take species off graph


## Distance plot, with nice colours INSECTS NMDS --------------
#check dimension
dimcheckMDS(as.matrix(insect3), distance = "bray", k = 6, trymax = 20,
            autotransform = TRUE)

# Run the NMDS
set.seed(2)  # Because the final result depends on the initial random placement of the points, we`ll set a seed to make the results reproducible
NMDS2 <- metaMDS(insect3, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")
NMDS2

# Check the stress
stressplot(NMDS2)

# Look at the results!
plot(NMDS2)

# nicer plot
ordiplot(NMDS2, type = "n")
orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", cex = 1.1, air = 0.01)

## sites
# Define a group variable (first 12 samples belong to group 1, last 12 samples to group 2)
group = c(rep("Blackford", 28), rep("Craigmillar", 28))

# Create a vector of color values with same length as the vector of group values
colors = c(rep("orange", 28), rep("purple", 28))

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS2, type = "n", cex.lab = 2, cex.axis = 2)

for(i in unique(group)) {
  ordihull(NMDS2$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(NMDS2, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", col = c(rep("red",28),  rep("blue", 28)), air = 0.01, cex = 1.25)

## Distance plot, with nice colours: INSECTS

group <- substr(insect2$q_plots, 5,5)
group[group=="1"] <- "0m"
group[group=="2"] <- "1m"
group[group=="3"] <- "7m"
group[group=="4"] <- "14m"

group.fac <- factor(group, levels = c("0m", "1m", "7m", "14m"))

# Create a vector of color values with same length as the vector of group values
colors <- group
colors[colors=="0m"] <- "yellow"
colors[colors=="1m"] <- "orange"
colors[colors=="7m"] <- "red"
colors[colors=="14m"] <- "purple"

# Plot convex hulls with colors based on the group identity
ordiplot(NMDS2, type = "n", cex.lab = 2, cex.axis = 2)

for(i in unique(group)) {
  ordihull(NMDS2$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

# orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS2, display = "sites", labels = F, air = 0.01, cex = 1.25)
legend("bottomleft", legend=levels(group.fac), bty= "n",
       col = colors, pch = 21, pt.bg = colors)



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

aov.plant <- aov(Shannon~distance*Site, data = p_div)
 
#insects
insectlm <- lm(Shannon~distance*Site, data = i_div)
summary(insectlm)
anova(insectlm)
plot(insectlm)


## distance as factor
insectlm2 <- lm(Shannon~as.factor(distance)*Site, data = i_div)
summary(insectlm2)
anova(insectlm2) #less significant
plot(insectlm2)

### Tukey test ----
tukey_plants <- TukeyHSD(aov.plant)
tukey_plants


# emmeans package
emmeans(plantlm2, pairwise ~ distance|Site)


emmeans(insectlm2, pairwise ~ distance|Site)


# ~~~~~~~~~~ community composition stats ~~~~~~~~~

 # INSECTS 
group <- substr(insect2$q_plots, 5,5)
group[group=="1"] <- "0m"
group[group=="2"] <- "1m"
group[group=="3"] <- "7m"
group[group=="4"] <- "14m"

group.fac <- factor(group, levels = c("0m", "1m", "7m", "14m"))

# Anosim for insects ----
# setting the seed means the 'random' outcome will always be the same and reproducible.
set.seed(123)
anosim(insect3, grouping = group.fac, permutations = 999, distance = "bray", strata = NULL,
       parallel = getOption("mc.cores"))


# PLANTS
# setting the seed means the 'random' outcome will always be the same and reproducible.
set.seed(123)

# this is what i THINK is the correct plant grouping to use 'group.fac'
group = rep(c("0m", "1m", "7m", "14m"), 14)
group.fac <- factor(group, levels = c("0m", "1m", "7m", "14m"))

<<<<<<< HEAD
# however, when using group.fac in the code below, the following error appears
=======
# anosim for plants ---
>>>>>>> 6fc09e9863adbff3638305b1ec6ebd95ad663d42
anosim(species, grouping = group.fac, permutations = 999, distance = "bray", strata = NULL,
      parallel = getOption("mc.cores"))



## -------  Simper test --------

# INSECT
simper(insect3, group, permutations = 999, trace = FALSE,  parallel = getOption("mc.cores"))

# results ...
# Error in comm[group == comp[i, 1], , drop = FALSE] : 
# (subscript) logical subscript too longv

str(insect3)


# PLANTS
simper(species, group, permutations = 999, trace = FALSE,  parallel = getOption("mc.cores"))


