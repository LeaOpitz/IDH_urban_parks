# Field course in EES 2020
# University of Edinburgh

# Lea Opitz (s1757963)
# Alessandra Cianfanelli (s1751650)


# Libraries ----
library(tidyverse)
library(wesanderson)
library(vegan)
library(goeveg)


#load data ----
b_plants <- read.csv("Data/raw_data_b_plants.csv")
c_plants <- read.csv("Data/raw_data_c_plants.csv") 
soil <- read.csv("Data/raw_data_soil.csv")

#join data
plants <- bind_rows(b_plants, c_plants)
