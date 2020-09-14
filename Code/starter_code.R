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
b_plants <- read.csv("Data/raw_data_b_plants.csv") %>% 
  rename(q_species = q.species)
c_plants <- read.csv("Data/raw_data_c_plants.csv") 
soil <- read.csv("Data/raw_data_soil.csv")

p_div <- read.csv("Data/PlantD.csv")
i_div <- read.csv("Data/InsectD.csv")

#join data
plants <- bind_rows(b_plants, c_plants) %>%
  dplyr::select(1:36)
