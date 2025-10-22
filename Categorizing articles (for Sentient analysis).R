## This code was adapted by Alex Tey with the Center for Biological Diversity
  ## from code published by Faunalytics in collaboration with Sentient Media.

## View the original code and analysis here: https://osf.io/xmwgz/files/osfstorage.

## Comments added in the Center's version are set off by double octothorpes (##) and a space.

#Clear workspace
rm(list = ls())

#Set working directory
setwd("~/Documents/work/Center/Sentient analysis")

## install.packages("quanteda")
## install.packages("readtext")
library(quanteda)
library(readtext)
library(dplyr)
library(tidyverse)
library(writexl)

#read in articles
dataframe <- readtext("corpus/*/*.rtf",
                      docvarsfrom = "filenames",
                      docvarnames = c("source", "number"),
                      dvsep = "_")

str(dataframe)

###Cleaning###
#Convert words to lowercase
df <- dataframe %>%
  mutate(text = str_to_lower(text))

###Create Coding Variables###
#each line is a text string that is looked for in the texts
#note that a . indicates any character so it can be completed in more than one way
#note that \\b creates a word barrier (e.g., "pig" isn't considered in "spigot")
#note that !str_detect excludes that string from being considered
#Specific exclusions were added during iterative accuracy checks

df <- df %>%
#Animal Agriculture Keywords
  mutate(animal.ag = case_when(str_detect(text, "\\bmeat") & !str_detect(text, "lot of meat there") & !str_detect(text,"meat processing plant") ~ 1,
                               str_detect(text, "dairy") ~ 1,
                               str_detect(text, "\\brear\\b") ~ 1,
                               str_detect(text, ".husbandry") ~ 1,
                               str_detect(text, "\\branch\\b") & !str_detect(text, "calistoga ranch") & !str_detect(text, "rios ranch") & !str_detect(text, "ranchers") ~ 1,
                               str_detect(text, "factory farm.") ~ 1,
                               str_detect(text, "\\bpasture.") ~ 1,
                               str_detect(text, "grazing practices") ~ 1,
                               str_detect(text, "rangeland.") ~ 1,
                               str_detect(text, "livestock") ~ 1,
                               str_detect(text, "\\bcattle\\b") & !str_detect(text, "cattle shelter") & !str_detect(text, "grass for sheep, cattle and pronghorn") ~ 1,
                               str_detect(text, "\\bruminant.") ~ 1,
                               str_detect(text, "\\bcows\\b") & !str_detect(text, "cash cow") ~ 1,
                               str_detect(text, "sheep") & !str_detect(text, "grass for sheep, cattle and pronghorn") & !str_detect(text, "landed on his sheep farm")  & !str_detect(text, "day on a sheep farm")~ 1,
                               str_detect(text, "\\blamb\\b") ~ 1,
                               str_detect(text, "\\blambs") ~ 1,
                               str_detect(text, "poultry") ~ 1,
                               str_detect(text, "chicken farm.") ~ 1,
                               str_detect(text, "chickens") ~ 1,
                               str_detect(text, "\\bhens\\b") ~ 1,
                               str_detect(text, "\\bpig\\b") & !str_detect(text, "guinea pig") ~ 1,
                               str_detect(text, "\\bpigs") & !str_detect(text, "pigs fly") ~ 1,
                               str_detect(text, "aquaculture") & !str_detect(text, "high-tech aquaculture system") ~ 1,
                               str_detect(text, "aquafarm.") ~ 1,
                               str_detect(text, "fish farm.") ~ 1,
                               str_detect(text, "pisciculture") ~ 1,
                               str_detect(text, "fisheries") & !str_detect(text, "fisheries scientist") & !str_detect(text, "fisheries biologist") & !str_detect(text, "fisheries and power lines") & !str_detect(text, "tourism and fisheries") ~1,
                               str_detect(text, "seafood") & !str_detect(text, "seafood restaurant") & !str_detect(text, "taint seafood") ~ 1,
                               str_detect(text, "salmon farm") ~ 1,
                               str_detect(text, "salmon hatcher.") ~ 1,
                               str_detect(text, "\\btuna\\b") ~ 1,
                               str_detect(text, "lobster farm.") ~ 1,
                               str_detect(text, "shrimp") ~ 1,
                               TRUE ~ 0),
#General Agriculture Keywords
         general.ag = case_when(str_detect(text, "farm.") 
                                & !str_detect(text, "farmer.") #exclude farmers
                                & !str_detect(text, "solar farm.") #exclude solar farms
                                & !str_detect(text, "wind farm.") ~ 1, #exclude wind farms
                                str_detect(text, "farmland") ~ 1,
                                str_detect(text, ".agricultur.") & !str_detect(text, "regenerative agriculture") ~ 1,
                                str_detect(text, "horticulture") ~ 1,
                                str_detect(text, "vineyard") ~ 1,
                                str_detect(text, "crop production") ~ 1,
                                str_detect(text, "plantation.") ~ 1,
                                str_detect(text, "cultivat.") ~ 1,
                                str_detect(text, "harvest") ~ 1,
                                str_detect(text, "growing season") ~ 1,
                                TRUE ~ 0),
#Regenerative Agriculture Keywords
         regen.ag = case_when(str_detect(text, "regenerative.") ~ 1,
                              str_detect(text, "carbon farming") ~ 1,
                              str_detect(text, "no-till farming") ~ 1,
                              str_detect(text, "agroecological farming") ~ 1,
                              str_detect(text, "sustainable agriculture") ~ 1,
                              str_detect(text, "agroforestry") & !str_detect(text, "agroforestry center") ~ 1,
                              str_detect(text, "topsoil regeneration") ~ 1,
                              str_detect(text, "crop rotation") ~ 1,
                              str_detect(text, "cover crop.") ~ 1,
                              str_detect(text, "organic farming") ~ 1,
                              str_detect(text, "go organic") ~ 1,
                              str_detect(text, "compost") ~ 1,
                              TRUE ~ 0),
#Land-Use Changes Keywords
         land.use = case_when(str_detect(text, "deforestation") ~ 1,
                              str_detect(text, "land clearing") ~ 1,
                              str_detect(text, "land fragment.") ~ 1,
                              str_detect(text, "\\blogging") ~ 1,
                              str_detect(text, "forests are logged") ~ 1,
                              str_detect(text, "timber\\b") ~ 1,
                              str_detect(text, "urban expansion.") ~ 1,
                              str_detect(text, "road construction") ~ 1,
                              str_detect(text, "urban sprawl") ~ 1,
                              str_detect(text, "land.use change.") ~ 1,
                              str_detect(text, "\\braze.") ~ 1,
                              TRUE ~ 0),
#Mining, Manufacturing, & Energy Production Keywords
         mme = case_when(str_detect(text, "industrial") 
                         & !str_detect(text, "industrialized nations")
                         & !str_detect(text, "industrialization")
                         & !str_detect(text, "pre-industrial") ~ 1,
                         str_detect(text, "\\bmines") ~ 1,
                         str_detect(text, "\\bmining") ~ 1,
                         str_detect(text, "oil and gas") ~ 1,
                         str_detect(text, "factories") ~ 1,
                         str_detect(text, "factory") ~ 1,
                         str_detect(text, "manufactur.") ~ 1,
                         str_detect(text, "facility") ~ 1,
                         str_detect(text, "facilities") ~ 1,
                         str_detect(text, "electricity") 
                         & !str_detect(text, "electricity outage") 
                         & !str_detect(text, "no electricity") ~ 1,
                         str_detect(text, "energy") 
                         & !str_detect(text, "solar energy") 
                         & !str_detect(text, "wind energy") 
                         & !str_detect(text, "geothermal energy") ~ 1,
                         str_detect(text, "power plant.") ~ 1,
                         TRUE ~ 0),
#Transportation Keywords
         transport = case_when(str_detect(text, "transportation") ~ 1,
                               str_detect(text, "vehicle.") ~ 1,
                               str_detect(text, "\\b car\\b") & !str_detect(text, "lived out of his car") ~ 1,
                               str_detect(text, "\\bcars\\b") ~ 1,
                               str_detect(text, "automobile.") ~ 1,
                               str_detect(text, "\\btrucks") & !str_detect(text, "fire trucks") ~ 1,
                               str_detect(text, "buses") ~ 1,
                               str_detect(text, ".plane\\b",) ~ 1,
                               str_detect(text, "jet\\b") & !str_detect(text, "jet stream") ~ 1,
                               str_detect(text, "jets") ~ 1,
                               str_detect(text, "aircraft.") ~ 1,
                               str_detect(text, "\\bcruise") ~ 1,
                               str_detect(text, "traffic") ~ 1,
                               str_detect(text, "exhaust emissions") ~ 1,
                               str_detect(text, "car exhaust") ~ 1,
                               str_detect(text, "autos") ~ 1,
                               TRUE ~ 0),
#Residential Keywords
         residential = case_when(str_detect(text, "\\bresidential") ~ 1,
                                 str_detect(text, "building codes") ~ 1,
                                 str_detect(text, "home efficiency") ~ 1,
                                 str_detect(text, "refrigera.") ~ 1,
                                 str_detect(text, "air condition.") ~ 1,
                                 str_detect(text, "appliance.") ~ 1,
                                 str_detect(text, "stove") ~ 1,
                                 str_detect(text, "furnace") ~ 1,
                                 str_detect(text, "insulation") ~ 1,
                                 str_detect(text, "light.bulb") ~ 1,
                                 str_detect(text, "home energy") ~ 1,
                                 str_detect(text, "heating and cooling") ~ 1,
                                 str_detect(text, "heating system.") ~ 1,
                                 str_detect(text, "rooftop solar") ~ 1,
                                 TRUE ~ 0),
#Consumerism Keywords
         consumerism = case_when(str_detect(text, "consumerism") ~ 1,
                                 str_detect(text, "electronics") ~ 1,
                                 str_detect(text, "packaging") ~ 1,
                                 str_detect(text, "single.use plastic.") ~ 1,
                                 str_detect(text, "plastic pollution") ~ 1,
                                 str_detect(text, "plastic waste") ~ 1,
                                 str_detect(text, "microplastic.") ~ 1,
                                 str_detect(text, "landfills") ~ 1,
                                 str_detect(text, "shopping") ~ 1,
                                 str_detect(text, "black friday") ~ 1,
                                 str_detect(text, "materialism") ~ 1,
                                 str_detect(text, "fast fashion") ~ 1,
                                 str_detect(text, "food waste") ~ 1,
                                 TRUE ~ 0),
#Fossil Fuels Keywords
         fossil.fuels = case_when(str_detect(text, "\\bcoal\\b") & !str_detect(text, "canary in the coal mine") ~ 1,
                                  str_detect(text, "\\boil") & !str_detect(text, "cooking oil") ~ 1,
                                  str_detect(text, "natural gas") ~ 1,
                                  str_detect(text, "petroleum") ~ 1,
                                  str_detect(text, "fossil.fuel.") ~ 1,
                                  TRUE ~ 0),
#Emissions Keywords
         emissions = case_when(str_detect(text, "emissions") ~ 1, 
                               str_detect(text, "methane") ~ 1,
                               str_detect(text, "co2") ~ 1,
                               str_detect(text, "carbon dioxide") ~ 1,
                               str_detect(text, "nitrous oxide") ~ 1,
                               str_detect(text, "water vap.") ~ 1,
                               str_detect(text, "greenhouse.gas.") ~ 1,
                               str_detect(text, "air pollut.") ~ 1,
                               str_detect(text, "\\bozone") ~ 1,
                               str_detect(text, "carbon footprint") ~ 1,
                               str_detect(text, "smog") ~ 1,
                               TRUE ~ 0),
#The lines below search for specific animal keywords
#Livestock
         livestock = case_when(str_detect(text, "livestock") ~ 1,
                               TRUE ~ 0),
#Cows
         cow = case_when(str_detect(text, "dairy") ~ 1,
                         str_detect(text, "\\bcattle\\b") & !str_detect(text, "cattle shelter") & !str_detect(text, "grass for sheep, cattle and pronghorn")~ 1,
                         str_detect(text, "\\bcow\\b") & !str_detect(text, "cash cow") ~ 1,
                         str_detect(text, "\\bcows\\b") ~ 1,
                         str_detect(text, "\\bbovine\\b") ~ 1,
                         TRUE ~ 0),
#Sheep
         sheep = case_when(str_detect(text, "sheep") & !str_detect(text, "grass for sheep, cattle and pronghorn") & !str_detect(text, "landed on his sheep farm")  & !str_detect(text, "day on a sheep farm") ~ 1,
                           str_detect(text, "\\blamb\\b") ~ 1,
                           str_detect(text, "\\blambs") ~ 1,
                           TRUE ~ 0),
#Chickens
         chicken = case_when(str_detect(text, "poultry") ~ 1,
                             str_detect(text, "chicken farm.") ~ 1,
                             str_detect(text, "chickens") ~ 1,
                             str_detect(text, "\\bhens\\b") ~ 1,
                             TRUE ~ 0),
#Pigs
         pig = case_when(str_detect(text, "\\bpig\\b") & !str_detect(text, "guinea pig") ~ 1,
                         str_detect(text, "\\bpigs") & !str_detect(text, "pigs fly") ~ 1,
                         TRUE ~ 0),
#Fishes
         fish = case_when(str_detect(text, "aquaculture") & !str_detect(text, "high-tech aquaculture system") ~ 1,
                          str_detect(text, "aquafarm.") ~ 1,
                          str_detect(text, "fish farm.") ~ 1,
                          str_detect(text, "pisciculture") ~ 1,
                          str_detect(text, "fisheries") & !str_detect(text, "fisheries scientist") & !str_detect(text, "fisheries biologist") & !str_detect(text, "fisheries and power lines") & !str_detect(text, "tourism and fisheries") ~1,
                          str_detect(text, "seafood") & !str_detect(text, "seafood restaurant") & !str_detect(text, "taint seafood") ~ 1,
                          str_detect(text, "salmon farm") ~ 1,
                          str_detect(text, "salmon hatcher.") ~ 1,
                          str_detect(text, "\\btuna\\b") ~ 1,
                          str_detect(text, "lobster farm.") ~ 1,
                          str_detect(text, "shrimp") ~ 1,
                          TRUE ~ 0),
#Methane
         methane = case_when(str_detect(text, "methane") ~ 1,
                             TRUE ~ 0),
## Assigning unique numbers to each article
        number = ifelse(source == "The Wall Street Journal", number + 100, number),
        number = ifelse(source == "The New York Times", number + 200, number),
        number = ifelse(source == "New York Post", number + 300, number),
        number = ifelse(source == "The Washington Post", number + 400, number),
        number = ifelse(source == "Reuters", number + 500, number),
        number = ifelse(source == "Star Tribune", number + 600, number),
        number = ifelse(source == "Chicago Tribune", number + 700, number),
        number = ifelse(source == "The Boston Globe", number + 800, number),
        number = ifelse(source == "CNN", number + 900, number))

#Export df to CSV
categorized.articles <- df[,3:16]

an.ag <- df[, c(3:7, 17:21)]
 
#write_csv(categorized.articles, "Categorized articles.csv",
           #col_names = T) #File with articles categorized into 10 climate themes

#write_csv(an.ag, "Animal Ag articles.csv",
          #col_names = T) #File containing mentions of animal agriculture keywords