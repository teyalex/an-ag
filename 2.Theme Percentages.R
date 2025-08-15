## This code was adapted by Alex Tey with the Center for Biological Diversity
  ## from code published by Faunalytics in collaboration with Sentient Media.

## View the original code and analysis here: https://osf.io/xmwgz/files/osfstorage

## Comments added in the Center's version are set off by double octothorpes (##)
  ## and a space, while comments from the original Faunalytics version are set off
  ## by a single octothorpe.

#Clear workspace
rm(list = ls())

library(ggplot2)
library(gridExtra)
library(grid)
library(tidyverse)
## library(faunalytics) ## This package is not publicly available.

#Set working directory
setwd("~/Documents/work/CBD/Sentient analysis")

#Read file with categorized articles
df <- read.csv("Categorized articles.csv") ## This step  created a data frame
                                              ## called "categorized", which is
                                              ## not necessary for our purposes.

## df <- categorized[,-c(3:4)] ## This removes columns for "year" and "bias,"
                                  ## which were not used in the Center analysis.

#Create function for converting count to % of articles
percentify <- function(x) {
  percent <- (x/888) #We have 888 articles ## changed; Faunalytics had 1000
  return(percent)
  }

#Count each theme
glimpse(df)

code.counts <- df %>%
  summarise_if(is.numeric, sum) 

#Percentify the counts
code.counts <- code.counts %>%
  mutate_all(list(pct = percentify))

## Center for Biological Diversity plots

  ## Preparing to plot

  df_sums <- df %>%
    group_by(source) %>% 
    summarize(animal.ag_count = sum(animal.ag),
              general.ag_count = sum(general.ag),
              regen.ag_count = sum(regen.ag),
              land.use_count = sum(land.use),
              mme_count = sum(mme),
              transport_count = sum(transport),
              residential_count = sum(residential),
              consumerism_count = sum(consumerism),
              fossil.fuels_count = sum(fossil.fuels),
              emissions_count = sum(emissions),
              livestock_count = sum(livestock),
              cow_count = sum(cow)) %>%
    select(source, animal.ag_count, general.ag_count, regen.ag_count, land.use_count,
           mme_count, transport_count, residential_count, consumerism_count,
           fossil.fuels_count, emissions_count, livestock_count, cow_count)
  
  
  df_long <- df_sums %>%
    pivot_longer(cols = c(animal.ag_count, general.ag_count, regen.ag_count, land.use_count,
                          mme_count, transport_count, residential_count, consumerism_count,
                          fossil.fuels_count, emissions_count, livestock_count, cow_count),
                 names_to = "category",
                 values_to = "count")
  
  df_totals <- df_long %>%
    group_by(source) %>%
    summarise(count = sum(count), .groups = "drop") %>%
    mutate(category = "total")
  
  df_long <- bind_rows(df_long, df_totals) %>%
    arrange(-count)
  
  ## Plotting mentions of transportation and animal agriculture in relation to total counts
  
  mentions <- ggplot(data = df_long %>%
           filter(category %in% c("animal.ag_count", "transport_count", "total")),
           aes(x = reorder(source, count), y = count, fill = reorder(category, count))) +
    geom_col(position = "dodge") +
    geom_text(aes(label = count), size = 4, position = position_dodge(width = 1), hjust = -0.2,
              family = "Helvetica", fontface = "bold") +
    coord_flip() +
    labs(title = "Animal agriculture in climate coverage",
         subtitle = str_wrap("In nine top U.S. news outlets, articles about climate changed mentioned animal
                              agriculture just 8% of the time—and often as a victim, not a cause. Transportation,
                              which accounts for a similar share of global emissions, was mentioned 39% of the time.",
                             100),
         caption = "Adapted from an analysis by Faunalytics and Sentient Media.
                    Methodology at github.com/teyalex/an-ag.",
         x = element_blank(),
         y = element_blank(),
         fill = element_blank()) +
    scale_fill_manual(values = c("#F1E0B2", "#D19700", "#9E0005"),
                      breaks = c("total", "transport_count", "animal.ag_count"),
                      labels = c("Total climate headlines", "Transportation mentions", "Animal ag mentions")) +
    theme_minimal() +
    theme(legend.position="top",
          plot.title = element_text(face = "bold", size = 24, family = "Oswald"),
          plot.subtitle = element_text(size = 14, family = "Helvetica"),
          plot.caption = element_text(face = "italic", size = 10, family = "Helvetica"),
          legend.text = element_text(size = 12, family = "Oswald"),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 15, family = "Oswald"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  ggsave("mention_plot.jpg", plot = mentions, width = 10.8, height = 7.2, units = "in")

          
## Because the faunalytics package is not publicly available, the section that uses
  ## it for visualizations has been removed.
