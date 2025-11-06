## This code was adapted by Alexandra Tey, a journalist with the Center for Biological Diversity,
  ## from code published by Faunalytics and Sentient Media.

## Sections in the original Faunalytics version are labeled with comments set off by a single octothorpe (#).
  ## Sections added in the Center's version are labeled with comments set off by double octothorpes and a space (## ).

## Contact Alex at a.tey@pm.me or the Center's Population and Sustainability team at PopSusColab@biologicaldiversity.org.
  ## https://biologicaldiversity.org | https://alextey.co | https://github.com/teyalex

  ## NOTE: Lines that download files to the user's computer are commented out.
    ## If you do want to save JPGs of the plots, use a CMND-F/CTRL-F find-replace to
    ## replace "# ggsave" with "ggsave".

#Clear workspace
rm(list = ls())

library(tidyverse)
library(ggtext)

#Set working directory
setwd("~/Your/file/directory/here")

## Find the original code used to prepare the data at https://osf.io/xmwgz/files/osfstorage ("1.Categorization.R")
  ## The Center's (very slightly) adapted version of this step is at https://github.com/teyalex/an-ag.

#Read file with categorized articles
df <- read.csv("Categorized articles.csv") ## In the original version, this step created
                                            ## a data frame called "categorized" that isn't
                                            ## necessary for our purposes, so now it's just df.

## df <- categorized[,-c(3:4)] ## This removed columns for "year" and "bias," which
                                  ## were not included in the Center's data collection.

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

## Center for Biological Diversity additions

  ## Reorganizing data frames for plotting

    df <- df %>%
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
      mutate(total = case_when(
        source == "Chicago Tribune" ~ 88,
        .default = 100))
    
    ## For mentions of all categories by publication in long (rather than wide) format
    
      df_long <- df %>%
        pivot_longer(cols = c(animal.ag_count, general.ag_count, regen.ag_count, land.use_count, mme_count,
                              transport_count, residential_count, consumerism_count, fossil.fuels_count, emissions_count,
                              livestock_count, cow_count, total),
                     names_to = "category",
                     values_to = "count")
      
    ## Summarizing categories
      
      categories <- df_long %>%
        group_by(category) %>%
        summarize(total = sum(count),
                  mean = mean(count, na.rm = TRUE),
                  sd = sd(count, na.rm = TRUE))
      
    ## For mentions of each category, not sorted by publication
    
      causes <- df_long %>%
        select(category, count) %>%
        filter(!(category == "total")) %>%
        group_by(category) %>%
        summarize(count = sum(count)) %>%
        mutate(facet = case_when(category %in% c("animal.ag_count", "cow_count", "general.ag_count", "land.use_count",
                                                 "livestock_count", "regen.ag_count") ~ "ag",
                                 .default = "other"))
      
  ## Comparing contexts of agriculture mentions
      
    ## Extracting articles that mention animal agriculture for manual checks
      
      df_an.ag <- read.csv("Categorized articles.csv") %>%
        filter(animal.ag == 1) %>%
        select(source, number) %>%
        arrange(number)
      
      ## We used this next line to create a CSV file where I hand-coded (in Excel) the contexts in which
        ## those articles mentioned animal agriculture. The 
      
      ## write_csv(df_an.ag, "Coded articles.csv", col_names = T)
      
    ## Creating data frames from coding articles as mentioning animal agriculture in the
      ## context of being "cause" or suffering a "consequence" or climate change
      
      an.ag_codes <- read.csv("Coded articles.csv") %>%
        mutate(context = case_when(cause == 1 & consequence == 0 ~ "cause_only",
                                   cause == 0 & consequence == 1 ~ "consequence_only",
                                   cause == 1 & consequence == 1 ~ "both",
                                   cause == 0 & consequence == 0 ~ "neither")) %>%
        filter(context != "neither") %>%
        select(source, context)
      
    ## Plot themes
      
      ## Color palettes 
      
      pal3 <- c("#F3DDB1", "#D5A00F", "#ED3125")
      pal3.1 <- c("#F3DDB1", "#F3DDB1", "#ED3125")
      pal2 <- c("#F3DDB1", "#ED3125")
      
      ## Caption text
      
      cap <- "Adapted from Faunalytics/Sentient Media | *github.com/teyalex/an-ag*"
      
      ## Standard/horizontal geom_col plot
      
      h.col <- theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
              plot.subtitle = element_text(size = 10, family = "Roboto"),
              plot.caption = element_markdown(size = 8, family = "Roboto"),
              axis.title.x = element_text(size = 10, family = "Oswald"),
              axis.text.x = element_text(size = 8, family = "Roboto"),
              axis.text.y = element_text(size = 8, family = "Oswald"),
              panel.grid.major.x = element_blank())
      
      ## Flipped/vertical geom_col plot, grid lines
      
      v.col <- theme_minimal() +
        theme(legend.position = "top",
              plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
              plot.subtitle = element_text(size = 10, family = "Roboto"),
              plot.caption = element_markdown(size = 8, family = "Roboto"),
              axis.title.x = element_text(size = 10, family = "Oswald"),
              axis.text.x = element_text(size = 8, family = "Roboto"),
              axis.text.y = element_text(size = 8, family = "Oswald"),
              panel.grid.major.y = element_blank())
  
  ## Plotting general and animal agriculture mentions in relation to all articles
      
      simple.mentions_plot <- ggplot(data = df_long %>%
                                       filter(category %in% c("total", "general.ag_count", "animal.ag_count")),
                                     aes(x = reorder(category, -count), y = count, fill = reorder(category, count))) +
        geom_col(width = 0.5) +
        labs(title = "Animal ag is underrepresented in climate stories",
             subtitle = str_wrap("CAPTION TKTK", 115),
             caption = cap,
             x = element_blank(),
             y = "Number of articles",
             fill = element_blank()) +
        scale_fill_manual(values = pal3,
                          breaks = c("total", "general.ag_count", "animal.ag_count")) +
        scale_x_discrete(breaks = c("total", "general.ag_count", "animal.ag_count"),
                         labels = c("Total climate headlines", "All ag mentions", "Animal ag mentions")) +
        h.col
      
      print(simple.mentions_plot)
      
      # ggsave("simple.mentions_plot.jpg", plot = simple.mentions_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)

  ## Plotting mentions of transportation and animal agriculture in relation to total counts
  
      mentions_plot <- ggplot(data = categories %>%
                                filter(category %in% c("animal.ag_count", "general.ag_count", "transport_count")),
                              aes(x = reorder(category, -mean), y = mean, fill = category)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                    width = 0.2) +
      labs(title = "Animal agriculture in climate coverage",
           subtitle = str_wrap("TKTK",
                               100),
           caption = cap,
           x = element_blank(),
           y = "Average mention count by publication",
           fill = element_blank()) +
      scale_x_discrete(breaks = c("transport_count", "general.ag_count", "animal.ag_count"),
                       labels = c("Transportation", "General ag", "Animal ag")) +
      scale_fill_manual(values = pal3,
                        breaks = c("transport_count", "general.ag_count", "animal.ag_count")) +
      h.col

    print(mentions_plot)
    
    # ggsave("mention_plot.jpg", plot = mentions_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
  
  ## Plotting causes of climate change across all outlets
  
    causes_plot <- ggplot(data = causes,
           aes(x = reorder(category, count), y = count, fill = facet)) +
      geom_col() +
      geom_text(aes(label = count),
                size = 3, position = position_dodge(width = 1), hjust = -0.2, family = "Oswald") +
      coord_flip() +
      labs(title = "Named industries in climate coverage",
           subtitle = str_wrap("Articles about the climate seldom mentioned themes related to agriculture,
                               especially compared to other causes of climate change.", 80),
           caption = cap,
           x = element_blank(),
           y = element_blank(),
           fill = element_blank()) +
      scale_fill_manual(values = pal2,
                        breaks = c("other", "ag"),
                        labels = c("Other climate topics", "Agricultural")) +
      scale_x_discrete(labels = c(mme_count = "Mining, manufacturing, energy",
                                  emissions_count = "Emissions",
                                  fossil.fuels_count = "Fossil fuels",
                                  transport_count = "Transportation",
                                  general.ag_count= "All agriculture",
                                  residential_count = "Residential",
                                  animal.ag_count = "Animal agriculture",
                                  consumerism_count = "Consumerism",
                                  regen.ag_count = "Regnerative agriculture",
                                  land.use_count = "Land use",
                                  cow_count = "Cows",
                                  livestock_count = "Livestock")) +
      v.col
    
    print(causes_plot)
    
    # ggsave("causes_plot.jpg", plot = causes_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  ## Plotting mentions of animal agriculture by publication and cause/consequence context
    
     context_plot <- ggplot(an.ag_codes %>%
                              filter(!context %in% c("neither")),
                            aes(x = context, fill = context)) +
      geom_bar(position = position_stack(),
               width = 0.5) +
      coord_flip() +
      labs(title = "Context of agriculture mentions",
           subtitle = str_wrap("Only 8% of the article sampled mentioned agriculture at all. Many did so
                               only in the context of climate change's impacts on agriculture, but almost as
                               many only framed it the other way around. Least often were both narratives included.",
                               80),
           caption = cap,
           x = "",
           y = "Number of articles",
           fill = element_blank()) +
      scale_fill_manual(values = pal3.1,
                        breaks = c("consequence_only", "cause_only", "both")) +
      scale_x_discrete(breaks = c("consequence_only", "cause_only", "both"),
                       labels = c("Climate affecting agriculture", "Agriculture affecting climate", "Both")) +
      scale_y_continuous(position = "right") +
      v.col +
      theme(legend.position = "none")
    
    print(context_plot)
    
    # ggsave("context_plot.jpg", plot = context_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)