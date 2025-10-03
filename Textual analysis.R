# Analysis by Alexandra Tey, a journalist with the Population and Sustainability team at the Center for Biological Diversity.
  # Contact Alex at a.tey@pm.me or the PopSus team at PopSusColab@biologicaldiversity.org.
  # https://biologicaldiversity.org | https://alextey.co | https://github.com/teyalex

# setup

  rm(list = ls())
  
  # these packages aren't on CRAN, so install.packages() won't work. if not already installed, install them like this instead
      # install_github("quanteda/quanteda.sentiment")
      # install_github("quanteda/quanteda.textplots")
  
  library(tidyverse)
  library(readtext)
  library(quanteda)
  library(remotes)
  library(quanteda.sentiment)
  library(quanteda.textplots)
  library(ggwordcloud)
  library(ggtext)
  library(effectsize)
  
  setwd("~/Documents/work/Center/Original analysis")
  
  # styles
  
    # words to exclude from text analysis
    stops <- c(stopwords("en"), "climate",  "change", "global warming", "new", "said", "can", "also", "one",
               "like", "s", "year", "now", "just", "years", "last", "many", "even", "going", "much", "according",
               "first", "say", "us", "words", "make", "get", "may", "think", "two", "know", "need", "still",
               "including", "way", "around", "help", "see", "back", "cnn", "mr", "today", "part", "copyright",
               "since", "use", "news", "really", "says", "across", "made", "already", "next", "want", "go", "lot",
               "come", "told", "week", "recent", "10", "used", "another", "called", "1", "three", "t")
  
    # color palettes
    pal2 <- colorRampPalette(c("#F3DDB1", "#ED3125"))(100)
    pal3 <- c("#A0E0AB","#ED3125", "#F3DDB1")
    pal3.1 <- c("#A0E0AB", "#F3DDB1", "#ED3125")
    
    # caption text
    cap <- "Alexandra Tey/Center for Biological Diversity | *github.com/teyalex/an-ag*"
    
    # word cloud theme
    cloud <- theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 18, family = "Oswald", hjust = 0.5),
            plot.subtitle = element_text(face = "italic", size = 10, family = "Helvetica", hjust = 0.5),
            plot.caption = element_markdown(size = 8, family = "Helvetica"))
            
    # box plot theme
    box <- theme_bw() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
            plot.subtitle = element_text(size = 10, family = "Helvetica"),
            plot.caption = element_markdown(size = 8, family = "Helvetica"),
            axis.title.x = element_text(size = 10, family = "Oswald"),
            axis.title.y = element_text(size = 10, family = "Oswald"),
            axis.text.y = element_text(size = 8, family = "Helvetica"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    # line plot theme
    line <- theme_bw() +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
            plot.subtitle = element_text(size = 10, family = "Helvetica"),
            plot.caption = element_markdown(size = 8, family = "Helvetica"),
            axis.title.x = element_text(size = 10, family = "Oswald"),
            axis.title.y = element_text(size = 10, family = "Oswald"),
            axis.text.y = element_text(size = 8, family = "Helvetica"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())

# importing articles

  # all scopes together
    
    df <- readtext("Corpus/*.txt",
                    docvarsfrom = "filenames",
                    docvarnames = c("date", "number", "scope"),
                    dvsep = "_") %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             month = format(as.Date(date), "%Y-%m-01")) %>%
      filter(date >= "2022-07-01",
             date <= "2025-07-31") %>%
      select(date, month, scope, text)
    
    corpus <- corpus(df)
    
  # "cli" scope (all climate articles)
    
    cli_df <- readtext("Corpus/*.txt",
                         docvarsfrom = "filenames",
                         docvarnames = c("date", "number", "scope"),
                         dvsep = "_") %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             month = format(as.Date(date), "%Y-%m-01")) %>%
      filter(scope == "cli",
             date >= "2022-07-01",
             date <= "2025-07-31") %>%
      select(date, month, text)
    
    cli <- corpus(cli_df)
    
  # "meat" scope (articles mentioning meat or animal agriculture)
    
    meat_df <- readtext("Corpus/*.txt",
                    docvarsfrom = "filenames",
                    docvarnames = c("date", "number", "scope"),
                    dvsep = "_") %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             month = format(as.Date(date), "%Y-%m-01")) %>%
      filter(scope == "meat",
             date >= "2022-07-01",
             date <= "2025-07-31") %>%
      select(date, month, text)
    
    meat <- corpus(meat_df)
    
  # "diet" scope (articles mentioning dietary changes or shifts)
    
    diet_df <- readtext("Corpus/*.txt",
                    docvarsfrom = "filenames",
                    docvarnames = c("date", "number", "scope"),
                    dvsep = "_") %>%
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             month = format(as.Date(date), "%Y-%m-01")) %>%
      filter(scope == "diet",
             date >= "2022-07-01",
             date <= "2025-07-31") %>%
      select(date, month, text)
    
      diet <- corpus(diet_df)
  
# keywords in context
  
  # computing tokens in the corpus
  tokens <- tokens(corpus)
  
  # "meat" keywords in context
  
    # creating data frame for "meat" KWIC
    kwic.meat_df <- as.data.frame(kwic(tokens, pattern = "meat")) %>%
      mutate(text = paste(pre, post, sep = " ")) %>%
      distinct(text)
    
    # creating corpus for "meat" KWIC
    kwic.meat_corpus <- corpus(kwic.meat_df)
    
    # creating document-feature matrix for "meat" KWIC
    kwic.meat_dfm <- kwic.meat_corpus |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      tokens_compound(pattern = phrase(c("greenhouse gas", "animal agriculture", "united states",
                                         "fossil fuels", "fossil fuel", "united nations", "new york", "new york city",
                                         "joe biden", "donald trump", "carbon dioxide", "tom vilsack"))) |>
      tokens_remove(stops) |>
      dfm()
    
    # creating data frame for most frequent words
    kwic.meat_top <- as.data.frame(topfeatures(kwic.meat_dfm, 100))
    kwic.meat_top <- rownames_to_column(kwic.meat_top, "word")
    names(kwic.meat_top) <- NULL
    names(kwic.meat_top) <- c("word", "count")

  # "agriculture" keywords in context
    
    # creating data frame for "agriculture" KWIC
    kwic.ag_df <- as.data.frame(kwic(tokens, pattern = "agriculture")) %>%
      mutate(text = paste(pre, post, sep = " ")) %>%
      distinct(text)
  
    # creating corpus for "agriculture" KWIC
    kwic.ag_corpus <- corpus(kwic.ag_df)
    
    # creating document-feature matrix for "agriculture" KWIC
    kwic.ag_dfm <- kwic.ag_corpus |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      tokens_compound(pattern = phrase(c("greenhouse gas", "animal agriculture", "united states",
                                         "fossil fuels", "fossil fuel", "united nations", "new york", "new york city",
                                         "joe biden", "donald trump", "carbon dioxide", "tom vilsack"))) |>
      tokens_remove(stops) |>
      dfm()
    
    # creating data frame for most frequent words
    kwic.ag_top <- as.data.frame(topfeatures(kwic.ag_dfm, 100))
    kwic.ag_top <- rownames_to_column(kwic.ag_top, "word")
    names(kwic.ag_top) <- NULL
    names(kwic.ag_top) <- c("word", "count")
    
  # "diet" keywords in context
    
    # creating data frame for "agriculture" KWIC
    kwic.diet_df <- as.data.frame(kwic(tokens, pattern = "diet")) %>%
      mutate(text = paste(pre, post, sep = " ")) %>%
      distinct(text)
    
    # creating corpus for "agriculture" KWIC
    kwic.diet_corpus <- corpus(kwic.diet_df)
    
    # creating document-feature matrix for "agriculture" KWIC
    kwic.diet_dfm <- kwic.diet_corpus |>
      tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      tokens_compound(pattern = phrase(c("greenhouse gas", "animal agriculture", "united states",
                                         "fossil fuels", "fossil fuel", "united nations", "new york", "new york city",
                                         "joe biden", "donald trump", "carbon dioxide", "factory farm.", "animal agriculture"))) |>
      tokens_remove(stops) |>
      dfm()
    
    # creating data frame for most frequent words
    kwic.diet_top <- as.data.frame(topfeatures(kwic.diet_dfm, 100))
    kwic.diet_top <- rownames_to_column(kwic.diet_top, "word")
    names(kwic.diet_top) <- NULL
    names(kwic.diet_top) <- c("word", "count")
      
# token counts in full corpus
  
  # creating document-feature matrix for full corpus
  dfm <- corpus |>
    tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
    tokens_compound(pattern = phrase(c("greenhouse gas", "animal agriculture", "united states",
                                       "fossil fuels", "fossil fuel", "united nations", "new york", "new york city",
                                       "joe biden", "donald trump", "carbon dioxide", "factory farm.", "animal agriculture"))) |>
    tokens_remove(stops) |>
    dfm()
  
  # creating data frame for top 100 terms in full corpus
  top100 <- as.data.frame(topfeatures(dfm, 100))
  top100 <- rownames_to_column(top100, "word")
  names(top100) <- NULL
  names(top100) <- c("word", "count")
  
  # creating data frame for top 1,000 terms in full corpus
  top10000 <- as.data.frame(topfeatures(dfm, 10000))
  top10000 <- rownames_to_column(top10000, "word")
  names(top10000) <- NULL
  names(top10000) <- c("word", "count")
  
  # finding which terms in word list are in top 1,000 terms
  
    words <- c("meat", "dairy", "livestock", "cow", "cows", "cattle", "beef", "sheep", "lamb", "chicken", "poultry",
               "hog.", "pig.", "pork", "rais.", "consum.", "eat.", "methane", "animal", "agriculture", "animal agriculture",
               "farm.", "factory farm.", "diet.", "consum.", "plant-based", "veg.", "flexitarian", "seafood", "pescatarian",
               "fish", "vegan.", "vegetarian.")
    
    top10000 <- top10000 %>%
      mutate(agdiet = case_when(
        word %in% words ~ T,
             .default = F))
    
    top10000[(top10000$agdiet == T),]
    
    # two words ranking above and below each `agdiet` terms in top 10,000
    
      # "methane"
      top10000[212:216,]
      
      # "agriculture"
      top10000[575:580,]
      
      # "meat"
      top10000[706:710,]
      
      # "fish"
      top10000[720:724,]
      
      # "animal"
      top10000[1354:1358,]
      
      # "dairy"
      top10000[1431:1435,]
      
      # "beef"
      top10000[1527:1531,]
      
      # "cows"
      top10000[1566:1570,]
      
      # "cattle"
      top10000[1619:1623,]
      
      # "livestock"
      top10000[1740:1744,]
      
      # "plant-based"
      top10000[2062:2066,]
      
      # chicken
      top10000[2516:2520,]
      
      # "cow"
      top10000[3169:3173,]
      
      # "pork"
      top10000[4682:4686,]
      
      # "sheep"
      top10000[4764:4768,]
      
      # "seafood"
      top10000[5878:5882,]
      
      # "poultry"
      top10000[6280:6284,]
      
      # "lamb"
      top10000[6786:6790,]
    
# sentiment analysis
  
  # cli
  cli_sent <- as.data.frame(cli |>
                              textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "1")
  
  # meat
  meat_sent <- as.data.frame(meat |>
                              textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "2")
  
  # diet
  diet_sent <- as.data.frame(diet |>
                              textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "3")

  # "meat" KWIC
  kwic.meat_sent <- as.data.frame(kwic.meat_dfm |>
                               textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "2")
  
  # "diet" KWIC
  kwic.diet_sent <- as.data.frame(kwic.diet_dfm |>
                                    textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "3")
       
  # testing for significance of differences in sentiment
  
    # sentiment of scopes
    
      sent <- rbind(cli_sent, meat_sent, diet_sent)
      
      sent_test <- sent %>%
        pivot_wider(names_from = scope,
                    values_from = sentiment) %>%
        drop_na()
      
      # cli vs. meat
      wilcox.test(sent_test$`1`, sent_test$`2`, paired = TRUE)
      
      # meat vs. diet
      wilcox.test(sent_test$`2`, sent_test$`3`, paired = TRUE)
      
      # cli vs. diet
      wilcox.test(sent_test$`1`, sent_test$`3`, paired = TRUE)
    
    # sentiment of KWIC
    
      kwic_sent <- rbind(kwic.meat_sent, kwic.diet_sent)
  
      kwic_sent_test <- kwic_sent %>%
        pivot_wider(names_from = scope,
                     values_from = sentiment) %>%
        drop_na()
      
      # "meat" KWIC vs. "diet" KWIC
      wilcox.test(kwic_sent_test$`2`, kwic_sent_test$`3`, paired = TRUE)
      
# article counts over time (by month)
    
  months_df <- df %>%
    mutate(month = as.Date(month, format = "%Y-%m-%d")) %>%
    group_by(month, scope) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  
  print(months_df, n = 10)
    
# plotting
  
  # PLOT: word cloud for full corpus
  
    top_plot <- ggplot(top100, aes(label = word, size = count, color = count)) +
      geom_text_wordcloud(family = "Oswald") +
      labs(title = "100 most frequent terms",
           subtitle = "From 8,058 climate articles between July 2022 and July 2025",
           caption = cap) +
      scale_color_continuous(palette = pal2) +
      scale_size_continuous(range = c(3, 15)) +
      cloud
    
    print(top_plot)
    
    ggsave("top_plot.jpg", plot = top_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
  
  # PLOT: word cloud of most frequent words near "meat"
  
    kwic.meat_plot <- ggplot(kwic.meat_top, aes(label = word, size = count, color = count)) +
      geom_text_wordcloud(family = "Oswald") +
      labs(title = "Keywords near \"meat\"",
           subtitle = "From 8,058 climate articles between July 2022 and July 2025",
           caption = cap) +
      scale_color_continuous(palette = pal2) +
      scale_size_continuous(range = c(3, 15)) +
      cloud
    
    print(kwic.meat_plot)
    
    ggsave("kwic.meat_plot.jpg", plot = kwic.meat_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: word cloud of most frequent words near "agriculture"
    
    kwic.ag_plot <- ggplot(kwic.ag_top, aes(label = word, size = count, color = count)) +
      geom_text_wordcloud(family = "Oswald") +
      labs(title = "Keywords near \"agriculture\"",
           subtitle = "From 8,058 climate articles between July 2022 and July 2025",
           caption = cap) +
      scale_color_continuous(palette = pal2) +
      scale_size_continuous(range = c(3, 15)) +
      cloud
    
    print(kwic.ag_plot)
    
    ggsave("kwic.ag_plot.jpg", plot = kwic.ag_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
  
  # PLOT: sentiment analysis across scopes
    
    sent_plot <- ggplot(sent, aes(x = scope, y = sentiment, fill = scope)) +
      geom_boxplot() +
      labs(title = "Article sentiment in climate coverage",
           subtitle = "Articles covering meat and diet averaged slightly higher overall sentiments with less variation.",
           caption = cap,
           x = "Scope",
           y = "Mean sentiment valence") +
      scale_x_discrete(labels = c("Climate", "+ meat", "+ diet")) +
      scale_fill_discrete(breaks = c("1", "2", "3"),
                          labels = c("climate", "+ meat", "+ diet"),
                          palette = pal3) +
      box
    
    print(sent_plot)
    
    ggsave("sent_plot.jpg", plot = sent_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: coverage of each scope over time
    
    months_plot <- ggplot(months_df, aes(x = month, y = count, color = scope)) +
      geom_step(linewidth = 0.8) +
      labs(title = "Coverage over time",
           subtitle = str_wrap("Apart from spikes around news events like COP sessions, the volume of climate reporting
                                has declined in the past three years. Animal agriculture and dietary change have been mentioned
                                in that coverage at a consistently low rate.", 90),
           caption = cap,
           x = "Month",
           y = "Number of articles",
           color = "") +
      scale_x_date(breaks = as.Date(c("2022-07-01", "2023-01-01", "2023-07-01", "2024-01-01", "2024-07-01", "2025-01-01", "2025-07-01")),
                   labels = c("July 2022", "Jan. 2023", "July 2023", "Jan. 2023", "July 2024", "Jan. 2025", "July 2025")) +
      scale_color_discrete(breaks = c("cli", "meat", "diet"),
                           labels = c("climate", "+ meat", "+ diet"),
                           palette = pal3.1) +
      line
    
    print(months_plot)
    
    ggsave("months_plot.jpg", plot = months_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)