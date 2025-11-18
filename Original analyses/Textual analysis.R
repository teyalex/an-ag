# Analysis by Alexandra Tey, a journalist with the Population and Sustainability team at the Center for Biological Diversity.
  # Contact Alex at a.tey@pm.me or the PopSus team at PopSusColab@biologicaldiversity.org.
  # https://biologicaldiversity.org | https://alextey.co | https://github.com/teyalex

  # NOTE: Lines that download files to the user's computer are commented out.
    # If you do want to save JPGs of the plots and a CSV of one table, use a CMND-F/CTRL-F find-replace to
    # replace "# ggsave" with "ggsave" and "# write" with "write".

# setup

  rm(list = ls())
  
  # the package `quanteda.sentiment` isn't on CRAN, so install.packages() won't work.
  # if not already installed, remove the "#" characters and install it like this instead:
    # install.packages("remotes")  
    # library(remotes)
      # install_github("quanteda/quanteda.sentiment")

  library(tidyverse)
  library(readtext)
  library(quanteda)
  library(quanteda.sentiment)
  library(quanteda.textplots)
  library(ggwordcloud)
  library(ggtext)

  setwd("~/Your/file/directory/here")
  
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
    cap <- "Center for Biological Diversity/Brighter Green | *github.com/teyalex/an-ag*"
    
    # word cloud theme
    cloud <- theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 18, family = "Oswald", hjust = 0.5),
            plot.subtitle = element_text(face = "italic", size = 10, family = "Roboto", hjust = 0.5),
            plot.caption = element_markdown(size = 8, family = "Roboto"))
            
    # box plot theme
    box <- theme_bw() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
            plot.subtitle = element_text(size = 10, family = "Roboto"),
            plot.caption = element_markdown(size = 8, family = "Roboto"),
            axis.title.x = element_text(size = 10, family = "Oswald"),
            axis.title.y = element_text(size = 10, family = "Oswald"),
            axis.text.y = element_text(size = 8, family = "Roboto"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    # line plot theme
    line <- theme_bw() +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
            plot.subtitle = element_text(size = 10, family = "Roboto"),
            plot.caption = element_markdown(size = 8, family = "Roboto"),
            axis.title.x = element_text(size = 10, family = "Oswald"),
            axis.text.x = element_text(size = 8, family = "Roboto"),
            axis.title.y = element_text(size = 10, family = "Oswald"),
            axis.text.y = element_text(size = 8, family = "Roboto"),
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
             date <= "2025-06-30") %>%
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
             date <= "2025-06-30") %>%
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
             date <= "2025-06-30") %>%
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
             date <= "2025-06-30") %>%
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
  
  # creating data frame for top 10,000 terms in full corpus
  top10000 <- as.data.frame(topfeatures(dfm, 10000))
  top10000 <- rownames_to_column(top10000, "word")
  names(top10000) <- NULL
  names(top10000) <- c("word", "count")
  
  # finding which terms in word list are in top 10,000 terms
  
    words <- c("meat", "dairy", "livestock", "cow", "cows", "cattle", "beef", "sheep", "lamb", "chicken", "poultry",
               "hog", "pig", "pork", "rais", "consum", "eat", "methane", "animal", "agriculture", "animal agriculture",
               "farm", "factory farm", "diet", "consum.", "plant-based", "veg", "flexitarian", "seafood", "pescatarian",
               "fish", "vegan", "vegetarian")
    
    top10000 <- top10000 %>%
      mutate(agdiet = case_when(
        word %in% words ~ T,
             .default = F)) %>%
      rowid_to_column()
    
    # two words ranking above and below each `agdiet` term in top 10,000
    
      rankings <- as.data.frame(top10000[202:206,]) %>%     # "methane"
        add_row(as.data.frame(top10000[567:571,])) %>%      # "agriculture"
        add_row(as.data.frame(top10000[689:693,])) %>%      # "meat"
        add_row(as.data.frame(top10000[699:703,])) %>%      # "fish"
        add_row(as.data.frame(top10000[707:711,])) %>%      # "animal"
        add_row(as.data.frame(top10000[1318:1322,])) %>%    # "dairy"
        add_row(as.data.frame(top10000[1371:1375,])) %>%    # "cows"
        add_row(as.data.frame(top10000[1539:1543,])) %>%    # "beef"
        add_row(as.data.frame(top10000[1555:1559,])) %>%    # "cattle"
        add_row(as.data.frame(top10000[1571:1575,])) %>%    # "livestock"
        add_row(as.data.frame(top10000[1738:1742,])) %>%    # "plant-based"
        add_row(as.data.frame(top10000[1890:1894,])) %>%    # "chicken"
        add_row(as.data.frame(top10000[2576:2580,])) %>%    # "cow"
        add_row(as.data.frame(top10000[3154:3158,])) %>%    # "vegan"
        add_row(as.data.frame(top10000[4272:4276,])) %>%    # "pork"
        add_row(as.data.frame(top10000[4830:4834,])) %>%    # "sheep"
        add_row(as.data.frame(top10000[6025:6029,])) %>%    # "seafood"
        add_row(as.data.frame(top10000[6476:6480,])) %>%    # "poultry"
        add_row(as.data.frame(top10000[7056:7060,])) %>%    # "vegetarian"
        add_row(as.data.frame(top10000[7377:7381,]))        # "lamb"
    
      # write.csv(rankings, "rankings.csv")
    
# sentiment analysis (not included in written report)
  
  # cli
  cli_sent <- as.data.frame(cli |>
                              textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "1")
  
  summary(cli_sent$sentiment)
  sd(cli_sent$sentiment)
  
  # meat
  meat_sent <- as.data.frame(meat |>
                              textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "2")
  
  summary(meat_sent$sentiment)
  sd(meat_sent$sentiment)
  
  # diet
  diet_sent <- as.data.frame(diet |>
                              textstat_valence(dictionary = data_dictionary_AFINN)) %>%
    mutate(scope = "3")
  
  summary(diet_sent$sentiment)
  sd(diet_sent$sentiment)

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
           subtitle = "From 8,058 climate articles from July 2022 through June 2025",
           caption = cap) +
      scale_color_continuous(palette = pal2) +
      scale_size_continuous(range = c(3, 15)) +
      cloud
    
    print(top_plot)
    
    # ggsave("top_plot.jpg", plot = top_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
  
  # PLOT: word cloud of most frequent words near "meat"
  
    kwic.meat_plot <- ggplot(kwic.meat_top, aes(label = word, size = count, color = count)) +
      geom_text_wordcloud(family = "Oswald") +
      labs(title = "Keywords near \"meat\"",
           subtitle = "From 8,058 climate articles from July 2022 through June 2025",
           caption = cap) +
      scale_color_continuous(palette = pal2) +
      scale_size_continuous(range = c(3, 15)) +
      cloud
    
    print(kwic.meat_plot)
    
    # ggsave("kwic.meat_plot.jpg", plot = kwic.meat_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: word cloud of most frequent words near "agriculture"
    
    kwic.ag_plot <- ggplot(kwic.ag_top, aes(label = word, size = count, color = count)) +
      geom_text_wordcloud(family = "Oswald") +
      labs(title = "Keywords near \"agriculture\"",
           subtitle = "From 8,058 climate articles from July 2022 through June 2025",
           caption = cap) +
      scale_color_continuous(palette = pal2) +
      scale_size_continuous(range = c(3, 15)) +
      cloud
    
    print(kwic.ag_plot)
    
    # ggsave("kwic.ag_plot.jpg", plot = kwic.ag_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
  
  # PLOT: sentiment analysis across scopes (not included in written report)
    
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
    
    # ggsave("sent_plot.jpg", plot = sent_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: coverage of each scope over time
    
    months_plot <- ggplot(months_df, aes(x = month, y = count, color = scope)) +
      geom_step(linewidth = 0.8) +
      annotate("text", x = as.Date("2022-07-01"), y = 435, label = str_wrap("Europe heat waves", 10),
               family = "Oswald", size = 2.5, hjust = "center") +
      annotate("text", x = as.Date("2022-11-01"), y = 445, label = "C0P27",
               family = "Oswald", size = 2.5, hjust = "center") +
      annotate("text", x = as.Date("2023-08-01"), y = 352, label = str_wrap("Greece wildfires, record global temps, Storm Daniel", 20),
               family = "Oswald", size = 2.5, hjust = "center") +
      annotate("text", x = as.Date("2023-12-01"), y = 349, label = "COP28",
               family = "Oswald", size = 2.5, hjust = "center") +
      annotate("text", x = as.Date("2024-11-01"), y = 236, label = "COP29",
               family = "Oswald", size = 2.5, hjust = "center") +
      labs(title = "Three years of declining climate coverage",
           subtitle = str_wrap("Apart from spikes around news events, the volume of climate reporting has declined over
           the past three years. Animal agriculture and dietary change are mentioned in that coverage at a consistently low rate.", 95),
           caption = cap,
           x = "Month",
           y = "Number of articles",
           color = "") +
      scale_x_date(breaks = as.Date(c("2022-07-01", "2023-01-01", "2023-07-01", "2024-01-01", "2024-07-01", "2025-01-01", "2025-07-01")),
                   labels = c("July '22", "Jan. '23", "July '23", "Jan. '24", "July '24", "Jan. '25", "July '25")) +
      scale_color_discrete(breaks = c("cli", "meat", "diet"),
                           labels = c("climate", "+ meat", "+ diet"),
                           palette = pal3.1) +
      line
    
    print(months_plot)
    
    # ggsave("months_plot.jpg", plot = months_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)