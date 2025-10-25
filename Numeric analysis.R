# Analysis by Alexandra Tey, a journalist with the Population and Sustainability team at the Center for Biological Diversity.
  # Contact Alex at a.tey@pm.me or the PopSus team at PopSusColab@biologicaldiversity.org.
  # https://biologicaldiversity.org | https://alextey.co | https://github.com/teyalex

# setup

  rm(list = ls())
  library(tidyverse)
  library(ggtext)
  setwd("~/Documents/work/Center/Original analysis")
  
# styles
  
  # color palettes
  pal3 <- c("#A0E0AB","#ED3125", "#F3DDB1")
  pal2 <- c("#ED3125", "#F3DDB1")
  
  
  # caption text
  
  cap <- "Center for Biological Diversity/Brighter Green | *github.com/teyalex/an-ag*"
  
  # standard/horizontal geom_col plot
  h.col <- theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
          plot.subtitle = element_text(size = 10, family = "Helvetica"),
          plot.caption = element_markdown(size = 8, family = "Helvetica"),
          axis.title.y = element_text(size = 10, family = "Oswald"),
          axis.text.x = element_text(size = 8, family = "Oswald"),
          axis.text.y = element_text(size = 8, family = "Helvetica"),
          panel.grid.major.x = element_blank())
  
  # flipped/vertical geom_col plot, grid lines
  v.col <- theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
          plot.subtitle = element_text(size = 10, family = "Helvetica"),
          plot.caption = element_markdown(size = 8, family = "Helvetica"),
          axis.title.x = element_text(size = 10, family = "Oswald"),
          axis.text.x = element_text(size = 8, family = "Helvetica"),
          axis.text.y = element_text(size = 8, family = "Oswald"),
          panel.grid.major.y = element_blank())
  
  # tile plot
  tile <- theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(size = 10, family = "Oswald"),
          legend.text = element_text(size = 8, family = "Helvetica"),
          plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
          plot.subtitle = element_text(size = 10, family = "Helvetica"),
          plot.caption = element_markdown(size = 8, family = "Helvetica"),
          axis.title.x = element_text(size = 10, family = "Oswald"),
          axis.text.x = element_text(size = 8, family = "Helvetica"),
          axis.text.y = element_text(size = 8, family = "Oswald"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
# importing data downloaded from Factiva
  
  # companies
  
    # importing from cli scope
    cli_companies <- as.data.frame(read.csv("Factiva exports/rst=(…)/cli_Company.csv",
                                row.names = NULL, skip = 2) %>%
      rename(company = Company,
             count = Document.Count)) %>%
      mutate(count = as.numeric(count))
    cli_companies <- cli_companies[1:(nrow(cli_companies)-13),]
    
    # importing from meat scope
    meat_companies <- read.csv("Factiva exports/rst=(…)/meat_Company.csv",
                                   row.names = NULL, skip = 2) %>%
      rename(company = Company,
             count = Document.Count)
    meat_companies <- meat_companies[1:(nrow(meat_companies)-13),]
    meat_companies$count <- as.numeric(meat_companies$count)
    
    # importing from diet scope
    diet_companies <- read.csv("Factiva exports/rst=(…)/diet_Company.csv",
                                        row.names = NULL, skip = 2) %>%
      rename(company = Company,
             count = Document.Count)
    diet_companies <- diet_companies[1:(nrow(diet_companies)-13),]
    diet_companies$count <- as.numeric(diet_companies$count)
    
    # joining cli, meat, and diet scopes; labeling types of companies/institutions
    companies <- cli_companies %>%
      left_join(meat_companies, by = "company", suffix = c("_cli", "_cli.meat")) %>%
      left_join(diet_companies, by = "company") %>%
      rename(count_cli.meat.diet = count) %>%
      replace(is.na(.), 0) %>%
      mutate(type = case_when(grepl("United Nations|Intergovernmental|International|European|World|Group of|Countries|Treaty|Organis", company) & !grepl("Greenpeace|Nature", company) ~ "Intergovernmental org.",
                              grepl("Environmental Protection|Federal|Admin|\\bUS|\\bU.S.|United States|Comptroller|Centers|Internal|Safety", company) ~ "U.S. federal govt. body",
                              grepl("Minnesota|Illinois", company) ~ "U.S. state govt. body",
                              grepl("United Kingdom|British|Met Office", company) ~ "Non-U.S. govt. body",
                              grepl("Hamas|Taliban", company) ~ "Non-state actor",
                              grepl("Resources|Defense Fund|Club|Greenpeace|Earthjustice|Global|Center|Conservation|Working|Heritage", company) & !grepl("Centers", company) ~ "NGO",
                              grepl("College|Society|University|Concerned|Potsdam", company) ~ "University/scientific org.",
                              grepl("Corp|PLC|Inc|News Group|LLC|Conoco|Climeworks|& Co.|Georgia|Company|Edison|ENI|Morgan", company) ~ "Corporation",
                              grepl("Petroleum Institute|Glasgow|Asset|Insur", company) ~ "Trade assn.")) %>%
      rename(cli = count_cli,
             meat = count_cli.meat,
             diet = count_cli.meat.diet) %>%
      pivot_longer(cols = c("cli", "meat", "diet"),
                   names_to = "scope",
                   values_to = "count")
    
    companies_pct <- companies %>%
      group_by(type, scope) %>%
      summarize(count = sum(count)) %>%
      mutate(pct = case_when(scope == "cli" ~ count/10696,
                             scope == "meat" ~ count/343,
                             scope == "diet" ~ count/115))
    
    companies %>%
      pivot_wider(names_from = scope,
                  values_from = count) %>%
      write.csv("companies.csv")
    
  # industries
    
    # importing from cli scope
    cli_industries <- read.csv("Factiva exports/rst=(…)/cli_Industry.csv",
                               row.names = NULL, skip = 2) %>%
      rename(industry = Industry,
             count = Document.Count) %>%
      mutate(count = as.numeric(count))
    cli_industries <- cli_industries[1:(nrow(cli_industries)-13),]
    
    # importing from meat scope
    meat_industries <- read.csv("Factiva exports/rst=(…)/meat_Industry.csv",
                                    row.names = NULL, skip = 2) %>%
      rename(industry = Industry,
             count = Document.Count)
    meat_industries <- meat_industries[1:(nrow(meat_industries)-13),]
    meat_industries$count <- as.numeric(meat_industries$count)
    
    # importing from diet scope
    diet_industries <- read.csv("Factiva exports/rst=(…)/diet_Industry.csv",
                                         row.names = NULL, skip = 2) %>%
      rename(industry = Industry,
             count = Document.Count)
    diet_industries <- diet_industries[1:(nrow(diet_industries)-13),]
    diet_industries$count <- as.numeric(diet_industries$count)
    
    # joining cli, meat, and diet scopes; labeling industry sectors
    industries <- cli_industries %>%
      left_join(meat_industries, by = "industry", suffix = c("_cli", "_cli.meat")) %>%
      left_join(diet_industries, by = "industry") %>%
      rename(count_cli.meat.diet = count) %>%
      replace(is.na(.), 0) %>%
      mutate(sector = case_when(grepl("Vehicles|Cars|Automotive|Transport|Airlines|Aerospace|Airports", industry) ~ "Transportation",
                                grepl("Energy|Gas|Oil|Coal|Solar|Power|Fossil Fuel|Utilities|Fracking|Pipeline|Biofuels|Petro", industry) ~ "Energy, fossil fuels",
                                grepl("Industr|Mining|Chemical|Metals|Machinery|Forestry|Plastic|Material", industry) ~ "Extraction, manufacturing",
                                grepl("Building|Constr|Estate|Cement", industry) ~ "Construction, real estate",
                                grepl("Farm|Grow|Agriculture|Food|Wine|Fertilizers|Fishing", industry) ~ "Agriculture, food",
                                grepl("Carbon", industry) ~ "Carbon capture/storage",
                                grepl("Banking|Insurance|Financ|Invest|Funds|Equity", industry) ~ "Finance, insurance",
                                grepl("Technolog|Computer|Software|Online|Electrical|Data|Crypto", industry) ~ "Technology",
                                grepl("Pharmaceutical|Vaccine|Healthcare", industry) ~ "Health, pharmaceuticals",
                                grepl("Waste|Recycl", industry) ~ "Waste, recycling",
                                grepl("Retail|Consumer|Leisure", industry)~ "Consumer/retail, wholesale",
                                grepl("Media", industry) ~ "Media, entertainment")) %>%
      rename(cli = count_cli,
             meat = count_cli.meat,
             diet = count_cli.meat.diet) %>%
      select(industry, sector, cli, meat, diet)
    
    write.csv(industries, "industries.csv")
    
    # calculating table of totals by industry sector
    industry_totals <- industries %>%
      select(sector, industry, cli) %>%
      group_by(sector) %>%
      summarize(count = sum(cli)) %>%
      mutate(pct = count/10696,
             is.ag = case_when(
               sector == "Agriculture, food" ~ T,
               .default = F)) %>%
      arrange(desc(pct))
    
    # preparing long-format data frame of industry sector for plotting
    industries_long <- industries %>%
      select(sector, cli, meat, diet) %>%
      pivot_longer(cols = c("cli", "meat", "diet"),
                   names_to = "scope",
                   values_to = "count") %>%
      group_by(sector, scope) %>%
      summarize(count = sum(count)) %>%
      mutate(rel.pct = case_when(
        scope == "cli" ~ count / 10696,       # total number of articles in the "cli" scope
        scope == "meat" ~ count / 343,        # total number of articles in the "meat" scope
        scope == "diet" ~ count / 115)) %>%   # total number of articles in the "diet" scope
      arrange(sector)
    
  # sources

    # importing from cli scope
    cli_sources <- read.csv("Factiva exports/rst=(…)/cli_Source.csv",
                             row.names = NULL, skip = 2) %>%
      select(Source, Document.Count) %>%
      rename(source = Source,
             count = Document.Count) %>%
      slice(1:(n() - 13)) %>%
      mutate(count = as.numeric(count),
             across(starts_with("source"),
                    ~ str_trim(str_remove_all(.x, " - All sources|\\:.*|\\s*\\(.*?\\)|\\bOnline\\b|\\bMagazine\\b")))) %>%
      group_by(source) %>%
      summarize(count = sum(count))
    
    # importing from meat scope
    meat_sources <- read.csv("Factiva exports/rst=(…)/meat_Source.csv",
                            row.names = NULL, skip = 2) %>%
      select(Source, Document.Count) %>%
      rename(source = Source,
             count = Document.Count) %>%
      slice(1:(n() - 13)) %>%
      mutate(count = as.numeric(count),
             across(starts_with("source"),
                    ~ str_trim(str_remove_all(.x, " - All sources|\\:.*|\\s*\\(.*?\\)|\\bOnline\\b|\\bMagazine\\b")))) %>%
      group_by(source) %>%
      summarize(count = sum(count))
    
    # importing from diet scope
    diet_sources <- read.csv("Factiva exports/rst=(…)/diet_Source.csv",
                                      row.names = NULL, skip = 2) %>%
      select(Source, Document.Count) %>%
      rename(source = Source,
             count = Document.Count) %>%
      slice(1:(n() - 13)) %>%
      mutate(count = as.numeric(count),
             across(starts_with("source"),
                    ~ str_trim(str_remove_all(.x, " - All sources|\\:.*|\\s*\\(.*?\\)|\\bOnline\\b|\\bMagazine\\b")))) %>%
      group_by(source) %>%
      summarize(count = sum(count))
    
    # joining cli, meat, and diet scopes
    sources <- cli_sources %>%
      left_join(meat_sources, by = "source", suffix = c("_cli", "_cli.meat")) %>%
      left_join(diet_sources, by = "source") %>%
      rename(diet = count,
             meat = count_cli.meat,
             cli = count_cli) %>%
      replace(is.na(.), 0)
    
    # downloading table of counts by source
    write.csv(sources, "sources.csv")
    
    # calculating percentages by scope for plotting
    source.pcts <- sources %>%
      mutate(meat_pct = meat/cli,
             diet_pct = diet/cli) %>%
      pivot_longer(cols = c("meat_pct", "diet_pct"),
                   names_to = "pct_scope",
                   values_to = "pct")
    
# clearing temporary variables
    
  rm(list = ls(pattern = "cli_|meat_|diet_"))
    
# summary statistics

  # counting number of articles in each scope
    
    scopes <- sources %>% 
      pivot_longer(cols = c("cli", "meat", "diet"),
                   names_to = "scope",
                   values_to = "count") %>%
      group_by(scope) %>%
      summarize(total = sum(count)) %>%
      arrange(desc(total)) %>%
      mutate(pct = case_when(scope == "cli" ~ NA,
                             TRUE ~ total/10696))
    
  # number of stories in each scope
      
    # cli (all stories)
    scopes$total[scopes$scope == "cli"]
    
    # meat
    scopes$total[scopes$scope == "meat"]
    
    # diet
    scopes$total[scopes$scope == "diet"]
    
  # proportions of each scope
      
    # proportion of climate stories that mention meat or animal agriculture
    scopes$total[scopes$scope == "meat"] / scopes$total[scopes$scope == "cli"]
    
    # proportion of climate stories that mention dietary change
    scopes$total[scopes$scope == "diet"] / scopes$total[scopes$scope == "cli"]
    
    # proportion of meat stories that mention dietary change
    scopes$total[scopes$scope == "diet"] / scopes$total[scopes$scope == "meat"]
  
  # sources
    
    # number of sources
    nrow(sources)
    
    # median number of articles per source
    median(sources$cli)
    
    # median number of meat articles per source
    median(sources$meat)
    
    # distribution of meat article percentages
      source.pcts %>%
        filter(pct_scope == "meat_pct") %>%
        pivot_wider(names_from = pct_scope,
                    values_from = pct) %>%
        summarize(mean = mean(meat_pct),
                  median = median(meat_pct),
                  sd = sd(meat_pct))
    
# plotting
      
  # PLOT: comparing scopes
      
    scopes_plot <- ggplot(scopes, aes(x = reorder(scope, -total), y = total, fill = scope)) +
      geom_col(width = 0.4) +
      geom_text(aes(label = scales::percent(pct)),
                vjust = -0.3,
                hjust = "center",
                size = 4,
                family = "Oswald") +
      labs(title = "Climate coverage neglects animal agriculture and dietary change",
           subtitle = str_wrap("In  sample of 10,696 climate change articles, 343 mentioned meat and animal agriculture,
                               and 115 mentioned dietary change.",
                               80),
           caption = cap,
           x = "",
           y = "Number of articles") +
      scale_fill_manual(values = pal3,
                        breaks = c("cli", "meat", "diet")) +
      scale_x_discrete(breaks = c("cli", "meat", "diet"),
                       labels = c("All climate stories", "Mentioning meat/animal agriculture", "Mentioning dietary change")) +
      h.col
    
    print(scopes_plot)
    
    ggsave("scopes_plot.jpg", plot = scopes_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: comparing company types
    
    companies_plot <- ggplot(companies_pct[which(companies_pct$pct > 0),], aes(x = reorder(type, pct), y = pct, fill = reorder(scope, pct))) +
      geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
      coord_flip() +
      labs(title = "Institution types by scope",
           subtitle = str_wrap("Corporations were mentioned more frequently than other types of institutions
                               in articles that mentioned meat, but much less frequently in articles that
                               also mentioned dietary change.",
                               85),
           caption = cap,
           x = "",
           y = "Frequency",
           fill = "Scope") +
      scale_fill_manual(values = pal3,
                        labels = c("Climate", "Climate + meat", "Climate + meat + diet")) +
      scale_y_continuous(labels = scales::label_percent(),
                         position = "right") +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 18, family = "Oswald"),
            plot.subtitle = element_text(size = 10, family = "Helvetica"),
            plot.caption = element_markdown(size = 8, family = "Helvetica"),
            legend.title = element_text(size = 10, family = "Oswald"),
            legend.text = element_text(size = 8, family = "Helvetica"),
            axis.title.x = element_text(size = 10, family = "Oswald"),
            axis.title.y = element_text(size = 10, family = "Oswald"),
            axis.text.x = element_text(size = 8, family = "Helvetica"),
            axis.text.y = element_text(size = 8, family = "Oswald"),
            panel.grid.major.y = element_blank())
    
    print(companies_plot)
    
    ggsave("companies_plot.jpg", plot = companies_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: comparing totals by source
    
    sources_plot <- ggplot(sources %>%
                             rename (a_cli = cli,
                                     b_meat = meat,
                                     c_diet = diet) %>%
                             pivot_longer(cols = c("a_cli", "b_meat", "c_diet"),
                                                    names_to = "scope",
                                                    values_to = "count"),
                           aes(x = reorder(source, count), y = count, fill = scope)) +
      geom_col(position = position_dodge(preserve = "single", reverse = T)) +
      coord_flip() +
      labs(title = "Rates of coverage by outlet",
           subtitle = str_wrap("All publications infrequently mentioned meat or animal agriculture and dietary change in their climate coverage.", 75),
           caption = cap,
           x = "",
           y = "Number of articles",
           fill = "") +
      scale_fill_manual(values = pal3,
                        breaks = c("a_cli", "b_meat", "c_diet"),
                        labels = c("All climate articles", "Meat mentions", "Diet mentions")) +
      scale_y_continuous(position = "right") +
      v.col
    
    print(sources_plot)
    
    ggsave("sources_plot.jpg", plot = sources_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: comparing mention percentages by source
    
    sources.pcts_plot <- ggplot(source.pcts[which(source.pcts$pct > 0),] %>%
                                  filter(cli > 5),
                                aes(x = reorder(source, cli), y = pct, fill = pct_scope)) +
      geom_col(position = position_dodge(preserve = "single")) +
      coord_flip() +
      labs(title = "An outlier outlet",
           subtitle = str_wrap("Fox News mentioned meat in 30% of its 56 climate articles. (Publications with fewer than five
                               articles are omitted here.)", 80),
           caption = cap,
           x = "",
           y = "Frequency in climate stories",
           fill = "") +
      scale_fill_manual(values = pal2,
                        breaks = c("meat_pct", "diet_pct"),
                        labels = c("Meat mentions", "Diet mentions")) +
      scale_y_continuous(labels = scales::label_percent(),
                         position = "right") +
      v.col

    print(sources.pcts_plot)
    
    ggsave("sources.pcts_plot.jpg", plot = sources.pcts_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
  
  # PLOT: comparing sectors in cli scope
    
    industries_plot <- ggplot(industry_totals, aes(x = reorder(sector, count), y = count, fill = is.ag)) +
      geom_col() +
      geom_text(aes(label = scales::percent(count/10696, accuracy = 1)),
                family = "Oswald",
                size = 4,
                hjust = 0.25) +
      coord_flip() +
      labs(title = "Sectors mentioned in climate coverage",
           caption = cap,
           fill = "",
           x = "") +
      scale_y_continuous(name = "Number of articles",
                         position = "right",
                         sec.axis = sec_axis(transform = ~./10696,
                                             name = "Percentage of total",
                                             labels = scales::label_percent())) +
      scale_fill_manual(values = pal2,
                        breaks = c(T, F)) +
      v.col +
      theme(legend.position = "none")
    
    print(industries_plot)
    
    ggsave("industries_plot.jpg", plot = industries_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)
    
  # PLOT: comparing sectors across scopes
    
    industries_2_plot <- ggplot(industries_long, aes(x = reorder(scope, -count), y = reorder(sector, desc(sector)), fill = rel.pct, label = count)) +
      geom_tile() +
      geom_text() +
      labs(title = "Frequency of sector mentions within each scope",
           subtitle = str_wrap("Even in the sample of articles that mention animal agriculture, specific agricultural industries
                               are rarely mentioned.", 80),
           x = "Scope",
           y = "",
           caption = cap,
           fill = "Frequency") +
      scale_x_discrete(breaks = c("cli", "meat", "diet"),
                       labels = c("Climate (10,696 articles)", "Meat (343)", "Diet change (115)"),
                       position = "top") +
      scale_fill_gradientn(colors = c("#FFFFFF", "#FAF1E0", "#ED3125"),
                           values = scales::rescale(c(0, 0.003, max(industries_long$count))),
                           labels = scales::label_percent()) +
      tile
    
    print(industries_2_plot)
    
    ggsave("industries_2_plot.jpg", plot = industries_2_plot, width = 6.5, height = 5.2, units = "in", dpi = 320)