library(tidyverse)
library(xlsx)
library(devtools)
library(ggsoccer)
library(ggrepel)
library(RPostgres)
library(lubridate)
library(ggimage)
library(ggdark)
library(glue)
library(ggtext)
library(patchwork)
library(showtext)
library(readxl)
font_add_google("Alatsi", "alatsi")
showtext_auto()


  # Read from file ----
  single_game_shots <- read_excel("~/R/avlgit/asheville_city/Mock Data/ACSC2022.xlsx", sheet = "Example_shooting")
  asp_ratio <- 1.618 #set y x ratio
  
  # Create variables ----
  # * ENTER DATA HERE ----
  xg_query <- filter(single_game_shots, Game_ID == 2 & M_W == "M")
  m_w <- "M"
  half_time <- 47
  full_time <- 97
  away_team_name <- "Asheville City"
  home_team_name <- "One Knoxville SC"
  date_for_plot <- "May 15, 2022"
  # Used for saving file
  date_label <- "5/15/2022"
  
  
  ## Find final score ----
  home_team_final_score <- sum(filter(xg_query, Home_Team == 1)$Outcome == "Goal")
  away_team_final_score <- sum(filter(xg_query, Home_Team == 0)$Outcome == "Goal")

  ## Create team table and placeholder for incremental xG
  teams <- distinct(xg_query, Team, Home_Team, .keep_all = F)
  
  start_rows <- data.frame(
    Team = teams$Team,
    Match_Half = c(1), 
    Minute = c(0), 
    expanded_minute = c(0),
    xG_calvaney = c(0)
  )

  large_df <- bind_rows(xg_query, start_rows) 
  
  ## Order large_df by minute 
  large_df <- large_df %>%
    group_by(Team) %>%
    arrange(Minute) %>%
    mutate(
      cum_xg = cumsum(xG_calvaney), 
      Shooting_Player = ifelse(
        Shot_Type == "Penalty", 
        paste0(Shooting_Player, " (PK)"), 
        Shooting_Player
      )
    ) 
  
  # Create final row ----
  # We do this to plot extends to end of game
  end_xg <- slice_tail(large_df)
  end_rows <- data.frame(
    Team = end_xg$Team,
    Half = c(max(end_xg$Match_Half)),
    Minute = full_time,
    cum_xg = end_xg$cum_xg
  )
  
  large_df <- bind_rows(large_df, end_rows)

  # Goals ----
  goals <- large_df %>%
    filter(Outcome == "Goal") %>%
    mutate(
      image_path = paste0("<img src='", "~/R/avlgit/asheville_city/images/icons8-soccer-ball-24.png", "' width = '12.5' height = '12.5'/>")
    )

  max_xg <- round(max(large_df$cum_xg), 1)

  home_xg <- large_df %>%
    filter(Home_Team ==1) %>%
    mutate(cum_xg = round(cum_xg, 2)) %>%
    tail(1)
  
  away_xg <- large_df %>%
    filter(Home_Team ==0) %>%
    mutate(cum_xg = round(cum_xg, 2)) %>%
    tail(1)

  
  # Colors ----
  # * ENTER COLORS HERE ----
  away_color <- "#2e334e"
  home_color <- "#993333" 
 
  # Create palette    
  pal <- c(away_color, home_color)

  
  # Labels ----
  home_team_label <- paste0(home_team_final_score, " G, ", home_xg$cum_xg, " xG")
  away_team_label <- paste0(away_team_final_score, " G, ", away_xg$cum_xg, " xG")
 
  # Create file name for output
  file_name <- paste0(
    str_replace_all(date_label, fixed("/"),""),
    str_replace_all(str_replace_all(home_team_name, " ", ""), "/", ""), 
    "v", 
    str_replace_all(str_replace_all(away_team_name, " ", ""), "/", ""),
    ".png")
  
  # Plot ----
  p2 <- ggplot(large_df) +
    geom_step(aes(x = Minute, y = cum_xg, group = Team, colour = Team), size = 1) +
    scale_colour_manual(values = pal) +
    geom_text_repel(
      data = goals, 
      aes(x = Minute, y = cum_xg, label = Shooting_Player),
      force = TRUE, box.padding = unit(1, "lines"),
      family = "alatsi",
      fontface = "bold",
      nudge_y = 0.05, 
      nudge_x = -6,
      size = 5,
      color="#000000",
      direction = "both"
    ) +
    geom_richtext(
      data = goals, 
      aes(x = Minute, y = cum_xg, label = image_path),
      fill = NA,
      colour = NA,
      label.padding = grid::unit(rep(0, 4), "pt")
    ) +
    geom_vline(xintercept = half_time, size = 0.75, colour = "grey", linetype = "dashed") + 
    annotate(
      "text",
      x = full_time + 5,
      y = home_xg$cum_xg ,
      label = home_team_label,
      family = "alatsi",
      fontface = "bold", 
      color=home_color,
      size = 5
    ) +
    annotate(
      "text",
      x = full_time + 5,
      y = away_xg$cum_xg,
      label = away_team_label,
      family = "alatsi",
      color=away_color,
      fontface = "bold",
      size = 5
    ) +
    scale_x_continuous(breaks = seq(0, full_time, 15), labels = seq(0, full_time, 15)) +
    scale_y_continuous(breaks = seq(0, max_xg, 0.2), labels = scales::label_number(accuracy = 0.1)) +
    labs(x = "Time (min)",
         y = "Cumulative xG", 
         colour = "000000") +
    theme_light() +
    labs(
      # Format title ----
      # Add color to team names
      title = str_glue(
        "<span style='color:{away_color};'>{away_team_name}
         <span style='color:#000000;'>vs.
         <span style='color:{home_color};'>{home_team_name}</span>
         </span>"),
      # Add subtitle ----
      # Date and final score
      subtitle = paste0(date_for_plot, "\n", away_team_final_score, " - ", home_team_final_score)
    ) +
    theme(text = element_text(family = "alatsi", face = "bold"),
          legend.position = "none",
          panel.background = element_rect((fill="grey95")),
          #panel.grid.major = element_line(size = 0.25, colour = "grey"),
          #panel.grid.minor = element_line(colour = "#383636"),
          #plot.background = element_rect(fill = "#cfcfcf", colour = "#cfcfcf"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          plot.title = element_markdown(hjust = 0.5, size = 32),
          plot.subtitle = element_text(hjust = 0.5, size = 25)
    ) 
  
  showtext_opts(dpi = 300) 
  # Save file ----
  ggsave(paste0("race_charts/", m_w, "/", file_name), plot = p2, height = 7.93, width = 8.40 * asp_ratio)

