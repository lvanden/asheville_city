library(tidyverse)
library(xlsx)
library(devtools)
library(ggsoccer)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(RPostgres)
library(lubridate)
library(ggimage)
library(ggdark)
library(glue)
library(ggtext)
library(patchwork)
library(showtext)
font_add_google("Roboto", "roboto")


#to read from csv
singleGameShots<-read.csv("~/R/avlgit/asheville_city/Mock Data/Example_shooting.csv")
asp_ratio <- 1.618 #set y x ratio

  
#Variables
  xg_query <- filter(singleGameShots,Game_ID==2)
  half_time <- 46
  full_time<- 93
  home_team_name="Asheville City"
  away_team_name="Wolves"
  date_label="4/5/2022"
  
  ##find final score
  
  home_team_final_score<-sum( filter(xg_query,Home_Team==1)$Outcome =="Goal" )
  away_team_final_score<-sum( filter(xg_query,Home_Team==0)$Outcome =="Goal" )


  ###create team table and placeholder for incremental xG
  
  teams <- distinct(xg_query, Team,Home_Team, .keep_all = F)
  
    
  start_rows <- data.frame(Team = teams$Team,
                           Match_Half = c(1), 
                           Minute = c(0), 
                           expanded_minute = c(0),
                           xG = c(0)
  )

  
  large_df <- bind_rows(xg_query, start_rows) 
  
  
##order large_df by minute  
  large_df <- large_df %>%
    group_by(Team) %>%
    arrange(Minute) %>%
    mutate(cum_xg = cumsum(xG), 
           Shooting_Player = if_else(Shot.Type== "Penalty", paste0(Shooting_Player, " (PK)"), Shooting_Player)
    ) 
  
  # ##create table for sum xg  
  # end_xg <- slice_tail(large_df)
  # end_rows <- data.frame(Team = end_xg$Team,
  #                        Half = c(max(end_xg$Match_Half)),
  #                        end_minute=full_time,
  #                        cum_xg = end_xg$cum_xg
  # )
  
  ##goals
  goals <- large_df %>%
    filter(Outcome == "Goal") %>%
    mutate(
      image = "image",    
      image_path ="image"
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
  

  #############################################

  home_team_label <- paste0(home_team_final_score, " G, ", home_xg$cum_xg, " xG")
  away_team_label <- paste0(away_team_final_score, " G, ", away_xg$cum_xg, " xG")
 
 
  title_text <- paste0(home_team_name, " vs ", away_team_name,
                       " - ", date_label)
  
  
  file_name <- paste0(
    str_replace_all(date_label, fixed("/"),""),
    str_replace_all(str_replace_all(home_team_name, " ", ""), "/", ""), 
    "v", 
    str_replace_all(str_replace_all(away_team_name, " ", ""), "/", ""),
    ".png")
  
  
  p2 <- ggplot(large_df) +
    geom_step(aes(x = Minute, y = cum_xg, group = Team, colour = Team), size = 1) +
    geom_text_repel(data = goals, aes(x = Minute, y = cum_xg, label = Shooting_Player),
                    force = TRUE, box.padding = unit(1, "lines"),
                    family = "roboto", fontface = "bold",
                    nudge_y = 0.05, nudge_x = -6,
                    size = 5,
                    direction = "both") +
    geom_richtext(data = goals, aes(x = Minute, y = cum_xg, label = image_path),
                  fill = NA, 
                  colour = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")) +
    geom_vline(xintercept = half_time, size = 0.75, colour = "grey", linetype = "dashed") + 
    annotate(
      "text",
      x = full_time - 4,
      y = home_xg$cum_xg+0.1,
      label = home_team_label,
      family = "roboto",
      fontface = "bold", 
      size = 6
    ) +
    annotate(
      "text",
      x = full_time - 4,
      y = away_xg$cum_xg+0.1,
      label = away_team_label,
      family = "roboto",
      fontface = "bold",
      size = 6
    ) +
    scale_y_continuous(breaks = seq(0, max_xg, 0.5), labels = seq(0, max_xg, 0.5)) +
    labs(x = "Time",
         y = "Cumulative xG", 
         colour = "") +
    dark_theme_minimal() +
    theme(text = element_text(family = "roboto", face = "bold"),
          legend.position = c(.11, .91),
          legend.title = element_blank(),
          legend.background = element_rect(colour = NA, fill = NA),
          legend.direction = "vertical",
          legend.margin = margin(0, 0, 0, 0, unit = "pt"),
          legend.justification = "center",
          legend.text = element_text(size = 12),
         # panel.grid.major = element_line(size = 0.25, colour = "grey"),
          panel.grid.minor = element_line(colour = "#383636"),
          plot.background = element_rect(fill = "#262525", colour = "#262525"),
          axis.title = element_text(size = 16), 
          axis.text = element_text(size = 12)
    ) 
  
  
  p2_mod <- p2 + 
    labs(title = title_text) + 
    theme(plot.title = element_text(hjust = 0.5, size = 32))
  
  showtext_opts(dpi = 300) 
  ggsave(paste0("race_charts/",file_name), plot=p2_mod,height = 7.93, width = 6.22 * asp_ratio)


