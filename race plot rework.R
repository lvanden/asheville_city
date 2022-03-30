#we need to load the functions (this needs to be done every time you want to run your script)
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

#to read from csv
singleGameShots<-read.csv("~/asheville_city/Mock Data/Example_shooting.csv")
##########################################
##create variables 
  
create_race_plot <- function(Game_ID = 1) {
  set.seed(35) #random number generator 
  asp_ratio <- 1.618 #set y x ratio
  # browser
  
  ##filter csv to Game ID
  xg_query <- filter(singleGameShots,Game_ID=={Game_ID})
  
  xg_query <- filter(singleGameShots,Game_ID==1)
  

 
  
  ##find half time and full time - max minute in period 1 or period 2
  ##this is not half time, this was the time of the last shot in the half
  
  half_time <- max(xg_query[xg_query$Match_Half==1,]$Minute)
  full_time<- max(xg_query[xg_query$Match_Half==2,]$Minute)
  
  ##find final score
  
  home_team_final_score<-sum( filter(xg_query,Home_Team==1)$Outcome =="Goal" )
  away_team_final_score<-sum( filter(xg_query,Home_Team==0)$Outcome =="Goal" )


  
  ###create team table and placeholder for incremental xG
  
  teams <- distinct(xg_query, Team,Home_Team, .keep_all = F)
  

  
  game_info <-data.frame(
    home_team_name = c(0),
    away_team_name =c(0))
  
  game_info %>%
    mutate(home_team_name = case_when(
      teams$Home_Team==1 ~ teams$Team,TRUE~"")) %>%
   mutate( away_team_name = case_when(
      teams$Home_Team==0 ~ Team,
      TRUE~ ""))
  
    
    
    
  
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
  
  ##create table for sum xg  
  end_xg <- slice_tail(large_df)
  end_rows <- data.frame(Team = end_xg$Team,
                         Half = c(max(end_xg$Match_Half)),
                         end_minute=full_time,
                         cum_xg = end_xg$cum_xg
  )
  

  
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
 
##add colors  
  # colors <- read_csv("/Users/arielledror/Downloads/team_info_nwsl.csv") %>%
  #   mutate(of_interest = str_extract(ggplot2_dark, "[^= ]*$"),
  #          of_interest = str_replace_all(of_interest, "'", ""), 
  #          of_interest = if_else(team_abbreviation == "NC", "#b7a369", of_interest),
  #          dark_color = if_else(team_abbreviation == "NC", "#b7a369", dark_color),
  #          of_interest = if_else(team_abbreviation == "UTA", "#FFB81C", of_interest))
  
  
  # pal <- colors$of_interest
  # names(pal) <- colors$team_name
  
###################################
## Missing Values
  #dates, game_info$home_team_name , images, goals,  
  
 #  pretty_date <- lubridate::stamp("January 1, 2021")
 # time_zone <- colors$time_zone[game_info$home_team_id == colors$team_id]
  
  #change_tz <- with_tz(ymd_hms(game_info$date_time_utc, tz = "UTC"), tz = time_zone)
 # print(change_tz)
 # date_label <- pretty_date(as.Date(change_tz, tz = time_zone))
 # date_name <- stringr::str_replace_all(change_tz, "-", "")
  
  
  title_text <- paste0(game_info$home_team_name, " - ", game_info$away_team_name,
                       "\n", date_label)
  
  file_name <- paste0(
    str_replace_all(str_replace_all(game_info$home_team_name, " ", ""), "/", ""), 
    "v", 
    str_replace_all(str_replace_all(game_info$away_team_name, " ", ""), "/", ""), 
    date_label,
    ".png")
  
  file_name_outcome <- paste0(
    str_replace_all(str_replace_all(game_info$home_team_name, " ", ""), "/", ""), 
    "v", 
    str_replace_all(str_replace_all(game_info$away_team_name, " ", ""), "/", ""),
    date_label,
    "outcome_prob.png")
  
  max_xg <- round(max(large_df$cum_xg), 1)
  

  ##this adds image and image path to goal lines
  goals <- large_df %>%
    filter(Outcome == "Goal") %>%
    mutate(
      image = "image",    
      image_path ="image"
    )
  
  p2 <- ggplot(large_df) +
    geom_step(aes(x = Minute, y = cum_xg, group = Team, colour = Team), size = 1) +
   # scale_colour_manual(values = pal) +
    geom_text_repel(data = goals, aes(x = Minute, y = cum_xg, label = Shooting_Player),
                    force = TRUE, box.padding = unit(1, "lines"),
                    family = "Big Shoulders Display", fontface = "bold",
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
      x = full_time + 5,
      y = home_xg$cum_xg,
      label = home_team_label,
      family = "Big Shoulders Display",
      fontface = "bold", 
      size = 5
    ) +
    annotate(
      "text",
      x = full_time + 5,
      y = away_xg$cum_xg,
      label = away_team_label,
      family = "Big Shoulders Display",
      fontface = "bold",
      size = 5
    ) +
    scale_y_continuous(breaks = seq(0, max_xg, 0.2), labels = seq(0, max_xg, 0.2)) +
    scale_x_continuous(breaks = seq(0, full_time, 15), labels = seq(0, full_time, 15)) +
    labs(x = "TIME",
         y = "CUMULATIVE xG", 
         colour = "") +
    theme_light() +
    theme(text = element_text(family = "Big Shoulders Display", face = "bold"),
          legend.position = c(.11, .91),
          legend.title = element_blank(),
          legend.background = element_rect(colour = NA, fill = NA),
          legend.direction = "vertical",
          legend.margin = margin(0, 0, 0, 0, unit = "pt"),
          legend.justification = "center",
          legend.text = element_text(size = 12),
          panel.grid.major = element_line(size = 0.25, colour = "grey"),
          panel.grid.minor = element_line(colour = "#383636"),
          plot.background = element_rect(fill = "#262525", colour = "#262525"),
          axis.title = element_text(size = 16), 
          axis.text = element_text(size = 12)
    ) 
  
  
  p2_mod <- p2 + 
    labs(title = title_text,
         caption = paste("caption")) + 
    theme(plot.title = element_text(hjust = 0.5, size = 24)) + 
    ggsave(paste0("race_charts/", file_name), height = 7.93, width = 6.22 * asp_ratio)
  
  
  final_p <- p2/p1 + 
    plot_layout(heights = unit(c(13, 1), c("cm", "null"))) + 
    plot_annotation(title = title_text,
                    caption = paste("caption", sep = "\n")) &
    theme(plot.title = element_text(hjust = 0.5, size = 24), 
          plot.background = element_rect(fill = "#262525"),
          plot.caption = element_text(size = 12),
          text = element_text(family = "Big Shoulders Display", face = "bold", colour = "white")
    ) 
  
  cowplot::save_plot(paste0("race_charts/", file_name_outcome), final_p, base_height = 7.93, base_asp = 16/9)
  print(final_p)
  print(p2_mod)
  
  return(final_p)
}

  



