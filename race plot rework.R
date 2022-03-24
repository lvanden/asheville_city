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
singleGameShots<-read.csv("C:/Users/samrc/OneDrive/Documents/Mock Data/Example_shooting.csv")
##########################################

  
create_race_plot <- function(Game_ID) {
  set.seed(35) #random number generator 
  asp_ratio <- 1.618 #set y x ratio
  # browser
  
  ##filter csv to Game ID
  xg_query <- filter(singleGameShots,Game_ID=={Game_ID})
  
  ##remove this- just for testing purposes
  xg_query <- filter(singleGameShots,Game_ID==1)
  
  ##find half time and full time - max minute in period 1 or period 2
  
  half_time <- max(xg_query[xg_query$Match_Half==1,]$Minute)
  
  full_time<- max(xg_query[xg_query$Match_Half==2,]$Minute)
  
  
  ###create team table and placeholder for incremental xG
  
  teams <- distinct(xg_query, Team,Home_Team, .keep_all = F)
  
  game_info <- teams %>% 
    mutate(Home_Team = case_when(
      Home_Team == 1 ~ glue::glue("Home Team"),
      TRUE ~ "Away Team"))
  
  start_rows <- data.frame(Team = game_info$Team,
                           Match_Half = c(1), 
                           Minute = c(0), 
                           expanded_minute = c(0),
                           xg_team = c(0)
  )
  
  ## I am not sure large_df is in the right format at this point
  
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
  
  
  
  View(large_df)
  

  
  home_xg <- large_df %>%
    filter(Home_Team ==1) %>%
    mutate(cum_xg = round(cum_xg, 2)) %>%
    tail(1)
  
  away_xg <- large_df %>%
    filter(Home_Team ==0) %>%
    mutate(cum_xg = round(cum_xg, 2)) %>%
    tail(1)
  
  
}


