library(RPostgres)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggimage)
library(ggdark)
library(glue)
library(ggtext)
library(patchwork)


# addr <- "https://icon-library.com/images/soccer-ball-icon-png/soccer-ball-icon-png-3.jpg"
# 
# image_background(image_colorize(image_read(addr), color = "#b7a369", opacity = 100), "white", flatten = T)
# conn <- dbConnect(Postgres(),
#                   user = getOption("asa_user"),
#                   password = getOption("asa_password"),
#                   host = getOption("asa_host"),
#                   port = 25060,
#                   dbname = getOption("asa_db_name"),
#                   sslmode = "require")
# 
# colors <- read_csv("/Users/arielledror/Downloads/team_info_nwsl.csv") %>%
#   mutate(of_interest = str_extract(ggplot2_dark, "[^= ]*$"),
#          of_interest = str_replace_all(of_interest, "'", ""), 
#          of_interest = if_else(team_abbreviation == "NC", "#b7a369", of_interest),
#          dark_color = if_else(team_abbreviation == "NC", "#b7a369", dark_color),
#          of_interest = if_else(team_abbreviation == "UTA", "#FFB81C", of_interest))


# pal <- colors$of_interest
# names(pal) <- colors$team_name
# 
# simulate_games <- function(shots = c()) {
#   goals <- 0
#   for (shot in shots){
#     if (runif(1)<=shot){
#       goals <- goals + 1
#     }
#   }
#   #Finally, return the number of goals
#   return(goals)
# }
# 
# num_games <- function(shots, num_sims) {
#   hold_df <- c() 
#   for(i in 1:num_sims) {
#     one_sim <- simulate_games(shots)
#     hold_df <- c(hold_df, one_sim)
#   }
#   
#   return(hold_df)
# }
# 
# match_winner <- function(home_shots, away_shots) {
#   home_goals <- 0
#   away_goals <- 0
#   home_goals <- simulate_games(home_shots)
#   away_goals <- simulate_games(away_shots)
#   
#   if(home_goals > away_goals){
#     return("home")
#   } else if(away_goals > home_goals) {
#     return("away")
#   } else {
#     return("draw")
#   }
#   
# }
# 
# win_percent <- function(sims = 100, 
#                         home_team_shots, 
#                         away_team_shots) {
#   home <- 0
#   away <- 0
#   draw <- 0
#   
#   for(i in 1:sims){
#     winner <- match_winner(home_shots = home_team_shots, away_shots = away_team_shots)
#     
#     if(winner == "home"){
#       home <- home + 1
#     } else if (winner == "away") {
#       away <- away + 1
#     } else {
#       draw <- draw + 1
#     }
#   }
#   
#   results <- data.frame(
#     team = c("home", "away", "draw"),
#     num_sims = c(home, away, draw), 
#     simulations = sims
#   ) 
#   
#   return(results)
# }



create_race_plot <- function(game_id) {
  set.seed(35) #random number generator 
  asp_ratio <- 1.618 #set y x ratio
  # browser
  xg_query <- glue("
 SELECT
   players.player_name, 
   teams.team_name, 
   xgoals.*, 
   events.period_id,
   events.minute, 
   events.second, 
   events.expanded_minute
 FROM nwsl.xgoals
 LEFT JOIN nwsl.events
  ON events.game_id = xgoals.game_id
  AND events.team_id = xgoals.team_id
  AND events.event_id = xgoals.event_id
  AND events.player_id = xgoals.shooter_id
 LEFT JOIN all_opta.teams
   ON teams.team_id = xgoals.team_id
 LEFT JOIN all_opta.players
   ON players.player_id = xgoals.shooter_id
 WHERE xgoals.game_id = {game_id}
")
  
  # xg <- dbGetQuery(conn, xg_query) 
  # 
  # own_goal_query <- glue("
  #   SELECT
  #     players.player_name, 
  #     at.team_name AS away_team_name, 
  #     games.away_team_id, 
  #     games.home_team_id,
  #     ht.team_name AS home_team_name, 
  #     player_team.team_name AS player_team, 
  #     player_team.team_id,
  #     events.period_id,
  #     events.minute, 
  #     events.second, 
  #     events.expanded_minute
  #   FROM nwsl.events
  #   LEFT JOIN all_opta.teams AS player_team
  #     ON player_team.team_id = events.team_id
  #   LEFT JOIN all_opta.players
  #     ON players.player_id = events.player_id
  #   LEFT JOIN nwsl.games
  #     ON events.game_id = games.game_id
  #   LEFT JOIN all_opta.teams AS ht
  #     ON games.home_team_id = ht.team_id
  #   LEFT JOIN all_opta.teams AS at
  #     ON games.away_team_id = at.team_id
  #   WHERE own_goal = TRUE 
  #     AND events.game_id = {game_id}
  #     AND type_id = 16
  #   ")
  # own_goal <- dbGetQuery(conn, own_goal_query) 
  
  # if(nrow(own_goal) > 0){
  #   own_goal <- own_goal %>%
  #     mutate(team_id = if_else(team_id == home_team_id, away_team_id, home_team_id),
  #            team_name = if_else(player_team == home_team_name, away_team_name, home_team_name),
  #            player_name = paste(player_name, "()", sep = " "), 
  #            xg_team = 0, 
  #            goal = TRUE) %>%
  #     select(player_name,
  #            team_name, 
  #            team_id, 
  #            period_id, 
  #            minute,
  #            second, 
  #            expanded_minute, 
  #            xg_team, 
  #            goal)
  # }
  
  
  
  game_query <- glue("
  SELECT
    teams_home.team_name AS home_team_name, 
    teams_away.team_name AS away_team_name, 
    games.*
  FROM nwsl.games
  LEFT JOIN all_opta.teams AS teams_home
    ON teams_home.team_id = games.home_team_id
  LEFT JOIN all_opta.teams AS teams_away
    ON teams_away.team_id = games.away_team_id
  WHERE game_id = {game_id}
")
  game_info <- dbGetQuery(conn, game_query)
  
  half_time <- glue("SELECT
                    period_id,
                    MAX(expanded_minute) AS half_end
                    FROM nwsl.events
                    WHERE game_id = {game_id}
                      AND period_id IN (1, 2, 3, 4)
                    GROUP BY 1
                    ") 
  
  # half_time_query <- dbGetQuery(conn, half_time)
  # 
  # if(game_id == 2194623){
  #   half_time_query$half_end[half_time_query$period_id == 2] <- 97
  # } else if(game_id == 2194625){
  #   half_time_query$half_end[half_time_query$period_id == 1] <- 47
  # } 
  
  end_minute <- half_time_query$half_end[half_time_query$period_id == max(half_time_query$period_id)]
  
  teams <- distinct(xg, team_name, team_id, .keep_all = F)
  
  start_rows <- data.frame(team_name = teams$team_name,
                           team_id = teams$team_id,
                           period_id = c(1), 
                           minute = c(0), 
                           second = c(0), 
                           expanded_minute = c(0),
                           xg_team = c(0)
  )
  
  large_df <- bind_rows(xg, start_rows) 
  
  # print(nrow(own_goal))
  # 
  # if(nrow(own_goal) > 0){
  #   large_df <- bind_rows(large_df, own_goal)
  # }

  large_df <- large_df %>%
    group_by(team_name) %>%
    arrange(period_id, minute, second) %>%
    mutate(cum_xg = cumsum(xg_team), 
           minute_second = expanded_minute + (second * 0.01),
           player_name = if_else(pattern_of_play == "Penalty", paste0(player_name, " (PK)"), player_name)
           ) 
  
  end_xg <- slice_tail(large_df)
  end_rows <- data.frame(team_name = end_xg$team_name, 
                         team_id = end_xg$team_id, 
                         period = c(max(end_xg$period)),
                         minute_second = end_minute,
                         cum_xg = end_xg$cum_xg
  )
  # 
  # large_df <- bind_rows(large_df, end_rows)
  # if(game_id == 2204624){
  #   large_df$minute_second[large_df$minute_second == 20.00] <- 21.00
  # }else if(game_id == 2204654) {
  #   large_df$minute_second[large_df$minute_second == 58.00] <- 59.00
  # }else if(game_id == 2204659){
  #   large_df$player_name[large_df$minute_second == 33.32] <- "Julia Roddar (OG)"
  # }else if(game_id == 2204666){
  #   large_df$player_name[large_df$minute_second == 74.27] <- "Shea Groom (OG)"
  #   large_df$player_name[large_df$minute_second == 77.41] <- "Katie Naughton (OG)"
  # }else if(game_id == 2204671){
  #   large_df$player_name[large_df$minute_second == 50.00] <- "Celia Jiménez (OG)"
  #   large_df$player_name[large_df$minute_second == 57.41] <- "Alana Cook (OG)"
  # }else if(game_id == 2204679){
  #   large_df$minute_second[large_df$minute_second == 66.00] <- 66.17
  #   large_df$player_name[large_df$minute_second == 10.39] <- "Kristen Edmonds (OG)"
  # } else if(game_id == 2204680){
  #   large_df$minute_second[large_df$minute_second == 7.55] <- 6.55
  # }else if(game_id == 2204682){
  #   large_df$player_name[large_df$minute_second == 30.12] <- "Alana Cook (OG)"
  #   large_df$minute_second[large_df$minute_second == 26.00] <- 27.00
  # }else if(game_id == 2204687){
  #   large_df$minute_second[large_df$minute_second == 54.00] <- 55.00 
  # }else if(game_id == 2204689){
  #   large_df$minute_second[large_df$minute_second == 37.54] <- 36.54
  # }else if(game_id == 2204697){
  #   large_df$player_name[large_df$minute_second == 17.55] <- "Erin Simon (OG)"
  # }else if(game_id == 2204725){
  #   large_df$minute_second[large_df$minute_second == 89.00] <- 90.00
  # }else if(game_id == 2204734){
  #   large_df$player_name[large_df$minute_second == 34.45] <- "Kristen Edmonds (OG)"
  # }else if(game_id == 2204739){
  #   large_df$player_name[large_df$minute_second == 35.37] <- "Kiara Pickett (OG)"
  # }else if(game_id == 2257409){
  #   large_df$minute_second[large_df$minute_second == 48.00] <- 49.00
  #   large_df$minute_second[large_df$minute_second == 110.00] <- 111.00
  #   large_df$minute_second[large_df$minute_second == 54.00] <- 55.00
  #   
  # }
  View(large_df)
  
  home_xg <- large_df %>%
    filter(team_name == game_info$home_team_name) %>%
    mutate(cum_xg = round(cum_xg, 2)) %>%
    tail(1)
  
  away_xg <- large_df %>%
    filter(team_name == game_info$away_team_name) %>%
    mutate(cum_xg = round(cum_xg, 2)) %>%
    tail(1)
# 
#   sim_game <- win_percent(sims = 10000, 
#                           home_team_shots = na.omit(large_df$xg_team[large_df$team_id == game_info$home_team_id]), 
#                           away_team_shots = na.omit(large_df$xg_team[large_df$team_id == game_info$away_team_id])) %>%
#     mutate(annotate_text = if_else(team == "draw", 
#                                    paste0(round((num_sims/simulations) * 100, 0), "% Draw"), 
#                                    paste0(round((num_sims/simulations) * 100, 0), "% Win")),
#            annotate_text = if_else(num_sims/simulations < 0.05 , "", annotate_text),
#            team_color = case_when(team == "home"~ colors$dark_color[colors$team_name == game_info$home_team_name],
#                                   team == "draw" ~ "grey55",
#                                   team == "away" ~ colors$dark_color[colors$team_name == game_info$away_team_name])
#     )
#   result_pal <- sim_game$team_color
#   names(result_pal) <- sim_game$team
  
  # p1 <- ggplot(sim_game, aes(group = team)) + 
  #   geom_bar(aes(x = as.factor(simulations), y = num_sims, fill = team), position = "fill", stat = "identity", width = 0.3) +
  #   geom_text(aes(x = as.factor(simulations), y = num_sims/10000, label = annotate_text),
  #             position = position_stack(vjust = .5),
  #             size = 4,
  #             family = "Big Shoulders Display", fontface = "bold") + 
  #   scale_fill_manual(values = result_pal) + 
  #   coord_flip() + 
  #   theme_nothing() + 
  #   theme(legend.position = "none", 
  #         plot.margin=grid::unit(c(0,0,0,0), "mm"),
  #         text = element_text(family = "Big Shoulders Display", face = "bold"),
  #         plot.background = element_rect(fill = "#262525", colour = "#262525")) 
  
  home_team_label <- paste0(game_info$home_score, " G, ", home_xg$cum_xg, " xG")
  away_team_label <- paste0(game_info$away_score, " G, ", away_xg$cum_xg, " xG")
  pretty_date <- lubridate::stamp("January 1, 2021")
  time_zone <- colors$time_zone[game_info$home_team_id == colors$team_id]
  
  change_tz <- with_tz(ymd_hms(game_info$date_time_utc, tz = "UTC"), tz = time_zone)
  print(change_tz)
  date_label <- pretty_date(as.Date(change_tz, tz = time_zone))
  date_name <- stringr::str_replace_all(change_tz, "-", "")
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
  goals <- large_df %>%
    filter(goal == TRUE) %>%
    mutate(
      image = paste0("/Users/arielledror/team_color_balls/", team_id, ".png"), 
      image_path = paste0("<img src='", image, "' width = '12.5' height = '12.5'/>")
    )
  
  p2 <- ggplot(large_df) +
    geom_step(aes(x = minute_second, y = cum_xg, group = team_name, colour = team_name), size = 1) +
    scale_colour_manual(values = pal) +
    geom_text_repel(data = goals, aes(x = minute_second, y = cum_xg, label = player_name),
                    force = TRUE, box.padding = unit(1, "lines"),
                    family = "Big Shoulders Display", fontface = "bold",
                    nudge_y = 0.05, nudge_x = -6,
                    size = 5,
                    direction = "both") +
    geom_richtext(data = goals, aes(x = minute_second, y = cum_xg, label = image_path),
                  fill = NA, 
                  colour = NA,
                  label.padding = grid::unit(rep(0, 4), "pt")) +
    geom_vline(data = half_time_query, aes(xintercept = half_end), size = 0.75, colour = "grey", linetype = "dashed") + 
    annotate(
      "text",
      x = end_minute + 5,
      y = if_else(game_id == 2204631 | game_id == 2204731 | game_id == 2204733, home_xg$cum_xg + 0.05, home_xg$cum_xg),
      label = home_team_label,
      family = "Big Shoulders Display",
      fontface = "bold", 
      size = 5
    ) +
    annotate(
      "text",
      x = end_minute + 5,
      y = away_xg$cum_xg,
      label = away_team_label,
      family = "Big Shoulders Display",
      fontface = "bold",
      size = 5
    ) +
    scale_y_continuous(breaks = seq(0, max_xg, 0.2), labels = seq(0, max_xg, 0.2)) +
    scale_x_continuous(breaks = seq(0, end_minute, 15), labels = seq(0, end_minute, 15)) +
    labs(x = "TIME",
         y = "CUMULATIVE xG", 
         colour = "") +
    dark_theme_minimal() +
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
         caption = paste("Viz: @arielle_dror", "Data: American Soccer Analysis", sep = "\n")) + 
    theme(plot.title = element_text(hjust = 0.5, size = 24)) + 
    ggsave(paste0("race_charts/", file_name), height = 7.93, width = 6.22 * asp_ratio)
  
  
  final_p <- p2/p1 + 
    plot_layout(heights = unit(c(13, 1), c("cm", "null"))) + 
    plot_annotation(title = title_text,
                    caption = paste("Viz: @arielle_dror", "Data: American Soccer Analysis", sep = "\n")) &
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
 