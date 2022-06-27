library(tidyverse)
library(stats)
library(ggsoccer)
library(showtext)
library(readxl)
font_add_google("Alatsi", "alatsi")
showtext_auto()
asp_ratio <- 1.618 
data<-read_excel("~/R/avlgit/asheville_city/Mock Data/ACSC2022.xlsx", sheet="Example_shooting")

team <- "Asheville City"
opponent <- "One Knoxville SC"
date_label="5/15/2022"
m_w="M"

data <- filter(data,Game_ID==2 & M_W=="M")

all_shots <- data %>%
  # Add mock x, y coordinates
  mutate(
    x = x_pos,#runif(nrow(.), min = 65, max = 100),
    y = y_pos#runif(nrow(.), min =20, max = 60)
  ) %>%
  filter(Team == team)



p2<-ggplot(all_shots) +
  annotate_pitch(colour = "#c7c9c8", fill = "#F0F0F0", limits = FALSE) +
  geom_point(aes(x = x, y = 100 - y, color = as.factor(Outcome), size = xG_calvaney)) +
  theme_pitch() + 
  # Flip coordinates because pitch is upright
  coord_flip(xlim = c(49, 101), ylim = c(-12, 112)) +
  ggtitle(paste0(team, " vs ", opponent),
          subtitle = paste0("Shot Attempts | ", date_label)) +
  theme(text = element_text(family = "alatsi"),plot.title = element_text(hjust = 0.5, face = "plain", size = 20),
        plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 15),
        legend.title = element_blank()) +
  # Scale size of shot
  scale_size(breaks = c(0:1), range = c(0.5, 4)) 


file_name <- paste0(
  str_replace_all(date_label, fixed("/"),""),
  str_replace_all(str_replace_all(team, " ", ""), "/", ""),
  ".png")

#showtext_opts(dpi = 300) 
ggsave(paste0("shot_map/",m_w,"/",file_name), plot = p2, height = 6, width = 7 * asp_ratio)

