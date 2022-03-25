library(tidyverse)
library(stats)
library(ggsoccer)

data <- read.csv("~/asheville_city/Mock Data/Example_shooting.csv")

team <- "Asheville City"

all_shots <- data %>%
  # Add mock x, y coordinates
  mutate(
    x = runif(nrow(.), min = 65, max = 100),
    y = runif(nrow(.), min = 20, max = 60)
  ) %>%
  filter(Team == team)


ggplot(all_shots) +
  annotate_pitch(colour = "#c7c9c8", fill = "#F0F0F0", limits = FALSE) +
  geom_point(aes(x = x, y = 100 - y, color = as.factor(Outcome)), size = xG) +
  theme_pitch() + 
  coord_flip(xlim = c(49, 101), ylim = c(-12, 112)) +
  ggtitle("Asheville City vs. Greenville Liberty",
          subtitle = " Shot Attempts | 05/15/2022") +
  theme(plot.title=element_text(hjust = 0.5, face="plain", size=13),
        plot.subtitle=element_text(hjust = 0.5, face="plain", size=10),
        legend.title=element_blank()) +
  scale_size(range = c(0,1))

ggsave("shots_05_15_22.png")
