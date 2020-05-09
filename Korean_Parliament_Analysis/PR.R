# loading necessary packages

library(tidyverse)
library(stringr)
library(gt)
library(broom)
library(ggplot2)
library(ggthemes)
library(plotly)
library(gganimate)
library(gifski)

# load the PR data

load("data_files/PR_twopartyvoteshare.Rdata")

# ggplot by ideology

graphanimate <- PR_twopartyvoteshare %>%
  ggplot(aes(x = year, y = pct_total, color = cons_or_not)) + 
  geom_line(show.legend = FALSE) + 
  
  # add colors
  
  scale_colour_manual(values = c("blue", "red")) +
  
  # scale the years
  
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  
  labs(title = "Conservative vs. Liberal Vote Share for PR Seats Nationwide",
       subtitle = "Current system started in 1996",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  theme_classic()
  
# add the text at the end

graphanimate_actual <- graphanimate + 
  geom_segment(aes(xend = 2016, yend = pct_total, group = cons_or_not), linetype = 2, 
               colour = 'grey', show.legend = FALSE) + 
  geom_point(size = 2, show.legend = FALSE) + 
  geom_text(aes(x = 2016.5, label = cons_or_not), hjust = 1, 
            angle = 45, show.legend = FALSE) + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off')   

# save it as a gif to upload it onto shiny

animate(graphanimate_actual, fps = 10, height = 750, width = 750, renderer = gifski_renderer())

anim_save("graphanimate_actual.gif", animation = last_animation(), path = "data_files")




