# loading necessary packages

library(tidyverse)
library(stringr)
library(gt)
library(broom)
library(ggplot2)
library(ggthemes)
library(plotly)

# load the PR data

load("data_files/PR_twopartyvoteshare.Rdata")

# ggplot by ideology

graphideology <- PR_twopartyvoteshare %>%
  ggplot(aes(x = year, y = pct_total, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(pct_total, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
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
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 25, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  
  theme_classic() 

PRinteractive <- ggplotly(graphideology, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

