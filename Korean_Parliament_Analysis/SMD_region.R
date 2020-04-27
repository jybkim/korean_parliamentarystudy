# loading necessary packages

library(tidyverse)
library(stringr)
library(gt)
library(broom)
library(ggplot2)
library(ggthemes)
library(plotly)

# load the SMD party share data

load("data_files/SMD_partyshare.Rdata")

# change the levels to 

levels(SMD_partyshare$cons_or_not) <- c("Liberal/Progressives", "Conservatives", "Independent")

# export them back 

write_rds(SMD_partyshare, path = "data_files/SMD_partyshare.rds")


# ggplot by region

# first graph by busan

busanrough <- SMD_partyshare %>%
  filter(region == "Busan") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Busan Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Busan <- ggplotly(busanrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for chungbuk

chungbukrough <- SMD_partyshare %>%
  filter(region == "Chungbuk") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Chungbuk Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Chungbuk <- ggplotly(chungbukrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for chungnam

chungnamrough <- SMD_partyshare %>%
  filter(region == "Chungnam") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Chungnam Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Chungnam <- ggplotly(chungnamrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graphing for daegu

daegurough <- SMD_partyshare %>%
  filter(region == "Daegu") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Daegu Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Daegu <- ggplotly(daegurough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graphing for daejeon

daejeonrough <- SMD_partyshare %>%
  filter(region == "Daejeon") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Daejeon Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Daejeon <- ggplotly(daejeonrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for gangwon

gangwonrough <- SMD_partyshare %>%
  filter(region == "Gangwon") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Gangwon Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1994, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Gangwon <- ggplotly(gangwonrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for gwangju

gwangjurough <- SMD_partyshare %>%
  filter(region == "Gwangju") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Gwangju Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1994, y = 25, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 100, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1994, y = 3,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Gwangju <- ggplotly(gwangjurough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for gyeongbuk province

gyeongbukrough <- SMD_partyshare %>%
  filter(region == "Gyeongbuk") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Gyeongbuk Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 90, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 40, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Gyeongbuk <- ggplotly(gyeongbukrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graphing for gyeonggi

gyeonggirough <- SMD_partyshare %>%
  filter(region == "Gyeonggi") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Gyeonggi Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1995, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 30, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Gyeonggi <- ggplotly(gyeonggirough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for gyeongnam province districts

gyeongnamrough <- SMD_partyshare %>%
  filter(region == "Gyeongnam") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Gyeongnam Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 5, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 40,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Gyeongnam <- ggplotly(gyeongnamrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for incheon

incheonrough <- SMD_partyshare %>%
  filter(region == "Incheon") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Incheon Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 32, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 1,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Incheon <- ggplotly(incheonrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for Jeju districts 

jejurough <- SMD_partyshare %>%
  filter(region == "Jeju") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Jeju Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1996, y = 50, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 60, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1996, y = 5,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Jeju <- ggplotly(jejurough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for jeonbuk province

jeonbukrough <- SMD_partyshare %>%
  filter(region == "Jeonbuk") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Jeonbuk Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1994, y = 45, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1994, y = 3,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Jeonbuk <- ggplotly(jeonbukrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for jeonnam province

jeonnamrough <- SMD_partyshare %>%
  filter(region == "Jeonnam") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Jeonnam Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1994, y = 35, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 80, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1994, y = 3,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Jeonnam <- ggplotly(jeonnamrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for Seoul districts

seoulrough <- SMD_partyshare %>%
  filter(region == "Seoul") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Seoul Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 1994, y = 65, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 1996, y = 29, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 1994, y = 2,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Seoul <- ggplotly(seoulrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# graph for sejong

sejongrough <- SMD_partyshare %>%
  filter(region == "Sejong") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Sejong Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 2013, y = 70, 
           label = "Conservatives", color = "Red", size = 5) +
  annotate(geom = "text", x = 2013, y = 60, 
           label = "Liberal/Progressives", color = "Blue", size = 5) +
  annotate(geom = "text", x = 2013, y = 30,
           label = "Independents", color = "yellow3", size = 5) + 
  
  theme_classic() 

Sejong <- ggplotly(sejongrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

# final graph for ulsan

ulsanrough <- SMD_partyshare %>%
  filter(region == "Ulsan") %>%
  ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not, group = 1,
             text = paste("Year: ", year,
                          "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%", 
                          "<br>Party/Ideology: ", cons_or_not))) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = c("blue", "red", "yellow3")) + 
  
  # scale the years
  scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                     labels = c("1988", "'92", "'96", "2000",
                                "'04", "'08", "'12", "'16")) +
  
  # scale the percentage
  scale_y_continuous(breaks = seq(10, 100, by = 10),
                     limits = c(0, 100)) +
  
  # add title to graph
  labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Ulsan Districts",
       subtitle = "Data in Between Breaks are By-Elections",
       caption  = "Data from Kuniaki Nemoto, Musashi University", 
       y = "Percent", x = "Year") +
  
  # add text 
  annotate(geom = "text", x = 2002, y = 80, 
           label = "Conservatives", color = "Red", size = 4) +
  annotate(geom = "text", x = 2005, y = 30, 
           label = "Liberal/Progressives", color = "Blue", size = 4) +
  annotate(geom = "text", x = 2003.5, y = 15,
           label = "Independents", color = "yellow3", size = 4) + 
  
  theme_classic() 

Ulsan <- ggplotly(ulsanrough, tooltip = "text") %>%
  layout(dragmode = "pan") %>%
  config(displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))



# testin for app.R



