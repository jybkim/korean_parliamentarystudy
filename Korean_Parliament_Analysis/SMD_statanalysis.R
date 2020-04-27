# loading necessary packages

library(tidyverse)
library(stringr)
library(gt)
library(broom)
library(ggplot2)
library(ggthemes)
library(plotly)

# load the SMD party share data

oldandvote <- get(load("data_files/oldandvoteshare.Rdata"))

# change the levels to 

levels(oldandvote$cons_or_not) <- c("Liberal/Progressives", "Conservatives", "Independent")

# make sure cons_or_not is a factor

oldandvote$cons_or_not <- as.factor(oldandvote$cons_or_not)

# export them back 

write_rds(oldandvote, path = "data_files/oldandvote.rds")


# create ggplot with regression

regressionplot <- oldandvote %>%
  filter(region == "Gyeonggi") %>%
  ggplot(aes(x = total_old, y = averagevoteshare, color = factor(cons_or_not))) + 
  geom_jitter() + 
  geom_smooth(method = "lm", 
              formula = y ~ x, se = TRUE) + 
  labs(title = "Exploring Vote Shares by Old Population",
       subtitle = "Regression Specified to Gyeonggi Province",
       x = "Population of 65 and Older", 
       y = "Percentage of Vote Share (%)", 
       color = "Party/Ideology Affiliation") + 
  theme_classic()


# create interaction table to account for cons_or_not factor

# but first, filter for Gyeonggi province

gyeonggi_oldandvote <- oldandvote %>%
  filter(region == "Gyeonggi")

interaction_model <- lm(data = gyeonggi_oldandvote, formula = 
                         averagevoteshare ~ total_old*cons_or_not)

# make gt table based off of this

interaction_gt <- interaction_model %>%
  tidy(conf.int = TRUE) %>%
  
  # round the digits
  
  mutate(estimate = round(estimate, 4)) %>%
  mutate(conf.low = round(conf.low, 4)) %>%
  mutate(conf.high = round(conf.high, 4)) %>%
  
  # select the variables
  
  select(term, estimate, conf.low, conf.high) %>%
  
  
  # make gt
  
  gt() %>% 
  tab_header(title = "Interaction of Ideology and Senior Population on Vote Shares") %>% 
  cols_label(term = "Variable", 
             estimate = "Estimate", 
             conf.low = "Lower bound", 
             conf.high = "Upper bound") %>% 
  tab_spanner(
    label = "Data from Kuniaki Nemoto",
    columns = vars(term, estimate, conf.low, conf.high))



