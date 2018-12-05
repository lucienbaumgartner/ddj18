library(dplyr)
library(ggplot2)

setwd('~/ddj18/output/')

rm(list=ls())

Sys.setlocale('LC_ALL', 'UTF-8')

# load data
load(grep('01-(bev)', list.files(), value=T))

# create vector with top 5 immigrant nations
vec <- df %>% 
  filter(!nationhistlang%in%c('Schweiz', 'Europa', 'Asien')) %>% 
  group_by(nationhistlang) %>% 
  summarise(n=n()) %>% 
  top_n(5, wt=n) %>% 
  print(.) %>% 
  select(nationhistlang) %>% 
  unlist
vec
# create subset with top 5 immigrant nations aggregates per year for time series
df %>% 
  filter(nationhistlang%in%vec) %>% 
  group_by(stichtagdatjahr, nationhistlang) %>% 
  summarise(n=n()) %>% 
  print(.) %>% 
  # plot
  ggplot(., aes(x=stichtagdatjahr, y=n, colour=nationhistlang, group=nationhistlang)) +
  geom_path()
  
  