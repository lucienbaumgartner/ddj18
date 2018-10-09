library(dplyr)
library(pbmclapply)
library(ggplot2)

rm(list=ls())

setwd('~/ddj18/output/')

load('02-joined-df-persnum-lvl.RData')

str(pers)

pers %>% 
  group_by(persnum) %>% 
  summarise(n=n()) %>% 
  ungroup %>% 
  group_by(n) %>% 
  summarise(nsum=n()) %>% 
  ggplot(.) +
  geom_bar(aes(x=n, y=nsum), stat='identity') +
  ylab('Number of Persons') +
  xlab('Number of Mentions')

diffs <- pers %>% 
  select(persnum, stichtagdatjahr) %>% 
  unique %>% 
  group_by(persnum) %>% 
  summarise(npers_y=n()) %>% 
  left_join(., pers %>% 
              group_by(persnum) %>% 
              summarise(npers=n()), 
            by='persnum') %>%
  mutate(diff=npers-npers_y) 

diffs %>% filter(diff > 10)

range(diffs$diff)

diffs %>% 
  filter(diff > 0) %>% 
  group_by(diff) %>% 
  summarise(n=n()) %>% 
  ggplot(., aes(x=diff, y=n)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n), vjust=-1) +
  ylab('Number of Persons') +
  xlab('Number of Mentions Not Related to Yearly Re-Occurence')

persl <- diffs %>% 
  filter(diff > 0) %>% 
  split(., .$persnum)

persl_c <- pbmclapply(persl, function(x){
  mutate_all(x, function(y) length(unique(y)))
}, mc.cores=6)
  