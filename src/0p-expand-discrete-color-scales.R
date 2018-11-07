
library(gplots)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(extrafont) # different fonts

rm(list=ls())

source('~/r-helpers/ggplot/ggplot-helper.R')

# set WD
setwd('~/ddj18/output/')

# load pop data
load('01-bevoelkerung-clean.RData')

# aggregate data
kids <- df %>% mutate(has.kids=ifelse(anzahlkinder>0,1,0)) %>% 
  filter(stichtagdatjahr%in%2012:2017) %>% 
  group_by(quarlang, stichtagdatjahr, has.kids) %>% 
  summarise(n=n()) %>%
  mutate(p=n/sum(n)) %>% 
  ungroup %>% 
  group_by(quarlang, has.kids) %>% 
  summarise(median.p=median(p))

# simple bar plot
kids %>% 
  filter(has.kids==1) %>% 
  ggplot() + 
  geom_bar(aes(quarlang, median.p, fill=factor(quarlang)), stat='identity') +
  scale_fill_brewer(palette="Set2") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
```

# Brewer palettes
# brewer palettes
display.brewer.all()

# define the number of discrete colors you need (== factor levels of the fill or colour var)
c.count <- length(unique(kids$quarlang))

# feed the palette to the function to create a pre-parametrized function
get.palette <- colorRampPalette(brewer.pal(9, "Set1"))

# function
get.palette

# function fed with the umber of colours needed
get.palette(c.count)

# is everything of the same length?
length(get.palette(c.count))==c.count&c.count==length(unique(kids$quarlang))

kids %>% 
filter(has.kids==1) %>% 
ggplot() + 
geom_bar(aes(quarlang, median.p, fill=factor(quarlang)), stat='identity') + 
theme(legend.position="right") +
scale_fill_manual(values=get.palette(c.count))  +
theme(axis.text.x = element_text(angle=45, hjust=1))
```

# Custom colors: via expanding
# draw some random colors
set.seed(727678)
c <- sample(colors(), 7) %>% col2hex
c

get.palette <- colorRampPalette(c)

kids %>% 
filter(has.kids==1) %>% 
ggplot() + 
geom_bar(aes(quarlang, median.p, fill=factor(quarlang)), stat='identity') + 
# HERE you specify the colors:
scale_fill_manual(values=get.palette(c.count))  +
theme(axis.text.x = element_text(angle=45, hjust=1))


# Custom colors: direct approach
Alternative example with custom colors:
# draw some random colors
set.seed(727678)
c <- sample(colors(), c.count) %>% col2hex
c

kids %>% 
filter(has.kids==1) %>% 
ggplot() + 
geom_bar(aes(quarlang, median.p, fill=factor(quarlang)), stat='identity') + 
scale_fill_manual(values=c) +
theme(axis.text.x = element_text(angle=45, hjust=1))

# Viridis
kids %>% 
filter(has.kids==1) %>% 
ggplot() + 
geom_bar(aes(quarlang, median.p, fill=factor(quarlang)), stat='identity') + 
# HERE you specify discreteness
scale_fill_viridis(discrete = T) +
theme(axis.text.x = element_text(angle=45, hjust=1))

