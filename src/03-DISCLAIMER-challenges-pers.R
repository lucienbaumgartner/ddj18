library(dplyr)
library(multidplyr)
library(pbmcapply)
library(ggplot2)
library(reshape2)
library(scales)
library(viridis)

rm(list=ls())

setwd('~/ddj18/output/')
source('~/r-helpers/ggplot/ggplot-helper.R') # some ggplot functions

# load persnum-level data
load('02-joined-df-persnum-lvl.RData')

# inspect structure
str(pers)

# create cluster
cl <- create_cluster(cores = 4)

# add data to the cluster, grouping it by persnum
dfcl <- pers %>% 
  partition(persnum, cluster = cl)

# add packages to the cluster
dfcl %>% 
  cluster_library('dplyr') %>% 
  cluster_library('reshape2')

## test
# count how many times each id is present per year
system.time(
  n_mentions <- dfcl %>% 
    group_by(stichtagdatjahr) %>% 
    summarise(n=n()) %>% 
    collect
)

system.time(
  n_mentions2 <- pers %>% 
    group_by(persnum, stichtagdatjahr) %>% 
    summarise(n=n())
)

# check if they are the same
n_mentions==n_mentions2
# ... no... because of the cluster partioning. So we just need to arrange everything according to persnum...
table(arrange(n_mentions, persnum, n)==arrange(n_mentions2, persnum, n))
# and voila, we have identical datasets
rm(n_mentions, n_mentions2)

## now let's move on
# compute the yearly excess
diffs <- dfcl %>% 
  group_by(stichtagdatjahr) %>% 
  summarise(dupl=n()-1) %>% 
  collect 

dfcl %>% 
  group_by(stichtagdatjahr) %>% 
  summarise(dupl=n()-1) %>% 
  collect 

# have a look at the observations that appear multiple times in at least one instance (==year)
diffs %>% filter(dupl > 0)

# what's the range of excess mentions?
range(diffs$dupl)

# now let's plot the distribution of excess mentions
q <- diffs %>% 
  filter(dupl > 0) %>% # filter out non-excess observations
  select(-stichtagdatjahr) %>% # deselect year
  summarise(n=sum(dupl)) %>% # sum up the duplicates per persnum
  # plot
  ggplot(., aes(x=n)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  ylab('Number of Persons') +
  xlab('Number of Mentions Not Related to Yearly Re-Occurence') +
  ggtitle('Number of Mentions Not Related to Yearly Re-Occurence') +
  theme_void
q
ggsave(q, filename='plots/n_duplicates.png', width=15, height=10)
ggsave(q, filename='~/ddj18/res/plots/n_duplicates.png', width=15, height=10)

# create a subset containing only duplicates, and compute how many times there is a unique value in each variable over all duplicated years for each persnum
persp <- dfcl %>% 
  filter(duplicated(stichtagdatjahr)|duplicated(stichtagdatjahr, fromLast =T)) %>% # isolate duplicates
  summarise_all(., function(x) length(unique(x))) %>% # compute length of unique values for each persnum over all years
  collect %>% 
  melt(id.vars='persnum') %>% # create longtable
  filter(value>1) # create subset with all variables with more than one unique value over all years for each persnum

# aggregate the unique value counts per variables accros all persnum and compute shares
pdata <- persp %>% 
  group_by(variable) %>% # drop persnum
  summarise(n=sum(value)) %>% # sum values over persnum
  mutate(perc=n/sum(n), # compute shares
         variable=factor(variable, levels = unique(variable)[order(perc)])) # change order of variables for plot

# plot
q <- ggplot(pdata, aes(x=variable, y=perc, fill=variable)) +
  geom_bar(stat='identity') +
  geom_text(data=pdata %>% 
              filter(variable%in%c(levels(variable)[1:5])), 
            aes(label=format(round(perc*100, 1), nsmall=1)), hjust=1.75, color='white') +
  geom_text(data=pdata%>% 
              filter(variable%in%c(levels(variable)[6:length(variable)])), 
            aes(label=format(round(perc*100, 1), nsmall=1)), hjust=1.75) +
  coord_flip() +
  ylab('Share of duplicates not due to yearly re-occurence') +
  xlab('Variable') +
  ggtitle('Share of duplicates not due to yearly re-occurence, broken down by variable') +
  scale_y_continuous(labels=scales::percent, expand = c(0, 0)) +
  scale_fill_viridis(discrete = T, name='Variable') +
  theme_void
ggsave(q, filename='plots/shares_duplicates.png', width=15, height=10)
ggsave(q, filename='~/ddj18/res/plots/shares_duplicates.png', width=15, height=10)
