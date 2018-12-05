library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
setwd('~/ddj18/output/')

rm(list=ls())

Sys.setlocale('LC_ALL', 'UTF-8')

# read in all the data as a list and subset for the years you are interested in
dfl <- lapply(grep('01-(bev|zuz|weg|fin)', list.files(), value=T)[c(1,3,4,2)], function(x){
  load(x)
  if(grepl('bev|weg|fin', x)){
    # since people who moved in 2014 are not in the 2014 bev data (I checked, can show you how),
    # we also need to include the 2013 data
    df <- filter(df, stichtagdatjahr%in%2013:2014)
  }else{
      # this is obviously not the case with those who move to zurich
      df <- filter(df, stichtagdatjahr==2014)
    }
  return(df)
  })

## change the wegzugs-data
# if the persnum in wegzug is not present in the bev data in the same year,
# we change the year in the wegzug data for that observation by -1
# reasoning: the person does not appear anymore in the bev data in the year he/she moved 
dfl[[2]] <- dfl[[2]] %>% 
  mutate(stichtagdatjahr=ifelse(!paste0(dfl[[2]]$stichtagdatjahr,'_',dfl[[2]]$persnum)%in%paste0(dfl[[1]]$stichtagdatjahr,'_',dfl[[1]]$persnum), 
                                stichtagdatjahr-1, 
                                stichtagdatjahr))
# join all the data together
df <- as_tibble(join_all(dfl, type='left'))
# check
str(df)
dim(df)
rm(dfl) # remove 

# subset the observation that interest you
df <- filter(df, ((!is.na(weglandhistlang)|!is.na(zuzlandhistlang))&!is.na(aeq_einkommen))&!((duplicated(persnum)|duplicated(persnum, fromLast = T))&(duplicated(stichtagdatjahr)|duplicated(stichtagdatjahr, fromLast = T))))
dim(df)

## compute share of income category leaving and moving in, for each district
subset <- df %>% 
  select(persnum, weglandhistlang, zuzlandhistlang, aeq_einkommen, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'zuzug', 'wegzug')) %>% 
  select(-c(weglandhistlang, zuzlandhistlang))  %>% 
  group_by(quarlang, aeq_einkommen, direction) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n),
         perc=ifelse(direction=='wegzug', -perc, perc)
         ) %>% 
  ungroup %>% 
  mutate(aeq_einkommen=factor(aeq_einkommen),
         aeq_einkommen=factor(aeq_einkommen, levels = levels(aeq_einkommen)[order(as.numeric(gsub("[A-z]+|-.*|\\'|\\s+",'', levels(aeq_einkommen))))])
         )
subset
# here is the isolated grep
levels(factor(df$aeq_einkommen))
gsub("[A-z]+|-.*|\\'|\\s+",'', levels(factor(df$aeq_einkommen)))      

# example plot for seefeld
subset %>% 
  filter(quarlang=='Seefeld') %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-1,1), labels = scales::percent)


## your approach:: compute share of peopleleaving and moving in, by income category, for each district
subset <- df %>% 
  select(persnum, weglandhistlang, zuzlandhistlang, aeq_einkommen, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'zuzug', 'wegzug')) %>% 
  select(-c(weglandhistlang, zuzlandhistlang))  %>% 
  group_by(quarlang, aeq_einkommen, direction) %>% 
  summarise(n=n()) 
subset <- left_join(subset, subset %>% 
                      ungroup %>% 
                      group_by(quarlang) %>% 
                      summarise(total=sum(n))) %>% 
  mutate(perc=n/total,
         perc=ifelse(direction=='wegzug', -perc, perc)
  ) %>% 
  ungroup %>% 
  mutate(aeq_einkommen=factor(aeq_einkommen),
         aeq_einkommen=factor(aeq_einkommen, levels = levels(aeq_einkommen)[order(as.numeric(gsub("[A-z]+|-.*|\\'|\\s+",'', levels(aeq_einkommen))))])
  )

# here is the isolated grep
levels(factor(df$aeq_einkommen))
gsub("[A-z]+|-.*|\\'|\\s+",'', levels(factor(df$aeq_einkommen)))      

subset %>% 
  filter(quarlang=='Seefeld') %>% 
  print(sum(perc)) %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent)


save(subset, file='aeq-pyramide.RDS')

