library(plyr)
library(dplyr)
library(reshape2)
getwd()

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

# subset the observation that interests me (those who came and left)
moved <- filter(df, ((!is.na(weglandhistlang)|!is.na(zuzlandhistlang))&!is.na(aeq_einkommen))&!((duplicated(persnum)|duplicated(persnum, fromLast = T))&(duplicated(stichtagdatjahr)|duplicated(stichtagdatjahr, fromLast = T))))
dim(moved)

# now get those who stayed
stayed <- filter(df, ((is.na(weglandhistlang)|is.na(zuzlandhistlang))&!is.na(aeq_einkommen)))
stayed <- distinct(stayed, persnum, stichtagdatjahr, .keep_all = TRUE)
dim(stayed)

# process these
stayed_diff <- stayed %>%
  mutate(aeq_einkommen=factor(aeq_einkommen),
         aeq_einkommen=factor(aeq_einkommen, levels = levels(aeq_einkommen)[order(as.numeric(gsub("[A-z]+|-.*|\\'|\\s+",'', levels(aeq_einkommen))))])
  ) %>%
  select(persnum, quarlang, aeq_einkommen, stichtagdatjahr) %>%
  mutate(incomenum = as.numeric(aeq_einkommen)) %>%
  group_by(persnum, quarlang) %>%
  summarise(n = n(),
            incomechange = ifelse(n < 2, NA,
                                  diff(incomenum[order(stichtagdatjahr)]))) %>%
  ungroup()
stayed_diff

## compute share of income category leaving and moving in by location, for each district
subset_loc <- moved %>% 
  select(persnum, weglandhistlang, zuzlandhistlang, wegkthistlang, zuzkthistlang, aeq_einkommen, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'zuzug', 'wegzug')) %>% 
  group_by(quarlang, aeq_einkommen, direction, weglandhistlang, zuzlandhistlang, wegkthistlang, zuzkthistlang) %>% 
  summarise(n=n()) %>% 
  ungroup %>% 
  mutate(aeq_einkommen=factor(aeq_einkommen),
         aeq_einkommen=factor(aeq_einkommen, levels = levels(aeq_einkommen)[order(as.numeric(gsub("[A-z]+|-.*|\\'|\\s+",'', levels(aeq_einkommen))))])
         )
subset_loc

# compute chare leaving and entering by district (ignoring location)
subset_moved <- moved %>% 
  select(persnum, weglandhistlang, zuzlandhistlang, aeq_einkommen, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'zuzug', 'wegzug')) %>% 
  select(-c(weglandhistlang, zuzlandhistlang))  %>% 
  group_by(quarlang, direction) %>% 
  summarise(ntot=n()) %>% 
  ungroup
subset_moved

# calculate percentages
subset_total <- as_tibble(join_all(list(subset_loc, subset_moved), type = "full"))
subset_total <- subset_total %>% 
  mutate(per = n/ntot) %>% 
  mutate(per = ifelse(direction == "wegzug", -per, per))
subset_total

# for the sake of the past code, create the subset dataset
subset <- moved %>% 
  select(persnum, weglandhistlang, zuzlandhistlang, aeq_einkommen, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'zuzug', 'wegzug')) %>% 
  select(-c(weglandhistlang, zuzlandhistlang))  %>% 
  group_by(quarlang, direction, aeq_einkommen) %>% 
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



### load data

load("/Users/juliahasi/Desktop/ddj18/Daten Blogbeitrag/aeq-pyramide.RDS")


library(ggplot2)


# NOT my approach: example plot for seefeld
subset %>% 
  filter(quarlang=='Seefeld') %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-1,1), labels = scales::percent)

## f??r small multiples
  + facet_wrap(~quarlang)

## NOT my approach: um alle Quartiere auf einmal ausgeben zun lassen und zu vergleichen

for(i in unique(subset$quarlang)){
  q <- subset %>% 
    filter(quarlang==i) %>% 
    ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
    geom_bar(stat='identity') +
    coord_flip() +
    scale_y_continuous(limits = c(-1,1), labels = scales::percent)
  ggsave(q, filename=paste0('plot_', i, '.png'))
}


## MY approach:: compute share of peopleleaving and moving in, by income category, for each district
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

quartiers_interest <- c("Enge","Alt-Wiedikon","Werd","Oberstrass","Hottingen","Seefeld")


## PROBLEM: Plot for all Districts i'm interested in 

subset %>% 
  filter(quarlang %in% quartiers_interest) %>% 
  print(sum(perc)) %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  #scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent) +
  facet_wrap(~quarlang)


subset %>% 
  filter(quarlang == 'Seefeld') %>% 
  print(sum(perc)) %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent) 
+ #facet_wrap(~quarlang)


## um alle Quartiere auf einmal ausgeben zun lassen und zu vergleichen

for(i in unique(subset$quarlang)){
  q <- subset %>% 
    filter(quarlang==i) %>% 
    ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
    geom_bar(stat='identity') +
    coord_flip() +
    scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent)
  ggsave(q, filename=paste0('plot_', i, '.png'))
}

subset %>% 
  filter(quarlang=="Seefeld") %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent)

