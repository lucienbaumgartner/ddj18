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

setwd('~/ddj18/res/jh/mail2/')

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

#### plot
p <- subset %>% 
  filter(quarlang %in% quartiers_interest) %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent) +
  facet_wrap(~quarlang)
p
ggsave(p, filename='income-facet.png')


###### Dotplots
quartiers_interest <- c("Enge","Alt-Wiedikon","Werd","Oberstrass","Hottingen","Seefeld")
land_int <- c("Schweiz", "Deutschland", "Europa", "USA")

dp.df <- df %>% 
  select(persnum, weglandhistlang, zuzlandhistlang, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'Zuzug', 'Wegzug'),
         country_zuz=case_when(
           zuzlandhistlang%in%land_int ~ zuzlandhistlang, 
           !zuzlandhistlang%in%land_int&!is.na(zuzlandhistlang) ~ 'Andere',
           is.na(zuzlandhistlang) ~ NA_character_),
         country_weg=case_when(
           weglandhistlang%in%land_int ~ weglandhistlang, 
           !weglandhistlang%in%land_int&!is.na(weglandhistlang) ~ 'Andere',
           is.na(weglandhistlang) ~ NA_character_)) %>% 
  select(-c(persnum, weglandhistlang, zuzlandhistlang)) %>% 
  melt(id.vars=c('quarlang', 'direction')) %>% 
  na.omit %>% 
  group_by(quarlang, direction, value) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n))

# check percentages
dp.df %>% 
  summarise(check=sum(perc)) %>% 
  print(n=100)

# as small multiples
p <- dp.df %>% 
  filter(quarlang %in% quartiers_interest) %>% 
  ggplot(aes(x=quarlang, y=perc, colour=value)) +
  geom_point() +
  
  geom_point(size=1.5) +
  geom_text(aes(label = value, vjust=-0.6)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1), 
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  scale_y_continuous(labels = percent) +
  labs(title = "Zuzugs- und wegzugsland pro Quartier, 2014", 
       subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Zuzügen/Wegzügen)", 
       caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "") +
  facet_wrap(~direction)

p
ggsave(p, filename = 'zuzweg-total-combined.png')
  
# as single plots 
## wegzüge
p <- dp.df %>% 
  filter(quarlang %in% quartiers_interest & direction=='Wegzug') %>% 
  ggplot(aes(x=quarlang, y=perc, colour=value)) +
  geom_point() +
  
  geom_point(size=1.5) +
  geom_text(aes(label = value, vjust=-0.6)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1), 
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  scale_y_continuous(labels = percent) +
  labs(title = "Wegzugsland pro Quartier, 2014", 
       subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Wegzügen)", 
       caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "") 
p
ggsave(p, filename = 'weg-single.png')

## zuzüge
p <- dp.df %>% 
  filter(quarlang %in% quartiers_interest & direction=='Zuzug') %>% 
  ggplot(aes(x=quarlang, y=perc, colour=value)) +
  geom_point() +
  
  geom_point(size=1.5) +
  geom_text(aes(label = value, vjust=-0.6)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1), 
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  scale_y_continuous(labels = percent) +
  labs(title = "Zuzugsland pro Quartier, 2014", 
       subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Zuzügen)", 
       caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "") 
p
ggsave(p, filename = 'zu-single.png')


