library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(ggrepel)
getwd()

setwd('~/ddj18/output/')

rm(list=ls())

Sys.setlocale('LC_CTYPE', 'UTF-8')

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

# wd to save data (differs from the one above)
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

# subset the observation that interest you (those who came and left)
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

# compute share leaving and entering by district (ignoring location)
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
  select(persnum, weglandhistlang, zuzlandhistlang, wegkthistlang, zuzkthistlang, aeq_einkommen, quarlang) %>% 
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


# zuzug plot
quartiers_interest <- c("Enge","Alt-Wiedikon","Werd","Oberstrass","Hottingen","Seefeld")
land_int <- c("Schweiz", "Deutschland", "Europa", "USA")

#Grafik: Dot Plot
zuzplot_data <- 
  subset_total %>% 
  filter(!is.na(zuzlandhistlang)&quarlang%in%quartiers_interest) %>%
  mutate(zuzlandint = ifelse(zuzlandhistlang%in%land_int, zuzlandhistlang, "Andere")) %>%
  group_by(quarlang, zuzlandint) %>%
  summarise(totper=sum(per))

# check if the percentages add up to 1 per district
zuzplot_data %>% 
  summarise(check=sum(totper))
# yep!
zuzplot <- 
  ggplot(zuzplot_data, aes(x = quarlang, y = totper, color = zuzlandint)) +
  
  geom_point(size=1.5) +
  geom_text(aes(label = zuzlandint, vjust=-0.6)) +
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

zuzplot
ggsave(zuzplot, filename = 'zuzugsplot.png', height=10, width=8)
# wegzug plot

#Grafik: Dot Plot
wegplot_data <- 
  subset_total %>% 
  filter(!is.na(weglandhistlang)&quarlang%in%quartiers_interest) %>%
  mutate(weglandint = ifelse(weglandhistlang%in%land_int, weglandhistlang, "Andere")) %>%
  group_by(quarlang, weglandint) %>%
  summarise(totper=sum(per))

# check if the percentages add up to -1 per district
wegplot_data %>% 
  summarise(check=sum(totper))
# yep!

## plot
wegplot <- 
  ggplot(wegplot_data, aes(x = quarlang, y = abs(totper), color = weglandint)) +
  geom_point(size=1.5) +
  # geom_text(aes(label = weglandint, vjust=-0.6)) +
  # you can use geom_text_repel to avoid label overlap (in the package documentation you read on how to nudge the labels)
  geom_text_repel(aes(label = weglandint, vjust=-0.6)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle=60, hjust=1), 
    legend.position = "none", 
    legend.title = element_blank()
  ) +
  scale_y_continuous(labels = percent) +
  labs(title = "Wegzugsland pro Quartier, 2014", 
       subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Zuzügen)", 
       caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "")

wegplot




########## 
quartiers_interest <- c("Enge","Alt-Wiedikon","Werd","Oberstrass","Hottingen","Seefeld")


## PROBLEM: Plot for all Districts i'm interested in 

subset %>% 
  filter(quarlang %in% quartiers_interest) %>% 
  print(sum(perc)) %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~quarlang)


subset_total %>% 
  filter(quarlang == 'Seefeld') %>% 
  print(sum(perc)) %>% 
  ggplot(., aes(y=perc, x=aeq_einkommen, fill=direction)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_y_continuous(limits = c(-0.12, 0.12), labels = scales::percent) 
+ #facet_wrap(~quarlang)
  