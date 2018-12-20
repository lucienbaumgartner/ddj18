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
  select(persnum, weglandhistlang, zuzlandhistlang, wegkthistlang, zuzkthistlang, aeq_einkommen, quarlang) %>% 
  filter(!(is.na(weglandhistlang)&is.na(zuzlandhistlang))) %>% 
  mutate(direction=ifelse(is.na(weglandhistlang)&!is.na(zuzlandhistlang), 'zuzug', 'wegzug')) %>% 
  select(-c(weglandhistlang, zuzlandhistlang))  %>% 
  group_by(quarlang, aeq_einkommen, direction, weglandhistlang, zuzlandhistlang, wegkthistlang, zuzkthistlang) %>% 
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
library(ggplot2)
library(scales)
theme_set(theme_minimal())
zuzplot <- ggplot(data = subset(subset_total %>% filter(!is.na(zuzlandhistlang)) %>%
                       mutate(zuzlandind = zuzlandhistlang %in% land_int,
                              zuzlandandere = ifelse(zuzlandind, zuzlandhistlang, "Andere")) %>%
                       group_by(quarlang, zuzlandandere) %>%
                       summarise(totper = sum(per)), quarlang %in% quartiers_interest)) + 
  aes(x = quarlang, y = totper, color = zuzlandandere) +
  theme(axis.text.x = element_text(angle=60, hjust=1), legend.position = "none", legend.title = element_blank()) +
  geom_point(size=1.5) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = zuzlandandere, vjust=-0.6)) +
  labs(title = "Zuzugsland pro Quartier, 2014", subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Zuzügen)", caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "")

zuzplot

# wegzug plot
quartiers_interest <- c("Enge","Alt-Wiedikon","Werd","Oberstrass","Hottingen","Seefeld")
land_int <- c("Schweiz", "Deutschland", "Europa", "USA")

#Grafik: Dot Plot
library(ggplot2)
library(scales)
theme_set(theme_minimal())
wegplot <- ggplot(data = subset(subset_total %>% filter(!is.na(weglandhistlang)) %>%
                       mutate(weglandind = weglandhistlang %in% land_int,
                              weglandandere = ifelse(weglandind, weglandhistlang, "Andere")) %>%
                       group_by(quarlang, weglandandere) %>%
                       summarise(totper = sum(per)), quarlang %in% quartiers_interest)) + 
  aes(x = quarlang, y = abs(totper), color = weglandandere) +
  theme(axis.text.x = element_text(angle=60, hjust=1), legend.position = "none", legend.title = element_blank()) +
  geom_point(size=1.5) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = weglandandere, vjust=-0.6)) +
  labs(title = "Wegzugsland pro Quartier, 2014", subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Zuzügen)", caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "")








# sample image file save
png("/Users/juliahasi/Desktop/Beispiele Plots/test.png", height = 960, width = 960)
ggplot(data = subset(subset_total %>% filter(!is.na(weglandhistlang)) %>%
                       mutate(weglandind = weglandhistlang %in% land_int,
                              weglandandere = ifelse(weglandind, weglandhistlang, "Andere")) %>%
                       group_by(quarlang, weglandandere) %>%
                       summarise(totper = sum(per)), quarlang %in% quartiers_interest)) + 
  aes(x = quarlang, y = abs(totper), color = weglandandere) +
  theme(axis.text.x = element_text(angle=60, hjust=1), legend.position = "none", legend.title = element_blank()) +
  geom_point(size=1.5) +
  scale_y_continuous(labels = percent) +
  geom_text(aes(label = weglandandere, vjust=-0.6)) +
  labs(title = "Wegzugsland pro Quartier, 2014", subtitle = "(Der ausgegebene Kanton entspricht dem grössten kantonalen Anteil an den Zuzügen)", caption = "(Grafik 3: Julia Haslach; Daten: Statistik Stadt Zürich.)", x = "", y = "")
dev.off()

