# Blogbeitrag: Das Abstimmungsverhalten der Zürisee-Gemeinden über die Zeit
# von 1990 bis 2018

# Libraries

# library(foreign) ->> use data.table
library(data.table)
library(lubridate) # your date variable was fromatted wrongly; for easy date transformation use lubridate
library(dplyr)
library(tidyr)
library(ca)
library(ggplot2)
library(pbmcapply)
library(ggrepel)

rm(list=ls())
# Datensätze einlesen

source('~/r-helpers/ggplot/ggplot-helper.R')

# setwd("/Users/katrinhauser/Desktop/Daten_Datenjournalismus_HS18")
setwd('~/ddj18/res/kh/')
df <- fread("KANTON_ZUERICH_abstimmungsarchiv_gemeinden.csv") %>% 
  setNames(., tolower(names(.)))
str(df)
df <- mutate(df, 
             abstimmungstag=dmy(abstimmungstag),
             year=as.numeric(year(abstimmungstag)))

df <- select(df, abstimmungstag, year, stat_vorlage_id, vorlage_kurzbez,
             gemeinde, stimmbeteiligung, az_ja_stimmen, az_nein_stimmen, 
             az_gültige_stimmzettel) %>% 
  filter(year %in% 1990:2018) %>% 
  mutate(stimmenanteil_ja=az_ja_stimmen/az_gültige_stimmzettel) %>% 
  as_tibble

# Faktoranalyse 
### OVER ALL YEARS
items <- df %>% 
  # mutate(gemeinde=paste0(gemeinde,'_',year)) %>% 
  select(gemeinde, stimmenanteil_ja, stat_vorlage_id)
items <- items %>% spread(stat_vorlage_id, stimmenanteil_ja)
items <- items %>% replace(is.na(.), 0)
items <- as.matrix(items)
rownames(items) <- items[,1]
items <- items[,-1]
storage.mode(items) <- "numeric"
items <- items[!rowSums(items)==0,!colSums(items)==0]
fit <- ca(items)
fitex <- tibble(dim1 = c(fit$colcoord[,1],fit$rowcoord[,1]), 
                dim2 = c(fit$colcoord[,2],fit$rowcoord[,2]),
                type=c(rep(1,length(fit$colcoord[,1])),rep(2,length(fit$rowcoord[,1]))),
                name=c(rownames(fit$colcoord),rownames(fit$rowcoord)),
                district=gsub('_.*','', name),
                year=gsub('.*_','', name))
fitex %>%
  filter(type==2) %>% 
  ggplot(aes(x=dim1, y=dim2)) +
  geom_point(alpha=0.8) 


######## YEARLY
dfex <- pbmclapply(unique(df$year), function(x){
  items <- df %>% 
    filter(year==x) %>% 
    # mutate(gemeinde=paste0(gemeinde,'_',year)) %>% 
    select(gemeinde, stimmenanteil_ja, stat_vorlage_id)
  items <- items %>% spread(stat_vorlage_id, stimmenanteil_ja)
  items <- items %>% replace(is.na(.), 0)
  items <- as.matrix(items)
  rownames(items) <- items[,1]
  items <- items[,-1]
  storage.mode(items) <- "numeric"
  items <- items[!rowSums(items)==0,!colSums(items)==0]
  
  
  fit <- ca(items)
  fitex <- tibble(dim1 = c(fit$colcoord[,1],fit$rowcoord[,1]), 
                  dim2 = c(fit$colcoord[,2],fit$rowcoord[,2]),
                  type=c(rep(1,length(fit$colcoord[,1])),rep(2,length(fit$rowcoord[,1]))),
                  district=c(rownames(fit$colcoord),rownames(fit$rowcoord)),
                  year=x)
  return(fitex)
}, mc.cores=3) %>% 
  do.call(rbind, .)

q <-
  dfex %>%
  filter(type==2, district%in%c('Adlikon', 'Bassersdorf', 'Zürich', 'Wil', 'Ellikon a.d.Th.', 'Winterthur')) %>% 
  ggplot(aes(x=dim1, y=dim2, group=district, color=district, label=year, size=year/1000)) +
  geom_hline(aes(yintercept=0), color='white', lty='dotted') +
  geom_vline(aes(xintercept=0), color='white', lty='dotted') +
  geom_point() +
  geom_path(aes(alpha=year/1000)) +
  # geom_smooth() +
  # geom_text_repel(size=5) +
  scale_size(range=c(1,2)) +
  scale_alpha(range=c(0.1,0.5)) +
  facet_wrap(~district) +
  theme_void(grid = 'none') +
  theme(
    panel.background = element_rect(fill='black'),
    plot.background = element_rect(fill='black'),
    axis.line = element_line(color='white', linetype = 'dotted'),
    axis.text = element_text(color='white'),
    strip.background =element_rect(fill="black"),
    strip.text = element_text(colour = 'white', size=15),
    text = element_text(size=15)
  ) +
  guides(color=FALSE, size=FALSE, alpha=FALSE)
q


ggsave(q, filename = 'first-try.png', width=12, height=12)

