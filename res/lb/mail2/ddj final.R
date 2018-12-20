#DDJ CODE

rm(list=ls())
setwd("C:\\Users\\Leo\\Documents\\R\\Herbstsemester 18\\ZURICH")

# my wd
setwd('~/ddj18/res/lb/mail2/')
Sys.setlocale('LC_ALL', 'C')


# Packages

library(ggplot2)
library(data.table)
library(shapeR)
# library(ggridges)
# library(texreg)
library(reshape2)
library(dplyr)
library(scales)


#laden der daten

fin <- fread("data_9383726.csv")

portrait <- fread("data_9089181.csv")

table(portrait$GEBIET_NAME)

table(fin$GEBIET_NAME)

#datensubset f?r 2017

fin2017 <- subset(fin, INDIKATOR_JAHR == 2017)

# subset and plot in one step
fin %>% 
  # isolate the variable you are interested in
  # either '239 Eigenkapital [Fr.]' OR '239 Eigenkapital [Fr./Einw.]'
  # '239 Eigenkapital [Fr./Einw.]' makes more sense I guess
  filter(INDIKATOR_NAME=='239 Eigenkapital [Fr.]') %>% 
  # make INDIKATOR_VALUE numeric
  mutate(INDIKATOR_VALUE=as.numeric(INDIKATOR_VALUE)) %>% 
  # plot
  ggplot(aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE))+
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_wrap(~ GEBIET_NAME) +
  labs(title = "Änderung des Eigenkapitals", subtitle = "2007-2017")
# as you can see, we have to clean the district names first to assure proper results


finsub <- 
  fin %>% 
  # isolate the variable you are interested in
  filter(INDIKATOR_NAME=='239 Eigenkapital [Fr./Einw.]') %>% 
  # clean GEBIET_NAME and make INDIKATOR_VALUE numeric
  mutate(INDIKATOR_VALUE=as.numeric(INDIKATOR_VALUE),
         # get rid of all the information after the comma
         GEBIET_NAME=gsub('\\,.*', '', GEBIET_NAME) %>% 
           # change encoding if needed; otherwise delete line and pipe
           `Encoding<-`('latin1'))
# compute median Einkapital / Einwohner for each GEBIET, to change the order of the GEBIETE in the next step
lvls <- 
  finsub %>%
  group_by(GEBIET_NAME) %>% 
  summarise(median_val=median(INDIKATOR_VALUE)) %>% 
  arrange(desc(median_val)) %>% 
  .$GEBIET_NAME

p <- 
  finsub %>% 
  # change the factor levels
  mutate(GEBIET_NAME=factor(GEBIET_NAME, levels=lvls)) %>% 
  # plot
  ggplot(aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE))+
  geom_point() + 
  geom_smooth(se = FALSE) +
  facet_wrap(~ GEBIET_NAME, nrow = 2) +
  scale_x_continuous(breaks = pretty_breaks()) +
  labs(title = "Aenderung des Eigenkapitals", subtitle = "2007-2017") +
  xlab('Jahr')+
  ylab('Eigenkapital [Fr./Einw.]')
p

ggsave(p, filename='eigenkapital.png', height=10, width=20)
