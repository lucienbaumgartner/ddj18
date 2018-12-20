###! make sure that you load plyr before dplyr
library(plyr)
library(dplyr)
library(data.table)

###! set your working directory
setwd('~/ddj18/input/st-zh/Daten/')

###! remove all objects before running your script
rm(list=ls())

#einlesen der relevanten datensaetze
### make this simpler, more compressed:
zuzug <- fread("zuzug.csv")
finanzen <- fread("finanzen.csv")
einkommen <- fread("einkommen.csv")
bevoelkerung <- fread("bevoelkerung.csv")

# DISCLAIMER: WE ONLY HAVE THE INCOME FOR THE YEAR 2014
sort(unique(einkommen$StichtagDatJahr))

#Datensatz erstellen mit 30 bis 64 Jaehrigen im Jahr 2014
###! good try, just chain the filter conditions
df <- filter(bevoelkerung, StichtagDatJahr%in%2013:2015 & AlterV05Kurz%in%c("30-34","35-39","40-44","45-49","50-54","55-59","60-64"))

####### DO NOT RUN
# I didn't get what year you are interested in in the end... if you want all years from 1993 ro 2014, just run this:
# SINCE WE ONLY HAVE THE INCOME FOR THE YEAR 2014 THIS DOESN'T MAKE MUCH SENSE THOUGH
df <- filter(bevoelkerung, StichtagDatJahr<=2014 & AlterV05Kurz%in%c("30-34","35-39","40-44","45-49","50-54","55-59","60-64"))
#######

#Da ich fuer meine Analyse aus dem Bevoelkerung Datensatz nur die PersNr und das Alter benoetige, kann ich den Rest weglassen
###! sure!
df <- select(df, PersNum, GebNum, AlterV05Kurz, EWID)

#Aus dem Zuzug Datensatz benoetige ich nur Zuzuege aus dem Ausland + PersNum + StichtagDatJahr
#Edit: das ist so nicht richtig, es ist auch das Land Schweiz drin??
###! just filter the data for 1993 to 2014 and non-swiss entries (!!!) first, since you join it with the bev data, then you subset the variables
zuz <- zuzug %>% 
  filter(StichtagDatJahr<=2014&!ZuzLandHistLang=='Schweiz') %>%
  select(PersNum, ZuzLandHistLang, StichtagDatJahr)
# check the years
sort(unique(zuz$StichtagDatJahr))
# now drop the year variable
zuz <- select(zuz, -StichtagDatJahr)
###! now we check for potential duplicates
filter(zuz, duplicated(zuz)|duplicated(zuz, fromLast = T)) ###! there are 225 entries...
filter(zuz, duplicated(zuz)) ###! generating 117 possible drop outs
###! now we filter them (you did it right, so I just take your code; you could also just simply use filter())
zuz <- zuz[!duplicated(zuz$PersNum),]

#Aus dem Einkommensdatensatz benoetige ich nur PersNum + aeq Einkommen + StichtagDatJahr
###! good call! But ther is no persnum in the einkommensdata, it's Haushalts??quivalenzeinkommen
###! thus you have to join over gebnum and ewid later, which is why I included the variable when subsetting df
eink <- select(einkommen, GebNum, aeq_Einkommen, EWID)


##### nun diese datensaetze mergen #####
# Zuerst Alter und Zuzug Datensaetze mergen
###! joins are much more controlled than a simple merge; use an recursively iterative left join for single line beauty
###! it is suuuuuper important that you use bev as first element, since it is the biggest data set and will inflate all other variables from the other datasets
df <- as_tibble(join_all(list(df, zuz, eink), type = 'left'))
###! and now you have to filter out all the people who didn't migrate to zurich that year (they have an NA in ZuzLandHistLang) OR have no information about their income
df <- filter(df, !(is.na(ZuzLandHistLang)|is.na(aeq_Einkommen)))
df
nrow(df)==length(duplicated(df))
df <- df[!duplicated(df),]
df
sort(unique(df$StichtagDatJahr))
#### 27516 unique cases
## I get 28,608
