

rm(list = ls())


setwd("~/ddj18/output/")


list.files()

load("01-gebaeude-clean.RData")
GEB<-df
gb_geb<- unique(GEB$gebnum) 

load("01-bevoelkerung-clean.RData")
BEV<-df
gb_bev <- unique(BEV$gebnum)
rm(df)

setwd("~/ddj18/res/os/")

BEV <- filter(BEV, !is.na(gebnum) & (gebnum %in% gb_geb))

GEB2 <- left_join(GEB, unique(select(BEV, quarlang, gebnum))) %>% 
  rename(quartier=quarlang) %>% 
  filter(!is.na(quartier)|is.na(baujahr))

save(GEB2, file='02-Gebäude-Quartier.RData')
################################################################################################
rm(list = ls())

# setwd("/Users/Roos/Desktop/Studium/Datenjournalismus/Output/")
load("02-Gebäude-Quartier.RData")

#Katgorien machen
library(dplyr)
GEB2 <- mutate(GEB2, 
               bauperiode=case_when(
                 baujahrgeb<1919 ~ "vor 1919",
                 baujahrgeb>=1919 & baujahrgeb<1950 ~ "1919-1949",
                 baujahrgeb>=1950 & baujahrgeb<1970 ~ "1950-1969",
                 baujahrgeb>=1970 & baujahrgeb<1990 ~ "1970-1989",
                 baujahrgeb>=1990 & baujahrgeb<2000 ~ "1990-2000",
                 baujahrgeb>=2000 ~ "2000 und später"
               ))

quartier <- c("Affoltern", "Albisrieden", "Alt-Wiedikon", "Altstetten",
         "City", "Enge", "Escher Wyss", "Fluntern", "Friesenberg",
         "Gewerbeschule", "Hard", "Hirslanden", "Hirzenbach", "Hochschulen",
         "Hottingen", "Höngg", "Langstrasse", "Leimbach", "Lindenhof", "Mühlebach",
         "Oberstrass", "Oerlikon", "Rathaus", "Saatlen", "Schwamendingen-Mitte", "Seebach",
         "Seefeld", "Sihlfeld", "Unterstrass", "Weinegg", "Werd", "Wipkingen", "Witikon",
         "Wollishofen")

#Medianwerte pro quartier rechnen in Gesamt df
GEB2$baujahrgeb_median <- NA

for(i in quartier){
  GEB2$baujahrgeb_median[GEB2$quartier == i] <-
    median(GEB2$baujahrgeb[GEB2$quartier ==i], na.rm = TRUE)
}


save(GEB2, file='03-Gebäude-Quartier.RData')
############################################################################
#Datensatz mit Finanzen verbindnen - Einkommen hinzufügen

load("03-Gebäude-Quartier.RData")
load("~/ddj18/output/01-finanzen-clean.RData")
FIN <- df
rm(df)

GEB2 <- left_join(GEB2, unique(select(FIN, aeq_einkommen, gebnum))) %>% 
  rename(einkommen=aeq_einkommen) %>% 
  filter(!is.na(quartier)|is.na(baujahrgeb))

save(GEB2, file='04-Gebäude-Quartier.RData')

load("04-Gebäude-Quartier.RData")

###################



############################################################################
#Datensatz mit Bevölkerungsdatensatz mergen
library(plyr)
library(dplyr)

# setwd("/Users/Roos/Desktop/Studium/Datenjournalismus/Output/")
setwd("~/ddj18/output/")
list.files()

load("01-gebaeude-clean.RData")
geb<-df
load("01-bevoelkerung-clean.RData")
bev<- df
rm(df)
df <- left_join(bev, geb)

modus <- function(v, na.rm=T) {
  if(isTRUE(na.rm)){
    uniqv <- unique(v)[!is.na(unique(v))]
    uniqv[which.max(tabulate(match(v[!is.na(v)], uniqv)))]
  }else{
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
}


df %>%
  group_by(gebnum, baujahrgeb) %>%
  summarise(modus_age = modus(alterv05kurz))
 

############################################################################
############################################################################
summary(GEB2$bauperiode)
summary(GEB2$baujahrgeb)


#dataset für plot mit anteilprozent
perc_data <- GEB2 %>% 
  group_by(quartier, bauperiode) %>% 
  summarise(n=n()) %>% 
  mutate(perc=n/sum(n))

##########################
#Plots

#barplot mit prozentsatz aller gebäudebauperioden pro quartier
library(ggplot2)
library(scales)

perc_data$quartier <- factor(perc_data$quartier, levels = sort(unique(perc_data$quartier), decreasing = TRUE))
perc_data$bauperiode <- factor(perc_data$bauperiode, levels = c("2000 und später", "1990-2000", "1970-1989", "1950-1969", "1919-1949", "vor 1919"))

r <- ggplot(perc_data)+ 
  geom_bar(aes(y= perc, x= quartier, fill= bauperiode), stat = "identity") + 
  coord_flip() +
  theme_light() + 
  labs(x="Zürcher Quartiere", y="Prozente") +
  scale_y_continuous(labels = percent) +
  ggtitle("Gebäude aus verschiedenen Bauperioden pro Quartier in (%)")+ scale_fill_brewer(palette = "Spectral")

r

ggsave(r, filename = paste0("plot_bar.png"), width=14, height=8)


########### BIS HIER kontrolliert


#######################
library(ggridges)

ggplot(data = subset(GEB2, baujahrgeb %in% seq(1800,2011))) + aes(x = baujahrgeb, y = quartier) + geom_density_ridges(aes(rel_min_height = 0.015))+
  labs(title = "Gebäude-Baujahre der Stadt Zürich", x= "Jahr", y ="")+
  theme_light()+ theme(legend.position = "right") +NULL

GEB2$quartier<- factor(GEB2$quartier, levels = unique(GEB2$quartier[order(GEB2$baujahrgeb_median, decreasing = FALSE)]))

library(viridis)
p<- ggplot(data = subset(GEB2, baujahrgeb %in% seq(1800,2011))) + aes(x = baujahrgeb, y = quartier, fill= baujahrgeb_median) + geom_density_ridges(aes(rel_min_height = 0.02))+
  labs(title = "Gebäude-Baujahre der Stadt Zürich", x= "Jahr", y ="")+ scale_fill_viridis(name="Gebäudejahr\n(Medianwert)", option = "C")+
  theme_light()+ theme(legend.position = "right") +NULL

p
ggsave(p, filename = paste0("ridge_plot.pdf"), width=14, height=9)
#############


#wichtige Quelle: https://www.baunetzwissen.de/altbau/fachwissen/baualtersstufen/baualtersstufe-der-60er-jahre-148200




#http://rgraphgallery.blogspot.com/2013/04/rg-stacked-bar-chart-number-and-percent.html
#http://t-redactyl.io/blog/2016/01/creating-plots-in-r-using-ggplot2-part-4-stacked-bar-plots.html

####################################################


#small multiples

ggplot(data = subset(GEB2, baujahrgeb %in% seq(1880,2011))) + aes(x= einkommen, y= baujahrgeb)+ geom_violin()+
  geom_smooth(se=FALSE) + facet_wrap(~quartier)+
  labs(title = "")+
  theme_light()+
  NULL










