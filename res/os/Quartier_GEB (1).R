

rm(list = ls())


setwd("/Users/Roos/Desktop/Studium/Datenjournalismus/Output/")


list.files()

load("01-gebaeude-clean.RData")
GEB<-df
gb_geb<- unique(GEB$gebnum) 

load("01-bevoelkerung-clean.RData")
BEV<-df
gb_bev <- unique(BEV$gebnum)
rm(df)

BEV <- subset(BEV, is.na(gebnum) == FALSE & (gebnum %in% gb_geb) == TRUE)

gb <- gb_geb[gb_geb %in% gb_bev]

GEB$quartier <- NA

for(i in 1:length(gb)){
  GEB$quartier[GEB$gebnum == gb[i]] <- as.character(unique(BEV$quarlang[BEV$gebnum == gb[i]]))
}


GEB2 <- subset(GEB, is.na(GEB$quartier) == FALSE)

save(GEB2, file='02-Gebäude-Quartier.RData')
################################################################################################
rm(list = ls())


setwd("/Users/Roos/Desktop/Studium/Datenjournalismus/Output/")
load("02-Gebäude-Quartier.RData")
Encoding(GEB2) <- 'latin1'

#Katgorien machen
library(dplyr)

attach(GEB2)


GEB2$bauperiode <- as.factor(ifelse(baujahrgeb <1919, "vor 1919",
                          ifelse(baujahrgeb>= 1919 & baujahrgeb <1950, "1919-1949",
                                 ifelse(baujahrgeb>= 1950 &baujahrgeb< 1970, "1950-1969",
                                        ifelse(baujahrgeb>=1970 & baujahrgeb<1990, "1970-1989",
                                               ifelse(baujahrgeb>=1990 &baujahrgeb<2000, "1990-2000",
                                                      ifelse(baujahrgeb>=2000, "2000 und später", 0)))))))


detach(GEB2)



qrt <- c("Affoltern", "Albisrieden", "Alt-Wiedikon", "Altstetten",
         "City", "Enge", "Escher Wyss", "Fluntern", "Friesenberg",
         "Gewerbeschule", "Hard", "Hirslanden", "Hirzenbach", "Hochschulen",
         "Hottingen", "Höngg", "Langstrasse", "Leimbach", "Lindenhof", "Mühlebach",
         "Oberstrass", "Oerlikon", "Rathaus", "Saatlen", "Schwamendingen-Mitte", "Seebach",
         "Seefeld", "Sihlfeld", "Unterstrass", "Weinegg", "Werd", "Wipkingen", "Witikon",
         "Wollishofen")

quartier <- factor(qrt)

q <- length(qrt)

#Medianwerte pro quartier rechnen in Gesamt df
GEB2$baujahrgeb_median <- NA

for(i in quartier){
  
  GEB2$baujahrgeb_median[GEB2$quartier == i]<-
    median(GEB2$baujahrgeb[GEB2$quartier ==i], na.rm = TRUE)
  
}


save(GEB2, file='03-Gebäude-Quartier.RData')
############################################################################
#Datensatz mit Finanzen verbindnen - Einkommen hinzufügen

load("03-Gebäude-Quartier.RData")
load("01-finanzen-clean.RData")
FIN <- df
rm(df)

gb_geb<- unique(GEB2$gebnum) 
gb_fin<-unique(FIN$gebnum)

gb <- gb_geb[gb_geb %in% gb_fin]

GEB2$einkommen <- NA

for(i in 1:length(gb)){
  GEB2$einkommen[GEB2$gebnum == gb[i]] <- as.character(unique(FIN$aeq_einkommen[FIN$gebnum == gb[i]]))
}

GEB2$einkommen<-as.factor(GEB2$einkommen)


summary(GEB2$einkommen)

GEB2 <- subset(GEB2, is.na(GEB2$einkommen) == FALSE)
save(GEB2, file='04-Gebäude-Quartier.RData')

load("04-Gebäude-Quartier.RData")

###################



############################################################################
#Datensatz mit Bevölkerungsdatensatz mergen
library(plyr)
library(dplyr)

setwd("/Users/Roos/Desktop/Studium/Datenjournalismus/Output/")
list.files()

load("01-gebaeude-clean.RData")
geb<-df
load("01-bevoelkerung-clean.RData")
bev<- df
dfl <- list(bev, geb)

df<- join_all(dfl, type='left')

summary(df)

 df %>%
  group_by(gebnum, baujahrgeb) %>%
             summarise(med_age = median(alterv05kurz))
 
 
############################################################################
############################################################################
summary(GEB2$bauperiode)
summary(GEB2$baujahrgeb)


#dataset für plot mit anteilprozent
mytable<- table(GEB2$bauperiode, GEB2$quartier)
mytable<-prop.table(mytable, 2)
mytable

mydata<- as.data.frame.matrix(mytable)
mydata$bauperiode <- rownames(mydata)

library(reshape2)
mydata_long <- melt(mydata, id.vars = c("bauperiode"), value.name = "percentage")

library(plyr)
data <- rename(mydata_long, c("bauperiode" = "bauperiode", "variable"="quartier", "percentage"="percentage")) 

#alternative: names(mydata_long)[2] <- paste("quartier")




##########################
#Plots

#barplot mit prozentsatz aller gebäudebauperioden pro quartier
library(ggplot2)
library(scales)

data$quartier <- factor(data$quartier, levels =
                                 sort(quartier, decreasing = TRUE))

data$bauperiode <- factor(data$bauperiode, levels = c("2000 und später", "1990-2000", "1970-1989", "1950-1969", "1919-1949", "vor 1919"))

r <- ggplot()+ geom_bar(aes(y= percentage, x= quartier, fill= bauperiode), data = data, stat = "identity",) + coord_flip() +
   theme_light() + labs(x="Zürcher Quartiere", y="Prozente") +
  scale_y_continuous(labels = percent) +
  ggtitle("Gebäude aus verschiedenen Bauperioden pro Quartier in (%)")+ scale_fill_brewer(palette = "Spectral")


r

ggsave(r, filename = paste0("plot_bar.png"), width=14, height=8)
c#######################
library(ggridges)

GEB2$quartier<- factor(GEB2$quartier, levels = sort(unique(GEB2$quartier), decreasing = TRUE))

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










