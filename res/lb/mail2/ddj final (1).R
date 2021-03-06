#DDJ CODE

rm(list=ls())
setwd("C:\\Users\\Leo\\Documents\\R\\Herbstsemester 18\\ZURICH")


# Packages

library(ggplot2)
library(data.table)
library(shapeR)
library(ggridges)
library(texreg)
library(reshape2)
library(dplyr)


#laden der daten

fin <- fread("data_9383726.csv")

portrait <- fread("data_9089181.csv")

table(portrait$GEBIET_NAME)

table(fin$GEBIET_NAME)

#datensubset f�r 2017

fin2017 <- subset(fin, INDIKATOR_JAHR == 2017)

#datensatz aufsplitten

gemeinde <- split(fin, fin$GEBIET_NAME)

thema <- split(fin2017, fin2017$INDIKATOR_NAME)

#Beispiel: Pensionsleistungen im Datensatz lokalisieren?

head(thema$`03x Leistungen f�r Pensionierte [Fr.]`)


pensionsleistungen <- subset(thema$`03x Leistungen f�r Pensionierte [Fr.]`)

#plot f�r Eigenkapital


eigenkapital <- subset(fin, fin$SUBSUBSET_NAME == "Eigenkapital")

head(eigenkapital)

table(eigenkapital$GEBIET_NAME)
table(eigenkapital$INDIKATOR_JAHR)
table(eigenkapital$INDIKATOR_VALUE)



orte <- c("Z�rich", "Kloten", "Illnau-Effretikon", "Winterthur", "Andelfingen", "Thalwil", "Kilchberg", "R�schlikon", "Horgen", "Oberrieden")

q <- length(orte)

q <- ggplot(data = eigenkapital + aes(x = INDIKATOR_JAHR, y = INDIKATOR_VALUE) 
            + geom_point()
            + geom_smooth(se = FALSE)
            + facet_wrap(~ gemeinde)
            + labs(title = "�nderung des Eigenkapitals", subtitle = "2007-2017"))




