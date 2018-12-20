library(ggplot2)
library(rgdal)
library(viridis)
library(extrafont)

#Finanzdaten laden
load("df.Rda")#data frame mit Durchschnittwerten pro Geimeinden für 2010-2017

#Shapefile laden
shp.raw <- readOGR("Shape/", layer = "GEN_A4_GEMEINDEN_SEEN_2017_F")
str(shp.raw@data)#BFS check

table(unique(shp.raw$BFS)%in%unique(df$BFS_EINHEIT))#4 abweichende BFS (ich musste ein paar Gemeinden rauswerfen, da die Daten nicht korrekt waren)

class(shp.raw)

shp <- fortify(shp.raw, region = "BFS")
class(shp)

dim(shp)
shp <- left_join(shp, rename(df, id=BFS_EINHEIT))

dim(shp)

shp %>%
  ggplot(.) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=REL_AUS),
               color="white",
               size=0.2) + 
  coord_equal() +
  scale_fill_viridis(option = "C", 
                     name="Fr. pro Kind",
                     rescaler = function(x, to = c(0, 1), from = NULL) {
                       ifelse(x<750, 
                              scales::rescale(x,
                                              to = to,
                                              from = c(min(x, na.rm = TRUE), 750)
                                              ),
                              1)}) +
  labs(title= "Ausgaben pro Kind für Tagesstrukturen")