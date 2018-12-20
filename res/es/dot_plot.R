library(dplyr)
library(data.table)

Sys.setlocale("LC_CTYPE", "UTF-8")

###! set your working directory
setwd('~/ddj18/input/st-zh/Daten/')

###! remove all objects before running your script
rm(list=ls())
bev <- fread("bevoelkerung.csv")

setwd('~/ddj18/res/es/')

## generate data for 
bev.sub <- 
  bev %>%
  # generate subset with German and Swiss people in 2017
  filter(NationHistLang%in%c("Schweiz", 'Deutschland')&StichtagDatJahr==2017) %>%
  group_by(NationHistLang, HHtypLang) %>% 
  summarise(n=n()) %>%
  # as you can see, there is a HHTypLang == "" if your work with the uncleaned data.
  # let's filter that out
  print %>% 
  filter(!HHtypLang=="") %>% 
  # now we compute the in-group shares
  mutate(perc=n/sum(n)) %>% 
  print

# now let's check if the percentages add up to one for each group
# since NationHistLang is still a grouping variable, we don't need to group again but can instead directly summarize
bev.sub %>% 
  summarise(check=sum(perc))

# let's plot
## integer values
q <- 
  ggplot(bev.sub, aes(y=HHtypLang, x=n, colour=NationHistLang, shape=NationHistLang)) +
  geom_point(alpha=0.8, size=3) +
  scale_x_continuous(expand=c(1*10^-2, 1*10^-2)) +
  xlab('') +
  ylab('Haushaltstyp') +
  theme_light() +
  guides(color=guide_legend(title="Nationalität"), 
         shape=guide_legend(title="Nationalität")) +
  labs(title='Some title', subtitle='Some subtitle')
q
ggsave(q, filename = 'dot_plot_integer.png', height=8, width=8)

## percentages
q <- 
  ggplot(bev.sub, aes(y=HHtypLang, x=perc, colour=NationHistLang, shape=NationHistLang)) +
  geom_point(alpha=0.8, size=3) +
  scale_x_continuous(labels=scales::percent, expand=c(1*10^-2.5, 1*10^-2.5)) +
  xlab('') +
  ylab('Haushaltstyp') +
  theme_light() +
  guides(color=guide_legend(title="Nationalität"), 
         shape=guide_legend(title="Nationalität")) +
  labs(title='Some title', subtitle='Some subtitle')
q
ggsave(q, filename = 'dot_plot_perc.png', height=8, width=8)

#### your legacy version
HHtyplang_ger <- subset_deutschland %>%group_by(HHtypLang)%>%summarise(n=n())
HHtyplang_ch <- subset_schweiz %>%group_by(HHtypLang)%>%summarise(n=n())
HHtyplang_ger <- na.omit(HHtyplang_ger)
HHtyplang_ch <- na.omit(HHtyplang_ch)
subset_ch_2017 <- subset(subset_schweiz, subset_schweiz$StichtagDatJahr=="2017")
HHtyplang_ch_2017 <- subset_ch_2017 %>%group_by(HHtypLang)%>%summarise(n=n())
HHtyplang_ch_2017 <- na.omit(HHtyplang_ch_2017)
subset_ger_2017 <- subset(subset_deutschland, subset_deutschland$StichtagDatJahr=="2017")
HHtyplang_ger_2017 <- subset_ger_2017 %>%group_by(HHtypLang)%>%summarise(n=n())
HHtyplang_ger_2017 <- na.omit(HHtyplang_ger_2017)
bev %>% 
  filter(HHtypLang%in%HHtyplang_ger_2017 $HHtypLang, HHtypLang%in%HHtyplang_ch_2017$HHtypLang) %>% 
  group_by(StichtagDatJahr, HHtypLang) %>% 
  summarise(n=n()) %>%
  print(.) %>%
  ggplot(., aes(x=n, y=HHtypLang)) +
  geom_point() +
  labs(title = "Haushaltstypen", x = "Anzahl", y= "Haushaltstyp", caption = "(Daten: Statistik Stadt Z??rich)")+
  theme_light()