library(data.table)
library(dplyr) 
library(reshape2)
setwd('~/ddj18/res/jg/')

rm(list=ls())

# import data from csv files stored in several subfolders
# alternatively: read in the files separately and put them into a list and set the names of the list to setNames(., c('pol', 'primar', 'sekundar'))
dfl <- lapply(paste0(list.files()[-length(list.files())], '/DATA-Table 1.csv'), function(x){
  df <- fread(x)
  return(df)
}) %>% 
  setNames(., c('pol', 'primar', 'sekundar'))

# let's have a look at the different names of the districts
lapply(c('primar', 'sekundar'), function(x) sort(gsub('\\,.*', '', dfl[[x]]$GEBIET_NAME)))

# let's see how many of the names in primar match the ones in sekundar
table(gsub('[A-Z]', '', dfl[['primar']]$GEFIS_EINHEIT)%in%gsub('[A-Z]', '', dfl[['sekundar']]$GEFIS_EINHEIT))
# which ones do not match?
dfl[['primar']][!gsub('[A-Z]+', '', dfl[['primar']]$GEFIS_EINHEIT)%in%gsub('[A-Z]+', '', dfl[['sekundar']]$GEFIS_EINHEIT),]

# check dimensions
sapply(dfl, dim)
## collapse list to dataframe
df <-
  # for each dataframe we do:
  lapply(c('pol','primar', 'sekundar'), function(x){
    temp <- 
      # get rid of the suffix in the gefis code, so that we end up with the actual zip code
      mutate(dfl[[x]], GEFIS_EINHEIT=gsub('0[0-9][A-Z]+$', '', GEFIS_EINHEIT)) %>% 
      # select variables of interest
      select(GEFIS_EINHEIT, GEBIET_NAME, INDIKATOR_VALUE) %>% 
      # rename the variables
      setNames(., c('gefis', paste0('gebiet.name_', x),  x))
    return(temp)
  }) %>% 
  # join the listed dataframes to a singel dataframe using a recursive iterative left join
  join_all(., by='gefis', type='left')

## now e want to create a dataframe where we sum up all the expenses per zip code and code the level of the sum (==whether the sum is a sum of primar and sekundar, pol and primar, only pol (so no real 'sum'), etc.)
df <- 
  df %>% 
  # create a longtable
  melt(id.var=c('gefis', 'gebiet.name_pol', 'gebiet.name_primar', 'gebiet.name_sekundar'),
       value.name='cash', 
       variable.name='lvl') %>% 
  # group by zip code and expense level
  group_by(gefis, lvl) %>%
  # sum up cash at this level
  summarise(cash_by=sum(cash, na.rm = T)) %>% 
  # all levels where we do not have any expense data have a value of 0, therefore we sort them out
  filter(!cash_by==0) %>% 
  # now sum up the expenses across all levels per zip code and create a factor denoting the overall level of expense sums
  summarise(cash_tot=sum(cash_by, na.rm = T),
            lvl=paste0(lvl, collapse='_')) %>% 
  # show results
  print(n=200)

## plot
ex <- 
  df %>% 
  mutate(gefis=factor(gefis),
         gefis=factor(gefis, levels=levels(gefis)[order(unique(cash_tot))])) %>% 
  filter(!gefis%in%c(8000, 8400)) %>% 
  ggplot() +
  geom_point(aes(cash_tot, gefis, colour=lvl, shape=lvl))+
  facet_wrap(~lvl, scales = 'free_y', ncol=1) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(colour='lightgrey'),
        panel.grid.major.x = element_line(colour='lightgrey')) 
ggsave(ex, filename = 'exploratory.png', height=10, width=8)

save(df, file='exploratory.RDS')
  
