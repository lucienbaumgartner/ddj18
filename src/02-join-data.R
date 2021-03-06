library(dplyr)
library(pbapply)
library(data.table)
rm(list=ls())

setwd('~/ddj18/output/')

# import data as listed dataframe and name each element of the list according to its dataset
dfl <- pblapply(grep('01', list.files(), value=T), function(x){
  load(x)
  return(df)
}) %>% 
  setNames(., grep('01', list.files(), value=T) %>% 
             gsub('01-|-clean\\.RData', '', .))
str(dfl)

# all datasets have a different amount of variables
lengths(dfl)

# now want to know which variable can be used to bind everything together
nms <- sapply(dfl, names)
nms <- sapply(nms, function(x) x[x%in%nms[['bevoelkerung']]])
# for each variable, we will use these respective variables to bind it together with bevoelkerung:
nms

# now lets combine them..
# .. but we have two kinds of information: building-level and person-level, so we need to create two datasets

## person-level data (without wohnungen)
for(i in names(nms)[!names(nms)%in%c('finanzen', 'wohnung')]){
  print(i)
  if(identical(i, 'bevoelkerung')){
    df <- dfl[['bevoelkerung']]
  }else{
    df <- left_join(df, dfl[[i]], by=nms[[i]])
  }
}
pers <- df

# check if dataset has inflated
identical(nrow(dfl[['bevoelkerung']]), nrow(pers))
# yes, it has we will see why in another script

save(pers, file='02-joined-df-persnum-lvl.RData')
rm('df', 'pers')

## building-level data (inflated by wohnungen!!!)
for(i in c('finanzen', 'gebaeude', 'wohnung')){
  print(i)
  if(identical(i, 'finanzen')){
    df <- dfl[['finanzen']]
  }else{
    df <- left_join(df, dfl[[i]], by=nms[[i]])
  }
}
geb <- df
save(df, file='02-joined-df-gebnum-lvl.RData')


