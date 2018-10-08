library(dplyr)
library(pbapply)

rm(list=ls())

setwd('~/ddj18/output/')

# import data as listed dataframe and name each element of the list according to it's dataset
dfl <- pblapply(grep('01', list.files(), value=T), function(x){
  load(x)
  return(df)
}) %>% 
  setNames(., grep('01', list.files(), value=T) %>% 
             gsub('01-|-clean\\.RData', '', .))
str(dfl)

lengths(dfl)

# now want to know which variable can be used to bind everything together
nms <- sapply(dfl, names)
nms <- sapply(nms, function(x) x[x%in%nms[['bevoelkerung-clean']]])[2:7]
