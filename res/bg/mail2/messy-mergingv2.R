library(dplyr)
library(reshape2)

rm(list = ls())

setwd('~/ddj18/res/bg/mail2/csv')

dfl <- lapply(grep('Table\\s1', list.files(), value = T), function(x){
    tmp <- read.csv(x, stringsAsFactors = F, header = T, skip = 4) %>% 
      as_tibble %>% 
      .[,sapply(.[1:ncol(.)], function(x) any(grepl('[0-9]|[A-z]', x)))] %>% 
      setNames(., make.names(names=gsub('[^a-zA-Z0-9äÄöÖüÜ]|^c', '', (paste0(.[1:3,]))) %>% .[!.==""], unique=TRUE, allow_ = TRUE)) %>% 
      .[-c(1:4),] %>% 
      mutate(Jahr=as.integer(gsub('[^0-9]', '', x)) %/% 10)
    if(any(grepl('HauptgebieteBezirkeRegionen', colnames(tmp)))) tmp <- rename(tmp, Gemeinden=HauptgebieteBezirkeRegionen) 
    tmp <- mutate_at(tmp, vars(-Gemeinden), as.numeric)
  })

taxrates <- dfl %>% 
  lapply(., function(x){setNames(x, tolower(colnames(dfl[[1]])))}) %>% 
  do.call(rbind, .) %>% 
  filter(!(is.na(gemeinden)|gemeinden=="")) %>% 
  print(n=100)
save(taxrates, file='taxrates-yearly.RDS')

tx.sub <- taxrates %>% 
  select(bfs, polent=gemeinden, taxrate=ohnekirchen, year=jahr)
save(tx.sub, file='taxrates-yearly-subset.RDS')
