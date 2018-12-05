rm(list = ls())

setwd('~/ddj18/res/bg/mail2/csv')
x=list.files()[6]
x= read.csv(x, sep=',')
dfl <- lapply(list.files(), function(x){
  if(grepl('Table\\s1', x)){
    tmp <- read.csv(x, stringsAsFactors = F, header = T, skip = 4) %>% 
      as_tibble %>% 
      .[,sapply(.[1:ncol(.)], function(x) any(grepl('[0-9]|[A-z]', x)))] %>% 
      setNames(., make.names(names=gsub('[^a-zA-Z0-9äÄöÖüÜ]|^c', '', (paste0(.[1:3,]))) %>% .[!.==""], unique=TRUE, allow_ = TRUE)) %>% 
      .[-c(1:4),] %>% 
      mutate(Jahr=as.integer(gsub('[^0-9]', '', x)) %/% 10)
    if(any(grepl('HauptgebieteBezirkeRegionen', colnames(tmp)))) tmp <- rename(tmp, Gemeinden=HauptgebieteBezirkeRegionen) 
    tmp <- mutate_at(tmp, vars(-Gemeinden), as.numeric)
  }else{
    if(ncol(read.csv(x))<3){
      tmp <- as_tibble(read.csv(x, sep=';', stringsAsFactors = F, header = T)) %>% select(-X)
    }else{
      tmp <- as_tibble(read.csv(x, stringsAsFactors = F, header = T)) %>% select(-X)
      }
  }
  
}) %>% 
  lapply(., function(x){setNames(x, tolower(colnames(x)))})

taxrates <- dfl[-c(1:2)] %>% 
  lapply(., function(x){setNames(x, colnames(dfl[[3]]))}) %>% 
  do.call(rbind, .) %>% 
  print(n=100)
save(taxrates, file='taxrates-yearly.RDS')

