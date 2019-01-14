library(dplyr)

rm(list=ls())

#### load data (this will look different for you!!!!!)

setwd('/Users/lucienbaumgartner/ddj18/output')
list.files()
load('01-bevoelkerung-clean.RData')
assign('bev', df)
load('01-gebaeude-clean.RData')
assign('geb', df)
load('01-finanzen-clean.RData')
assign('fin', df)
rm(df)
str(bev)
str(geb)
str(fin)

# fucntion to compute the modus
modus <- function(v, na.rm=T) {
  if(isTRUE(na.rm)){
    uniqv <- unique(v)[!is.na(unique(v))]
    uniqv[which.max(tabulate(match(v[!is.na(v)], uniqv)))]
  }else{
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
}

## create subset for which we have to compute the modus income per housing number
bev.noewid <- filter(bev, !is.na(gebnum)&is.na(ewid))   # we filter all obs with a gebnum but no ewid

fin.noewid <-
  # filter all obs in the fin data that matches the genum in the bev.noewid data set
  filter(fin, gebnum %in% unique(bev.noewid$gebnum)) %>% 
  # compute modus of the income variable (median fails because it's not numeric)
  # add a differentiating variable, just in case we might need it later
  group_by(gebnum, stichtagdatjahr) %>% 
  summarize(aeq_einkommen=modus(aeq_einkommen), 
            type='modus')

# join both subset to a conglomerate
bev.noewid <- left_join(bev.noewid, fin.noewid)

## create subset of non-probematic cases and perform more granular join
bev.ewid <- 
  # filter obs who have both gebnum and ewid
  filter(bev, !is.na(gebnum)&!is.na(ewid)) %>% 
  # create identifier variable by concenating genum and ewid
  mutate(identifier=paste0(gebnum, '_', ewid))

fin.ewid <- 
  # filter alls obs in the fin data with the same identifier
  mutate(fin, identifier=paste0(gebnum, '_', ewid)) %>% 
  filter(identifier%in%unique(bev.ewid$identifier)) %>% 
  select(-c(ewid, gebnum))

# now we join them and drop the identifier variable and add the type variable
bev.ewid <- 
  left_join(bev.ewid, fin.ewid, by=c('stichtagdatjahr', 'identifier')) %>% 
  select(-identifier) %>% 
  mutate(type='unaggregated')

## now we join the data together BY ROW and filter out all those who have NAs in the the aeq income thingy
df <- 
  rbind(bev.noewid, bev.ewid) %>% 
  filter(!is.na(aeq_einkommen))

# now we end up with..
dim(df)
### 296256 obs
####