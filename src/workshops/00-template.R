library(dyplr)
library(pbapply)

Sys.getlocale()

rm(list = ls())

setwd('~/ddj18/output/')

Sys.setlocale('LC_ALL', 'en_US')
grep('^01', list.files(), value=T)

grep('^01-(bev|.*zug)', list.files(), value=T)


load(grep('^01-(bev|.*zug)', list.files(), value=T)[1])
str(df)
unique(df$hhtyplang)



rm(list=ls())
load('~/ddj18/res/merged-data-benjamin-lax.RDS')
df
Sys.getlocale()
Encoding(df$gebiet_name) <- 'UTF-8'
