

rm(list=ls(all=TRUE))

# setwd("C:\\Users\\Leo\\Documents\\R\\Herbstsemester 18\\ZURICH")
setwd('~/ddj18/res/lb/')

# Packages

library(ggplot2)

library(data.table)

### my csv
# import csv as data.table object
prt <- fread('fpzuerich_04-12-2018_23-50/DATA-Table 1.csv')
str(yprt)
# create list with single dataframes, split by GEBIET_NAME
prtl <- split(prt, prt$GEBIET_NAME)
str(prtl)


### your csv
yprt <- fread("data_2696314.csv")
str(yprt)