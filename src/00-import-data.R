library(data.table)

rm(list=ls())

setwd('~/ddj18/input/st-zh/Daten/')

# list all files in the working directory
list.files()

# let's convert all the files to r-objects and save them
for(i in list.files()){
  df <- fread(i)
  save(df, 
       file = 
         paste0(
           '~/ddj18/output/00-',
           gsub(
             pattern = '\\.csv', 
             replacement = '', 
             x = i
             ),
           '.RData'
           )
       )
}

# this is what the evaluation of the file path will give for each iteration; here example for first iteration:
paste0(
  '~/ddj18/output/00-',
  gsub(
    pattern = '\\.csv', 
    replacement = '', 
    x = list.files()[1]
  ),
  '.RData'
)

# what does gsub do??
# it looks for the pattern and replaces it with a custom expression; in this case it just replaces '.csv' with nothing == it deletes the suffix.
# before gsub
list.files()[1]
# after gsub
gsub(
  pattern = '\\.csv', 
  replacement = '', 
  x = list.files()[1]
)

# condensed form:
for(i in list.files()) save(fread(i), file = paste0('~/ddj18/output/00-', gsub('\\.csv', '', i), '.RData'))

