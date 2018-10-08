library(dplyr)

rm(list=ls())

setwd('~/ddj18/output/')

# now that the data is store in RData-format, we can just load it, we don't need fread() anymore

### mono-element handling: single data sets
# have a look at all the files in the WD:
list.files()

# import 00-bevoelkerung.RData
load('00-bevoelkerung.RData')

# how is the data set called that just has been added to wour workspace?
ls()

# what dimensions does it have (rows    columns)?
dim(df)

# what variables does it contain?
colnames(df)

# combined approach:
str(df)
# str() gives you the dimensions of the object, the var names, the var classes, as well as printout of the first few elements of each var (== first few rows!)

# let's have a look at the first 10 observations with head (default is 6 rows, we specify n=10)
head(df)
head()
head(df, 10)

# now what class does the object df have?
class(df)

# let's convert it to a tibble (sparse dataframe, optimal to work with dplyr)
df <- as_tibble(df)
# now you can just evaluate df and it will return you only the first 10 rows, and show you the class of each variable
df

## I just hate var names containing upper case characters, so I coerce them to lower case
# original var names
colnames(df)
# coerced tomlower
tolower(colnames(df))
# now actually overwrite the names
df <- setNames(df, tolower(colnames(df)))

df

## now let's have a look at some variables
# let's find out the range of stichtagdatjahr
range(df$stichtagdatjahr) # so the data ranges from 1993-2007
# what age categories do we have?
unique(df$alterv05kurz)
# ah-ah, we see that is a parsing error: "100 u. \xe4lter" should be "100 u. älter", but it's still in utf-8 encoding.
# we see quarlang also has that problem:
unique(df$quarlang)

# to be sure that we have the correct encodings for our character vars, we will change the encodings for all char vars
# how do we do that?
# the easiest is to write a little function:
change_encoding <- function(x){
  if(is.character(x)) Encoding(x) <- 'latin1'
  return(x)
}

# now we apply the function to all char vars
# for that we use an apply-wrapper function and coerce the single vectors back to a tibble
df <- sapply(df[1:length(df)], change_encoding, simplify = F) %>% as_tibble
# whoops, what is that %>%-thingy?! ... you will hear more about it

# let's check if it worked:
sapply(df[1:length(df)], function(x) if(is.character(x)) unique(x), simplify = F)

# cool, let's move on.

## missing value analysis
# it is always good to know how many NAs a dataset has. So we will compute the shares of NAs for each variable.
sapply(df[1:length(df)], function(x) length(x[is.na(x)])/length(x), simplify = F)
# looks like only gebnum and ewid have NAs (57%, 79%)
# but sometimes NAs are not present as such but as empty char ("")
# so let's see if soem of the variables contain empty chars
# let's convert those to NAs if there happen to be any
system.time(
  df <- mutate_all(df, function(x) ifelse(is.character(x)&grepl('^$', x), NA, x))
)

######
# now we want to do this with all datasets we have
library(dplyr)

rm(list=ls())

setwd('~/ddj18/output/')

change_encoding <- function(x){
  if(is.character(x)) Encoding(x) <- 'latin1'
  return(x)
}

replNA <- function(x) ifelse(is.character(x)&grepl('^$', x), NA, x)

# now that the data is store in RData-format, we can just load it, we don't need fread() anymore

### multi-element handling: single data sets
for(i in list.files()){
  print(i)
  
  # import 00-bevoelkerung.RData
  load(i)
  
  # let's convert it to a tibble (sparse dataframe, optimal to work with dplyr)
  df <- as_tibble(df) %>% 
    setNames(., tolower(colnames(df)))
  
  # clean data
  df2 <- mutate_all(df, change_encoding) %>% 
    mutate_all(., replNA)
  
  save(df, file = paste0('01-', gsub('00-|\\.RData', '', i), '-clean.RData'))
  rm(df)
}
