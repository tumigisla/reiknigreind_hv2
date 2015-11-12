############################################################################
# Script that reads through althingi speeches training data and
# creates a dictionary of the words used. The dictionary contains the
# words used and the frequency of usage for each.
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
library(plyr)
library(data.table)

# List the training files.
files <- list.files(path = 'althingi_tagged')

# Make sure the dictionary object is empty before populating it.
if(exists('real_dictionary')){
  rm(real_dictionary)
}

# Read all the data files and create a dictionary out of the words
# contained in them.
for (name in files[1]) {
  new <- read.csv(paste0('althingi_tagged/', name), encoding = 'UTF-8', colClasses = c('character', 'character', 'character'))
  # Remove all non letter words.
  new <- subset(new, grepl('(^[[:alpha:]]+$)|\\.', new$Word), select = c('Word', 'Lemma', 'Tag'))
  # Edit all words to lower case.
  new <- data.frame(lapply(new, tolower), stringsAsFactors = FALSE)
  new <- data.table(new)
  new$nextTag <- c(new$Tag[2:(length(new$Tag))], NA)
  new$nextWord <- c(new$Word[2:(length(new$Word))], NA)
}