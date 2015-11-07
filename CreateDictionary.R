############################################################################
# Script that reads through althingi speeches training data and
# creates a dictionary of the words used. The dictionary contains the
# words used and the frequency of usage for each.
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
library(plyr)

# List the training files.
files <- list.files(path = 'althingi_tagged')

# Make sure the dictionary object is empty before populating it.
if(exists('dictionary')){
  rm(dictionary)
}

# Read all the data files and create a dictionary out of the words
# contained in them.
for (name in files) {
  new <- read.csv(paste0('althingi_tagged/', name), encoding = 'UTF-8')
  
  if(exists('dictionary')) {
    dictionary <- rbind(dictionary, new)
  } else {
    dictionary <- new
  }

}

# Remove all non letter words and set the words to lowercase.
dictionary <- subset(dictionary, grepl('^[[:alpha:]]+$', dictionary$Word))
dictionary <- lapply(dictionary, tolower)

# Count the instance of each word.
dictionary <- count(dictionary, c('Word'))

# Make a permenent copy of the dictionary on the hard disk.
write.table(dictionary, file='dictionary.csv', sep=',', fileEncoding = 'UTF-8')