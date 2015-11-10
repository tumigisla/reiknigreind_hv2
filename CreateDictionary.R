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
  new <- read.csv(paste0('althingi_tagged/', name), encoding = 'UTF-8', colClasses = c('character', 'character', 'character'))
  # Remove all non letter words.
  new <- subset(new, grepl('^[[:alpha:]]+$', new$Word), select = c('Word'))
  # Edit all words to lower case.
  new <- data.frame(lapply(new, tolower), stringsAsFactors = FALSE)
  # Count the occurance of each word.
  new <- count(new, c('Word'))
               
  if(exists('dictionary')) {
    # Merge the existing dictionary and the new dictionary, then create a updated dictionary with the count sum.
    dictionary <- merge(dictionary, new, by = 'Word', all = TRUE)
    dictionary <- data.frame(dictionary$Word, rowSums(dictionary[, c(2,3)], na.rm = TRUE), stringsAsFactors = FALSE)
    colnames(dictionary) <- c('Word', 'Count')
  } else {
    dictionary <- new
  }

}

# Make a permenent copy of the dictionary on the hard disk.
write.table(dictionary, file='dictionary.csv', sep=',', fileEncoding = 'UTF-8')