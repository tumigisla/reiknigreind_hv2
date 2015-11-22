############################################################################
# Script that reads through althingi speeches training data and
# creates a context sensative dictionaries. Creates three dictionaries
# for lemma, tag and links.
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
library(plyr)
library(data.table)

# List the training files.
files <- list.files(path = 'althingi_tagged')

# Make sure the dictionary objects is empty before populating it.
if(exists('dictionaryTag')){
  rm(dictionaryTag)
}

if(exists('dictionaryLemma')){
  rm(dictionaryLemma)
}

if(exists('dictionaryLink')){
  rm(dictionaryLink)
}
# Read all the data files and create a dictionaries.
for (name in files) {
  # Read in the current file.
  new <- read.csv(paste0('althingi_tagged/', name), encoding = 'UTF-8', stringsAsFactors = FALSE)
  # Remove all non letter words.
  new <- subset(new, grepl('(^[[:alpha:]]+$)|^\\.$', new$Word), select = c('Word', 'Lemma', 'Tag'))
  # Edit all words to lower case.
  new <- data.frame(lapply(new, tolower), stringsAsFactors = FALSE)
  new <- data.table(new)
  # Make new columns with the next tags and lemmas that appear.
  new$nextTag <- c(new$Tag[2:(length(new$Tag))], NA)
  new$nextLemma <- c(new$Lemma[2:(length(new$Lemma))], NA)
  # Count the apperance of each tag nextTag pair in the file.
  newTag <- data.table(count(new, c('Tag', 'nextTag')))
  # Count the apperance of each lemma nextLemma pair in the file.
  newLemma <- data.table(count(new, c('Lemma', 'nextLemma')))
  new$nextTag <- NULL
  new$nextLemma <- NULL
  # Select all unique word, lemma, tag pairs in the file.
  newLink <- unique(new)
  # Merge values for the current file with the global dictionaries.
  if(exists('dictionaryLemma')) {
    dictionaryTag <- merge(dictionaryTag, newTag, by=c('Tag', 'nextTag'), all=TRUE)
    dictionaryLemma <- merge(dictionaryLemma, newLemma, by=c('Lemma', 'nextLemma'), all=TRUE)
    dictionaryLink <- merge(dictionaryLink, newLink, by=c('Word', 'Lemma', 'Tag'), all=TRUE)
    # Count togather unique tag nextTag values.
    dictionaryTag <- data.table(dictionaryTag$Tag, dictionaryTag$nextTag, rowSums(dictionaryTag[, c(3,4), with=FALSE], na.rm = TRUE))
    # Count together unique lemma nextLemma values.
    dictionaryLemma <- data.table(dictionaryLemma$Lemma, dictionaryLemma$nextLemma, rowSums(dictionaryLemma[, c(3,4), with=FALSE], na.rm = TRUE))
    colnames(dictionaryTag) <- c('Tag', 'nextTag', 'Count')
    colnames(dictionaryLemma) <- c('Lemma', 'nextLemma', 'Count')
  } else {
    dictionaryTag <- newTag
    dictionaryLemma <- newLemma
    dictionaryLink <- newLink
  }
}
# Save the dictionaries in files
write.table(dictionaryLink, file='dictionarylink.csv', sep=',', fileEncoding = 'UTF-8')
write.table(dictionaryTag, file='dictionarytag.csv', sep=',', fileEncoding = 'UTF-8')
write.table(dictionaryLemma, file='dictionarylemma.csv', sep=',', fileEncoding = 'UTF-8')