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
if(exists('dictionaryTag')){
  rm(dictionaryTag)
}

if(exists('dictionaryWord')){
  rm(dictionaryWord)
}

# Read all the data files and create a dictionary out of the words
# contained in them.
for (name in files) {
  new <- read.csv(paste0('althingi_tagged/', name), encoding = 'UTF-8', colClasses = c('character', 'character', 'character'))
  # Remove all non letter words.
  new <- subset(new, grepl('(^[[:alpha:]]+$)|^\\.$', new$Word), select = c('Word', 'Lemma', 'Tag'))
  # Edit all words to lower case.
  new <- data.frame(lapply(new, tolower), stringsAsFactors = FALSE)
  new <- data.table(new)
  new$nextTag <- c(new$Tag[2:(length(new$Tag))], NA)
  new$nextWord <- c(new$Word[2:(length(new$Word))], NA)
  newTag <- data.table(count(new, c('Tag', 'nextTag')))
  newWord <- data.table(count(new, c('Word', 'nextWord')))
  newLink <- data.table(new$Word, new$Tag)
  colnames(newLink) <- c('Word', 'Tag')
  setkey(newLink)
  newLink <- unique(newLink)
  if(exists('dictionaryWord')) {
    dictionaryTag <- merge(dictionaryTag, newTag, by=c('Tag', 'nextTag'), all=TRUE)
    dictionaryWord <- merge(dictionaryWord, newWord, by=c('Word', 'nextWord'), all=TRUE)
    dictionaryLink <- merge(dictionaryLink, newLink, by=c('Word', 'Tag'), all=TRUE)
    dictionaryTag <- data.table(dictionaryTag$Tag, dictionaryTag$nextTag, rowSums(dictionaryTag[, c(3,4), with=FALSE], na.rm = TRUE))
    dictionaryWord <- data.table(dictionaryWord$Word, dictionaryWord$nextWord, rowSums(dictionaryWord[, c(3,4), with=FALSE], na.rm = TRUE))
    colnames(dictionaryTag) <- c('Tag', 'nextTag', 'Count')
    colnames(dictionaryWord) <- c('Word', 'nextWord', 'Count')
  } else {
    dictionaryTag <- newTag
    dictionaryWord <- newWord
    dictionaryLink <- newLink
  }
}

write.table(dictionaryLink, file='dictionarylink.csv', sep=',', fileEncoding = 'UTF-8')
write.table(dictionaryTag, file='dictionarytag.csv', sep=',', fileEncoding = 'UTF-8')
write.table(dictionaryWord, file='dictionaryword.csv', sep=',', fileEncoding = 'UTF-8')