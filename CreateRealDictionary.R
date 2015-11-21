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

if(exists('dictionaryLemma')){
  rm(dictionaryLemma)
}

if(exists('dictionaryLink')){
  rm(dictionaryLink)
}
# Read all the data files and create a dictionary out of the words
# contained in them.
for (name in files[1:15]) {
  print(name)
  new <- read.csv(paste0('althingi_tagged/', name), encoding = 'UTF-8', stringsAsFactors = FALSE)
  # Remove all non letter words.
  new <- subset(new, grepl('(^[[:alpha:]]+$)|^\\.$', new$Word), select = c('Word', 'Lemma', 'Tag'))
  # Edit all words to lower case.
  new <- data.frame(lapply(new, tolower), stringsAsFactors = FALSE)
  new <- data.table(new)
  #new$nextTag <- c(new$Tag[2:(length(new$Tag))], NA)
  new$nextLemma <- c(new$Lemma[2:(length(new$Lemma))], NA)
  #newTag <- data.table(count(new, c('Tag', 'nextTag')))
  newLemma <- data.table(count(new, c('Lemma', 'nextLemma')))
  #new$nextTag <- NULL
  new$nextLemma <- NULL
  newLink <- unique(new)
  if(exists('dictionaryLemma')) {
    #dictionaryTag <- merge(dictionaryTag, newTag, by=c('Tag', 'nextTag'), all=TRUE)
    dictionaryLemma <- merge(dictionaryLemma, newLemma, by=c('Lemma', 'nextLemma'), all=TRUE)
    dictionaryLink <- merge(dictionaryLink, newLink, by=c('Word', 'Lemma', 'Tag'), all=TRUE)
    #dictionaryTag <- data.table(dictionaryTag$Tag, dictionaryTag$nextTag, rowSums(dictionaryTag[, c(3,4), with=FALSE], na.rm = TRUE))
    dictionaryLemma <- data.table(dictionaryLemma$Lemma, dictionaryLemma$nextLemma, rowSums(dictionaryLemma[, c(3,4), with=FALSE], na.rm = TRUE))
    #colnames(dictionaryTag) <- c('Tag', 'nextTag', 'Count')
    colnames(dictionaryLemma) <- c('Lemma', 'nextLemma', 'Count')
  } else {
    #dictionaryTag <- newTag
    dictionaryLemma <- newLemma
    dictionaryLink <- newLink
  }
}

write.table(dictionaryLink, file='dictionarylink1.csv', sep=',', fileEncoding = 'UTF-8')
#write.table(dictionaryTag, file='dictionarytag1.csv', sep=',', fileEncoding = 'UTF-8')
write.table(dictionaryLemma, file='dictionarylemma1.csv', sep=',', fileEncoding = 'UTF-8')