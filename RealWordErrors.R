############################################################################
# data is the csv file to find real word errors in
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
library(plyr)

findRealWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data["OurGuess"] <- ""
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }
  
  data <- subset(data, grepl('(^[[:alpha:]]+$)|^\\.$', data$Word), select = colnames(data))
  sum <- 0
  
  for(i in 2:50) {
    print(data$Word[i-1])
    print(data$Word[i])
    print(data$Word[i+1])
    
    possibleTags <- findCandidateTags(data$Tags[i - 1], data$Tags[i + 1])
    possibleWords <- findCandidateWords(data$Word[i - 1], data$Words[i + 1])
      
    linkedDict <- dictionaryLink[which(dictionaryLink$Tag %in% possibleTags$Tag & dictionaryLink$Word %in% possibleWords$Word),]
    linkedDict['Prob'] <- ''
    linkedDictLength <- nrow(linkedDict)
    for (i in 1:linkedDictLength) {
      tagProb <- possibleTags$Prob[possibleTags$Tag == linkedDict$Tag[i]]
      wordProb <- possibleWords$Prob[possibleWords$Word == linkedDict$Word[i]]
      linkedDict$Prob[i] <- tagProb + wordProb
    }
    print(linkedDict[which(linkedDict$Prob == max(linkedDict$Prob)),])
  }
}

findCandidateWords <- function(previousWord, nextWord) {
  
  countForward <- sum(dictionaryWord$Count[dictionaryWord$Word == previousWord], na.rm = TRUE)
  countBackward <- sum(dictionaryWord$Count[dictionaryWord$Word == nextWord], na.rm = TRUE)
  wordForward <- dictionaryWord[which(dictionaryWord$Word == previousWord),]
  wordForward$Count <- log10(wordForward$Count/countForward)
  wordBackward <- dictionaryWord[which(dictionaryWord$nextWord == nextWord),]
  wordBackward$Count <- log10(wordBackward$Count/countBackward)
  
  colnames(wordForward)[2] <- 'WordJoin'   # nextWord
  colnames(wordBackward)[1] <- 'WordJoin'  # Word
  possibleWords <- data.frame(merge(wordForward, wordBackward, by='WordJoin'), stringsAsFactors = FALSE)
  possibleWords$Count.x <- possibleWords$Count.x + possibleWords$Count.y
  possibleWords$Count.y <- NULL
  colnames(possibleWords) <- c('Word', 'Prob')
  possibleWords <- possibleWords[order(possibleWords$Prob, decreasing = TRUE),]
  possibleWords <- head(possibleWords, 100)
  
  return(possibleWords)
}

findCandidateTags <- function(previousTag, nextTag) {
  
  countForward <- sum(dictionaryTag$Count[dictionaryTag$Tag == previousTag], na.rm = TRUE)
  countBackward <- sum(dictionaryTag$Count[dictionaryTag$nextTag == nextTag], na.rm = TRUE)
  tagsForward <- dictionaryTag[which(dictionaryTag$Tag == previousTag),]
  tagsForward$Count <- log10(tagsForward$Count/countForward)
  tagsBackward <- dictionaryTag[which(dictionaryTag$nextTag == nextTag),]
  tagsBackward$Count <- log10(tagsBackward$Count/countBackward)
  
  colnames(tagsForward)[2] <- 'TagJoin'   # nextTag
  colnames(tagsBackward)[1] <- 'TagJoin'  # Tag
  possibleTags <- data.frame(merge(tagsForward, tagsBackward, by='TagJoin'), stringsAsFactors = FALSE)
  possibleTags$Count.x <- possibleTags$Count.x + possibleTags$Count.y
  possibleTags$Count.y <- NULL
  colnames(possibleTags) <- c('Tag', 'Prob')
  possibleTags <- possibleTags[order(possibleTags$Prob, decreasing = TRUE),]
  possibleTags <- head(possibleTags, 100)
  
  return(possibleTags)
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)