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
    print(paste0(data$Word[i-1], " ", data$Word[i], " ", data$Word[i+1]))
    
    countForward <- sum(dictionaryTag$Count[dictionaryTag$Tag == data$Tag[i - 1]], na.rm = TRUE)
    countBackward <- sum(dictionaryTag$Count[dictionaryTag$nextTag == data$Tag[i + 1]], na.rm = TRUE)
    tagsForward <- dictionaryTag[which(dictionaryTag$Tag == data$Tag[i - 1]),]
    tagsForward$Count <- log10(tagsForward$Count/countForward)
    tagsBackward <- dictionaryTag[which(dictionaryTag$nextTag == data$Tag[i + 1]),]
    tagsBackward$Count <- log10(tagsBackward$Count/countBackward)
    
    colnames(tagsForward)[2] <- 'TagJoin'   # nextTag
    colnames(tagsBackward)[1] <- 'TagJoin'  # Tag
    possibleTags <- data.frame(merge(tagsForward, tagsBackward, by='TagJoin'), stringsAsFactors = FALSE)
    possibleTags <- data.frame(possibleTags$TagJoin, possibleTags$Count.x + possibleTags$Count.y, stringsAsFactors = FALSE)
    colnames(possibleTags) <- c('Tag', 'Prob')
    possibleTags <- possibleTags[order(possibleTags$Prob, decreasing = TRUE),]
    possibleTags <- head(possibleTags, 100)
    
    countForward <- sum(dictionaryWord$Count[dictionaryWord$Word == data$Word[i - 1]], na.rm = TRUE)
    countBackward <- sum(dictionaryWord$Count[dictionaryWord$Word == data$Word[i + 1]], na.rm = TRUE)
    wordForward <- dictionaryWord[which(dictionaryWord$Word == data$Word[i - 1]),]
    wordForward$Count <- log10(wordForward$Count/countForward)
    wordBackward <- dictionaryWord[which(dictionaryWord$nextWord == data$Word[i + 1]),]
    wordBackward$Count <- log10(wordBackward$Count/countBackward)
    
    colnames(wordForward)[2] <- 'WordJoin'   # nextWord
    colnames(wordBackward)[1] <- 'WordJoin'  # Word
    possibleWords <- data.frame(merge(wordForward, wordBackward, by='WordJoin'), stringsAsFactors = FALSE)
    possibleWords <- data.frame(possibleWords$WordJoin, possibleWords$Count.x + possibleWords$Count.y, stringsAsFactors = FALSE)
    colnames(possibleWords) <- c('Word', 'Prob')
    possibleWords <- possibleWords[order(possibleWords$Prob, decreasing = TRUE),]
    possibleWords <- head(possibleWords, 100)
    
    linkedDict <- dictionaryLink[which(dictionaryLink$Tag %in% possibleTags$Tag & dictionaryLink$Word %in% possibleWords$Word),]
    linkedDict['Prob'] <- ''
    linkedDictLength <- nrow(linkedDict)
    for (i in 1:linkedDictLength) {
      tagProb <- possibleTags$Prob[possibleTags$Tag == linkedDict$Tag[i]]
      wordProb <- possibleWords$Prob[possibleWords$Word == linkedDict$Word[i]]
      prob <- tagProb + wordProb
      if (substr(linkedDict$Tag, 1, 1) != substr(data$Tag[i], 1, 1))
        prob <- prob + log10(0.1)
      linkedDict$Prob[i] <- tagProb + wordProb
    }
    print(linkedDict[which(linkedDict$Prob == max(linkedDict$Prob)),])
  }
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)