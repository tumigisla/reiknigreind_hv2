############################################################################
# data is the csv file to find real word errors in
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
library(plyr)
library(data.table)

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
  
  dictionaryTag <- data.table(dictionaryTag)
  dictionaryWord <- data.table(dictionaryWord)
  
  for(i in 2:50) {
    print(data$Word[i-1])
    print(data$Word[i])
    print(data$Word[i+1])
    
    possibleTags <- findCandidates(dictionaryTag, data$Tag[i - 1], data$Tag[i + 1])
    possibleWords <- findCandidates(dictionaryWord, data$Word[i - 1], data$Word[i + 1])

    linkedDict <- dictionaryLink[which(dictionaryLink$Tag %in% possibleTags$Candidates & dictionaryLink$Word %in% possibleWords$Candidates),]
    linkedDict['Prob'] <- ''
    linkedDictLength <- nrow(linkedDict)

    for (i in 1:linkedDictLength) {
      tagProb <- possibleTags$Prob[possibleTags$Candidates == linkedDict$Tag[i]]
      wordProb <- possibleWords$Prob[possibleWords$Candidates == linkedDict$Word[i]]
      linkedDict$Prob[i] <- tagProb + wordProb
    }
    print(linkedDict[which(linkedDict$Prob == max(linkedDict$Prob)),])
  }
}

findCandidates <- function(dictionary, preciding, following) {

  forward <- dictionary[dictionary[[1]] == preciding,]
  forward[[3]] <- log10(forward[[3]]/sum(forward$Count, na.rm = TRUE))
  colnames(forward)[2] <- 'join'

  backward <- dictionary[dictionary[[2]] == following,]
  backward[[3]] <- log10(backward[[3]]/sum(backward$Count, na.rm = TRUE))
  colnames(backward)[1] <- 'join'

  candidates <- data.table(merge(forward, backward, by='join'))
  str(candidates)
  candidates <- data.table(candidates$join, candidates$Count.x + candidates$Count.y)
  colnames(candidates) <- c('Candidates', 'Prob')
  candidates <- candidates[order(candidates$Prob, decreasing=TRUE),]
  candidates <- head(candidates, 100)
  return(candidates)
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)