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
  dictionaryLemma <- data.table(dictionaryLemma)
  dictionaryLink <- data.table(dictionaryLink)
  
  for(i in 2:100) {
    data$Word[i + 1] <- tolower(data$Word[i + 1])
    data$Lemma[i + 1] <- tolower(data$Lemma[i + 1])
    data[i + 1,] <- findNonWordError(data[i + 1,])
    print(paste0(data$Word[i-1], " ", data$Word[i], " ", data$Word[i + 1]))
    before <- dictionaryLemma[which(dictionaryLemma$Lemma == data$Lemma[i - 1] & dictionaryLemma$nextLemma == data$Lemma[i]),]$Count
    after <- dictionaryLemma[which(dictionaryLemma$Lemma == data$Lemma[i] & dictionaryLemma$nextLemma == data$Lemma[i + 1]),]$Count
    
    if(length(before) & length(after)) {
      if(before > 10 & after > 10) {
        data$OurGuess[i] <- data$Word[i]
        next
      }
    }
    
    possibleTags <- findCandidates(dictionaryTag, data$Tag[i - 1], data$Tag[i + 1])
    possibleTags <- possibleTags[order(possibleTags$Prob, decreasing = TRUE),]
    possibleTags <- head(possibleTags, 100)
    possibleLemmas <- findCandidates(dictionaryLemma, data$Lemma[i - 1], data$Lemma[i + 1])

    linkedDict <- dictionaryLink[which(dictionaryLink$Tag %in% possibleTags$Candidates & dictionaryLink$Lemma %in% possibleLemmas$Candidates),]
    linkedDict['Prob'] <- ''
    linkedDictLength <- nrow(linkedDict)
    class <- substr(data$Tag[i], 1, 1)
    for (j in 1:linkedDictLength) {
      tagProb <- possibleTags$Prob[possibleTags$Candidates == linkedDict$Tag[j]]
      lemmaProb <- possibleLemmas$Prob[possibleLemmas$Candidates == linkedDict$Lemma[j]]
      prob <- tagProb + lemmaProb
      if (substr(linkedDict$Tag[j], 1, 1) != class) {
        prob <- prob + log10(0.1)
      }
      dist <- adist(linkedDict$Word[j], data$Word[i])[[1]]
      if (dist > 2) {
        prob <- prob - 15
      } else {
        prob <- prob + log10(1/(100^dist))
      }
      linkedDict$Prob[j] <- prob
    }
    data$Word[i] <- linkedDict[which(linkedDict$Prob == max(linkedDict$Prob)),]$Word
    data$OurGuess[i] <- data$Word[i]
  }
  return(data)
}

findCandidates <- function(dictionary, preciding, following) {

  forward <- dictionary[dictionary[[1]] == preciding,]
  forward[[3]] <- log10(forward[[3]]/sum(forward$Count, na.rm = TRUE))
  colnames(forward)[2] <- 'join'

  backward <- dictionary[dictionary[[2]] == following,]
  backward[[3]] <- log10(backward[[3]]/sum(backward$Count, na.rm = TRUE))
  colnames(backward)[1] <- 'join'

  candidates <- data.table(merge(forward, backward, by='join'))
  candidates <- data.table(candidates$join, candidates$Count.x + candidates$Count.y)
  colnames(candidates) <- c('Candidates', 'Prob')
  return(candidates)
}

findNonWordError <- function(word) {
  guess <- correctWord(word$Word)
  if (guess != word$Word) {
    word$Word <- guess
    possibilities <- dictionaryLink[which(dictionaryLink$Word == guess),]
    if (nrow(possibilities) > 1) {
      possibilitiesTags <- possibilities[which(possibilities$Tag == word$Tag),]
      if (nrow(possibilitiesTags) == 0) {
        word$Lemma <- possibilities$Lemma[1]
      } else {
        word$Lemma <- possibilitiesTags$Lemma[1]
      }
    } else if (nrow(possibilities) == 1) {
      word$Lemma <- possibilities$Lemma[1]
    }
  }
  return(word)
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)