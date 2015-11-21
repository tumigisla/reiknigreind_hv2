############################################################################
# data is the csv file to find real word errors in
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
findRealWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data["OurGuess"] <- ""
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }

  data <- subset(data, grepl('(^[[:alpha:]]+$)|^\\.$', data$Word), select = colnames(data))
  
  for(i in 2:500) {
    #start <- dictionaryTag[dictionaryTag$Tag == data$Tag[i - 1] & dictionaryTag$nextTag == data$Tag[i],]$Count
    #finish <- dictionaryTag[dictionaryTag$Tag == data$Tag[i] & dictionaryTag$nextTag == data$Tag[i + 1],]$Count
    #countA <- sum(dictionaryTag$Count[dictionaryTag$Tag == data$Tag[i - 1]], na.rm = TRUE)
    #countB <- sum(dictionaryTag$Count[dictionaryTag$nextTag == data$Tag[i + 1]], na.rm = TRUE)
    
    countAB <- dictionaryTag[which(dictionaryTag$nextTag == data$Tag[i]),]$Count
    countBC <- dictionaryTag[which(dictionaryTag$Tag == data$Tag[i-1]),]$Count
    
    countAx <- dictionaryWord[which(dictionaryWord$nextWord == data$Word[i]),]$Count
    countxC <- dictionaryWord[which(dictionaryWord$Word == data$Word[i]),]$Count
    
    prob <- (countAB / countAx) * (countBC / countxC)
    
    print(prob)
    
    #if (countA == 0 || countB == 0) {
    #  print(data$Tag[i - 1])
    #  print(countA)
    #  print(data$Tag[i + 1])
    #  print(countB)
    #}
    #propAB <- start/countA
    #propBC <- finish/countB
    #propABWord <- startWord/countAWord
    #propBCWord <- finishWord/countBWord
    
    #propAB <- propAB / propBC
    #propBC <- propABWord / propBCWord
    
    #prop <- propAB * propBC
    
    #print(prop)
  }
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)