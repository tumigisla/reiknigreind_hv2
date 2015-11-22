############################################################################
# data is the csv file to find non word errors in
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
findNonWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }
  
  data["OurGuess"] <- ""
  
  wordData <- unique(data$Word)
  
  for (i in 1:length(wordData)){
    print(i)
    guess <- correctWord(wordData[[i]])
    data[which(data$Word == wordData[[i]]),]$OurGuess <- guess
  }
  
  return(data)
}

correctWord <- function(word) {
  
  word <- tolower(word)
  
  if(!grepl('^[[:alpha:]]+$', word)){
    return(word)
  }
  size <- nchar(word)
  
  currentDictionary <- get(paste0("dictionary", size))
  
  index <- match(word, currentDictionary$Word)
  if(!is.na(index)) {
    if(currentDictionary[index,]$Count > 1000) {
      return(word)
    } else if(currentDictionary[index,]$Count < 100) {
      currentDictionary[index,]$Count = 100
    }
  } else {
    currentDictionary = rbind(currentDictionary, data.frame(Word = word, Count = 100, stringsAsFactors = FALSE))
  }
  
  editDistDf <- data.frame(currentDictionary$Word, matrix(adist(word, currentDictionary$Word), byrow=T), currentDictionary$Count, stringsAsFactors = FALSE)
  colnames(editDistDf) <- c('Word', 'Adist', 'Count')
  
  for (j in 0:2) {
    maxFreq <- editDistDf[which(editDistDf$Adist == j),]
    maxFreq <- maxFreq[which(maxFreq$Count == max(maxFreq$Count)),]
    maxFreq <- data.frame(maxFreq$Word, (maxFreq$Count/(50^j)), stringsAsFactors = FALSE)
    colnames(maxFreq) <- c('Word', 'Weight')
    if(exists('correct')) {
      correct <- rbind(correct, maxFreq)
    } else {
      correct <- maxFreq
    }
  }
  
  guess <- correct[which(correct$Weight == max(correct$Weight)),]$Word[1]

  return(guess)
}

words <- findNonWordErrors('althingi_errors/095.csv', TRUE)