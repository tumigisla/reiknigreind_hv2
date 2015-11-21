
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
  
  wordData <- unique(data)
  wordData <- data.frame(wordData)
  colnames(wordData) <- c("Word")
  wordData$OurGuess <- ""
  
  # Add the correction of the word to the CorrectWord column
  lengthData <- length(wordData$Word)

  for (i in 1:lengthData) {
    
    print(i)
    
    if(exists('correct')) {
      rm(correct)
    }
    
    tmpWord <- tolower(wordData$Word[i])
    
    if(!grepl('^[[:alpha:]]+$', tmpWord)){
      wordData$OurGuess[i] <- tmpWord
      next
    }
    
    index <- match(tmpWord, dictionary$Word)
    
    if(!is.na(index)) {
      if(dictionary[index,]$Count > 500) {
        wordData$OurGuess[i] <- tmpWord
        next
      }
    }

    editDistDf <- data.frame(dictionary$Word, matrix(adist(tmpWord, dictionary$Word), byrow=T), dictionary$Count, stringsAsFactors = FALSE)
    colnames(editDistDf) <- c('Word', 'Adist', 'Count')
    
    for (j in 0:2) {
      maxFreq <- editDistDf[which(editDistDf$Adist == j),]
      maxFreq <- maxFreq[which(maxFreq$Count == max(maxFreq$Count)),]
      maxFreq <- data.frame(maxFreq$Word, (maxFreq$Count/(100^j)), stringsAsFactors = FALSE)
      colnames(maxFreq) <- c('Word', 'Weight')
      if(exists('correct')) {
        correct <- rbind(correct, maxFreq)
      } else {
        correct <- maxFreq
      }
      
    }
    
    wordData$OurGuess <- correct[which(correct$Weight == max(correct$Weight)),]$Word
    
  }
  return(wordData)
}

words <- findNonWordErrors('althingi_errors/079.csv', TRUE)
