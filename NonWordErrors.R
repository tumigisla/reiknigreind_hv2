
############################################################################
# data is the csv file to find non word errors in
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
findNonWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data["OurGuess"] <- ""
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }
  
  # Add the correction of the word to the CorrectWord column
  lengthData <- length(data$Word)
  
  for (i in 1:5000) {
    
    print(i)
    
    if(exists('correct')) {
      rm(correct)
    }
    
    tmpWord <- tolower(data$Word[[i]])
    
    if(!grepl('^[[:alpha:]]+$', tmpWord)){
      data[i, 5] <- tmpWord
      next
    }
    
    index <- match(tmpWord, dictionary$Word)
    
    if(!is.na(index)) {
      if(dictionary[index,]$Count > 500) {
        data[i, 5] <- tmpWord
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
    
    data[i, 5] <- correct[which(correct$Weight == max(correct$Weight)),]$Word
    
    if( data[i, 1] != data[i, 5]){
      print(data[i, 1])
      print(data[i, 5])
    }
    
  }
  return(data)
}

words <- findNonWordErrors('althingi_errors/079.csv', TRUE)
