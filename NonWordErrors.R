############################################################################
# data is the csv file to find non word errors in
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################
findNonWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data <- subset(data, grepl('^[[:alpha:]]+$', data$Word))
    data["OurGuess"] <- ""
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }
  
  # Add the correction of the word to the CorrectWord column
  lengthData <- length(data$Word)
  for (i in 1:200) {
    tmpWord <- data$Word[[i]]
    
    if (!(tmpWord %in% dictionary$Word)) {
      # Data frame with: [EditDist, Word] for all words in dictionary
      editDistDf <- data.frame(data.frame(matrix(adist(tolower(tmpWord), dictionary$Word), byrow=T)), dictionary$Word)
      colnames(editDistDf) <- c('EditDist', 'Word')
      
      # Extract words having the min edit distance.
      minEditDist <- min(editDistDf$EditDist)
      minEditDistWords <- subset(editDistDf, EditDist == minEditDist, select=c('Word')) 
      
      # Pick the word having the most prob of showing up, if we have more than one word.
      # Otherwise we just use the word itself.
      if (length(minEditDistWords$Word) > 1) {
        maxFreq <- 0
        maxFreqWord <- ''
        for (word in minEditDistWords$Word) {
          freq <- as.numeric(subset(dictionary, Word == word, select = c('Count')))
          if (freq > maxFreq) {
            maxFreq = freq
            maxFreqWord = word
          }
        }
        tmpWord <- maxFreqWord
      }
      else
        tmpWord <- as.character(minEditDistWords$Word)
      }
    
    # Update the CorrectWord column in the data frame
    data[i, 5] <- tmpWord
  }
  return(data)
}

words <- findNonWordErrors('althingi_errors/079.csv', TRUE)
