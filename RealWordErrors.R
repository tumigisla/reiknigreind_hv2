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
    countForward <- sum(dictionaryTag$Count[dictionaryTag$Tag == data$Tag[i - 1]], na.rm = TRUE)
    countBackward <- sum(dictionaryTag$Count[dictionaryTag$nextTag == data$Tag[i + 1]], na.rm = TRUE)
    tagsForward <- dictionaryTag[which(dictionaryTag$Tag == data$Tag[i - 1]),]
    tagsForward$Count <- tagsForward$Count/countForward
    tagsBackward <- dictionaryTag[which(dictionaryTag$nextTag == data$Tag[i + 1]),]
    tagsBackward$Count <- tagsBackward$Count/countBackward
    
    countForward <- sum(dictionaryWord$Count[dictionaryWord$Word == data$Word[i - 1]], na.rm = TRUE)
    countBackward <- sum(dictionaryWord$Count[dictionaryWord$Word == data$Word[i + 1]], nna.rm = TRUE)
    wordForward <- dictionaryWord[which(dictionaryWord$Word == data$Word[i - 1]),]
    wordForward$Count <- wordForward$Count/countForward
    wordBackward <- dictionaryWord[which(dictionaryWord$Word == data$Word[i + 1]),]
    wordBackward$Count <- wordBackward$Count/countBackward
    
    print(dictionaryLink[which(dictionaryLink$Tag %in% tagsForward$Tag & dictionaryLink$Word %in% wordForward$Word),])
    
  }
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)
