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
    start <- dictionaryTag[dictionaryTag$Tag == data$Tag[i - 1] & dictionaryTag$nextTag == data$Tag[i],]$Count
    finish <- dictionaryTag[dictionaryTag$Tag == data$Tag[i] & dictionaryTag$nextTag == data$Tag[i + 1],]$Count
    countA <- sum(dictionaryTag$Count[dictionaryTag$Tag == data$Tag[i - 1]], na.rm = TRUE)
    countB <- sum(dictionaryTag$Count[dictionaryTag$nextTag == data$Tag[i + 1]], na.rm = TRUE)
    if (countA == 0 || countB == 0) {
      print(data$Tag[i - 1])
      print(countA)
      print(data$Tag[i + 1])
      print(countB)
    }
    propA <- start/countA
    propB <- finish/countB
    print(propA * propB)
  }
  
}

words <- findRealWordErrors('althingi_errors/079.csv', TRUE)
