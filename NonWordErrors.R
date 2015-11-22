############################################################################
# Corrects non-word errors in a text.
#
# Authors: Kjartan Marteinsson, Snorri Agust Snorrason, Tumi Snaer Gislason.
############################################################################
library(data.table)

# data is the csv file to find non word errors in
findNonWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }
  # Add in a column for guess for the correct word.
  data["OurGuess"] <- ""
  
  # Since this is not context sensative only need to correct each word once.
  wordData <- unique(data$Word)
  
  # Go through each unique word and correct it.
  for (i in 1:length(wordData)){
    # Get the correction method guess.
    guess <- correctWord(wordData[[i]])
    
    # Add the guess to all occurance of the word in the data.
    data[which(data$Word == wordData[[i]]),]$OurGuess <- guess
  }
  
  return(data)
}

# Function that checks and corrects a word for a non-word error.
correctWord <- function(word) {
  # Put the word to lowercase, since dictionary in lowercase.
  word <- tolower(word)
  
  # Only interested in words.
  if(!grepl('^[[:alpha:]]+$', word)){
    return(word)
  }
  
  # Only interested in words that are +-1 length away from the
  # original word.
  size <- nchar(word)
  currentDictionary <- get(paste0("dictionary", size))
  
  # Check if word exists in dictionary.
  index <- match(word, currentDictionary$Word)
  
  # If word is common enough then assume it is correct.
  if(!is.na(index)) {
    if(currentDictionary[index,]$Count > 500) {
      return(word)
    } 
    # If word is uncommon still assume it is fairly likely to be correct.
    else if(currentDictionary[index,]$Count < 100) {
      currentDictionary[index,]$Count = 100
    }
  }
  # If word is not in the dictionary still assume it is fairly likely to be correct.
  else {
    currentDictionary = rbind(currentDictionary, data.frame(Word = word, Count = 100, stringsAsFactors = FALSE))
  }
  # Find the edit distance to all candidate words.
  editDistDf <- data.frame(currentDictionary$Word, matrix(adist(word, currentDictionary$Word), byrow=T), currentDictionary$Count, stringsAsFactors = FALSE)
  colnames(editDistDf) <- c('Word', 'Adist', 'Count')
  # Only want words that are 2 or less edit distance away from original word.
  for (j in 0:2) {
    # Find the most likely candidate at each edit distance.
    maxFreq <- editDistDf[which(editDistDf$Adist == j),]
    maxFreq <- maxFreq[which(maxFreq$Count == max(maxFreq$Count)),]
    # Larger edit distances are less likely than smaller ones.
    maxFreq <- data.frame(maxFreq$Word, (maxFreq$Count/(50^j)), stringsAsFactors = FALSE)
    colnames(maxFreq) <- c('Word', 'Weight')
    if(exists('correct')) {
      correct <- rbind(correct, maxFreq)
    } else {
      correct <- maxFreq
    }
  }
  # Take the candidate with highest likelyhood.
  guess <- correct[which(correct$Weight == max(correct$Weight)),]$Word[1]

  return(guess)
}