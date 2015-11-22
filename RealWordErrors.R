############################################################################
# Finds and correct word errors (real and non) in a text.
#
# Authors: Kjartan Marteinsson, Snorri Agust Snorrason, Tumi Snaer Gislason.
############################################################################
library(plyr)
library(data.table)

# Function that finds errors in a text dataset.
findRealWordErrors <- function(data, csv=FALSE) {
  
  # Read in the csv file if we haven't done that already
  if (csv) {
    data <- read.csv(data, encoding='UTF-8', colClasses=c('character', 'character', 'character', 'character'))
    data["OurGuess"] <- ""
    data$Word <- lapply(data$Word, tolower)
    data$CorrectWord <- lapply(data$CorrectWord, tolower)
  }
  # Only correct real words.
  data <- subset(data, grepl('(^[[:alpha:]]+$)|^\\.$', data$Word), select = colnames(data))
  sum <- 0
  
  # Store dictionaries as data.table to attempt to speed up function.
  dictionaryTag <- data.table(dictionaryTag)
  dictionaryLemma <- data.table(dictionaryLemma)
  dictionaryLink <- data.table(dictionaryLink)
  setkey(dictionaryLink)

  for(i in 4025:4027) {
    # Set word to check to lowercase, since dictionaries in lower case.
    data$Word[i + 1] <- tolower(data$Word[i + 1])
    data$Lemma[i + 1] <- tolower(data$Lemma[i + 1])
    # We look at the word, along with the previous word and the next word.
    # Call them ABC.
    print(i)
    print(paste0(data$Word[i-1], " ", data$Word[i], " ", data$Word[i + 1]))
    # Look-ahead and correct any non-word error in the next word.
    data[i + 1,] <- findNonWordError(data[i + 1,])
    # Check how often AB appear together.
    before <- dictionaryLemma[which(dictionaryLemma$Lemma == data$Lemma[i - 1] & dictionaryLemma$nextLemma == data$Lemma[i]),]$Count
    # Check how often BC appear together.
    after <- dictionaryLemma[which(dictionaryLemma$Lemma == data$Lemma[i] & dictionaryLemma$nextLemma == data$Lemma[i + 1]),]$Count
    # If the words appear together often enough in the dictionary assume the word B is correct.
    if(length(before) & length(after)) {
      if(before > 15 & after > 15) {
        data$OurGuess[i] <- data$Word[i]
        next
      }
    }
    
    # Find all candidate tags by using the tags of A and C and seeing which tags appear between them in the dictionary.
    possibleTags <- findCandidates(dictionaryTag, data$Tag[i - 1], data$Tag[i + 1])
    # Only check the most common tags. This is done to speed-up the algorithm.
    possibleTags <- possibleTags[order(possibleTags$Prob, decreasing = TRUE),]
    possibleTags <- head(possibleTags, 100)
    # Find all candidate lemmas by using the lemmas of A and C and seeing which lemmas appear between them in the dictionary.
    possibleLemmas <- findCandidates(dictionaryLemma, data$Lemma[i - 1], data$Lemma[i + 1])
    # Find all word, tag, lemma entries that match the candidate lemmas and tags.
    linkedDict <- dictionaryLink[which(dictionaryLink$Tag %in% possibleTags$Candidates & dictionaryLink$Lemma %in% possibleLemmas$Candidates),]
    linkedDict['Prob'] <- ''
    linkedDictLength <- nrow(linkedDict)
    if (linkedDictLength == 0) {
      data$OurGuess[i] <- data$Word[i]
      next
    }
    # Save the lexical class of the current word.
    class <- substr(data$Tag[i], 1, 1)
    # Calculate the likelyhood of each candidate.
    print(linkedDictLength)
    for (j in 1:linkedDictLength) {
      # Using log-likelyhood. Plus the likelyhood of the candidate having the given tag and lemma.
      tagProb <- possibleTags$Prob[possibleTags$Candidates == linkedDict$Tag[j]]
      lemmaProb <- possibleLemmas$Prob[possibleLemmas$Candidates == linkedDict$Lemma[j]]
      prob <- tagProb + lemmaProb
      # If the candidate lexical class differs from the words penalize it. Expect the
      # lexical class not to change.
      if (substr(linkedDict$Tag[j], 1, 1) != class) {
        prob <- prob + log10(0.1)
      }
      # Find the edit distances between the candidate and the word. More
      # likely that the word is a lower edit distance away.
      dist <- adist(linkedDict$Word[j], data$Word[i])[[1]]
      # All words more than 2 edit distances away have the same penalty.
      if (dist > 2) {
        prob <- prob - 15
      }
      else {
        prob <- prob + log10(1/(50^dist))
      }
      linkedDict$Prob[j] <- prob
    }
    # Select the word with the highest likelyhood as the correct word.
    data$Word[i] <- linkedDict[which(linkedDict$Prob == max(linkedDict$Prob)),]$Word
    data$OurGuess[i] <- data$Word[i]
  }
  return(data)
}

# Function that finds candidates based on the words in front and the words after.
findCandidates <- function(dictionary, preciding, following) {
  # Find all the words that fit the preceeding word.
  forward <- dictionary[dictionary[[1]] == preciding,]
  # Use log-likelyhood, since we have small probabilites.
  forward[[3]] <- log10(forward[[3]]/sum(forward$Count, na.rm = TRUE))
  colnames(forward)[2] <- 'join'

  # Find all the words that fit the following word.
  backward <- dictionary[dictionary[[2]] == following,]
  backward[[3]] <- log10(backward[[3]]/sum(backward$Count, na.rm = TRUE))
  colnames(backward)[1] <- 'join'
  
  # Select all candidates that appear in both lists and calculate the
  # combined likelyhood.
  candidates <- data.table(merge(forward, backward, by='join'))
  candidates <- data.table(candidates$join, candidates$Count.x + candidates$Count.y)
  colnames(candidates) <- c('Candidates', 'Prob')
  return(candidates)
}

# Function that calls non-word error checker.
findNonWordError <- function(word) {
  # Get a guess for the correct word from the error checker.
  guess <- correctWord(word$Word)
  # If a new word was return need to update the lemma of the word.
  # Assume the tags stay the same.
  if (guess != word$Word) {
    word$Word <- guess
    # Find all possible lemmas that fit the word.
    possibilities <- dictionaryLink[which(dictionaryLink$Word == guess),]
    # Try to select the one with the correct tag, if able.
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
nCorrectGuesses <- sum(as.character(words$OurGuess) == as.character(words$CorrectWord) & words$CorrectWord != '', na.rm = TRUE)
nTotalGuesses <- sum(words$CorrectWord != '' & words$OurGuess != '', na.rm = TRUE)
performance <- nCorrectGuesses / nTotalGuesses