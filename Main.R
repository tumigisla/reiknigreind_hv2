# TRUE iff object exists in Global Environment
objectExists <- function(object) {
  return(exists(as.character(substitute(object))))
}

# Checks for existance of all the dictionaries needed.
# If they don't exist they're either generated from existing .csv
# files, but of these .csv files don't exist they're generated.
# Nb: The scripts for generating the .csv files are time consuming.
# (The .csv files should be in the project dir)
generateDicts <- function() {
  if (!objectExists(dictionary))
    if (!file.exists('dictionary.csv'))
      source('CreateDictionary.R')
    else 
      dictionary <- read.csv('dictionary.csv', stringsAsFactors = FALSE, encoding='UTF-8')
  
  if (!objectExists(dictionaryLemma) | 
      !objectExists(dictionaryTag) | 
      !objectExists(dictionaryLink))
    if (!file.exists('dictionaries/dictionarylemma.csv', 'dictionaries/dictionarytag.csv', 'dictionaries/dictionarylink.csv'))
      source('CreateRealDictionary.R')
    else {
      if (!dictionaryLemma) dictionaryLemma <- read.csv('dictionaries/dictionarylemma.csv', stringsAsFactors = FALSE, encoding='UTF-8')
      if (!dictionaryTag) dictionaryTag <- read.csv('dictionaries/dictionarytag.csv', stringsAsFactors = FALSE, encoding='UTF-8')
      if (!dictionaryLink) dictionaryLink <- read.csv('dictionaries/dictionarylink.csv', stringsAsFactors = FALSE, encoding='UTF-8')
    }
}

# The spelling corrector checks for both non word errors
# and real word errors.
runSpellingCorrector <- function() {
  source('NonWordErrors.R')
  source('RealWordErrors.R')
}

main <- function() {
  generateDicts()
  runSpellingCorrector()
}

main()