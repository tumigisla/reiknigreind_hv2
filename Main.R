############################################################################
# This script handles all the preparation, i.e. importing libraries, generating
# dictionaries and then runs the program itself.
# This is the only file you need to source.
#
# Make sure you set the working directory to the source file location!
#
# Authors: Kjartan Marteinsson, Snorri Ágúst Snorrason, Tumi Snær Gíslason.
############################################################################

# TRUE iff object exists in Global Environment
objectExists <- function(object) {
  return(exists(as.character(substitute(object))))
}

# Package manager. Installs required packages if they're not already installed.
packages <- c("data.table", "plyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Checks for existance of all the dictionaries needed.
# If they don't exist they're either generated from existing .csv
# files, but of these .csv files don't exist they're generated.
# Nb: The scripts for generating the .csv files are time consuming.
# (The .csv files should be in the project dir)
if (!objectExists(dictionary)) {
  if (!file.exists('dictionary.csv'))
    source('CreateDictionary.R')
  else 
    dictionary <- read.csv('dictionary.csv', stringsAsFactors = FALSE, encoding='UTF-8')
}

if (!objectExists(dictionaryLemma) | 
    !objectExists(dictionaryTag) | 
    !objectExists(dictionaryLink)) {
  filesExist <-file.exists('dictionarylemma.csv', 'dictionarytag.csv', 'dictionarylink.csv') 
  if (FALSE %in% filesExist) {
    source('CreateRealDictionary.R')
  }
  else {
    if (!objectExists(dictionaryLemma)) dictionaryLemma <- read.csv('dictionarylemma.csv', stringsAsFactors = FALSE, encoding='UTF-8')
    if (!objectExists(dictionaryTag)) dictionaryTag <- read.csv('dictionarytag.csv', stringsAsFactors = FALSE, encoding='UTF-8')
    if (!objectExists(dictionaryLink)) dictionaryLink <- read.csv('dictionarylink.csv', stringsAsFactors = FALSE, encoding='UTF-8')
  }
}
dictionaryLemma <- apply(dictionaryLemma, 1, function(e) { iconv(e, from = "UTF-8", to="ASCII", sub="byte")})
dictionaryLemma <- data.frame(t(dictionaryLemma), stringsAsFactors = FALSE)

# Create dictionaries for each word length range faster non-word corrections.
for (i in 1:max(nchar(dictionary$Word))) {
  assign(paste0("dictionary", i), dictionary[which(nchar(dictionary$Word) < i + 2 & nchar(dictionary$Word) > i - 2),])
}
# The spelling corrector checks for both non word errors
# and real word errors.
source('NonWordErrors.R')
source('RealWordErrors.R')