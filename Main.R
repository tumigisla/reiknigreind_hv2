############################################################################
# This script handles all the preparation, i.e. importing libraries, generating
# dictionaries and then runs the program itself.
# This is the only file you need to source.
#
# Make sure you set the working directory to the source file location!
#
# Authors: Kjartan Marteinsson, Snorri Agust Snorrason, Tumi Snaer Gislason.
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

# For converting icelandic letters to byte order marks when comparing
lookup <- data.frame(
            c('á', 'é', 'í', 'ó', 'ú', 'ý', 'ð', 'þ', 'æ', 'ö'), 
            c('<c3><al>', '<c3><a9>', '<c3><ad>', '<c3><b3>', '<c3><ba>', '<c3><bd>', '<c3><b0>', '<c3><be>', '<c3><a6>', '<c3><b6>')
          )
colnames(lookup) <- c('original', 'convert')

# Create dictionaries for each word length range faster non-word corrections.
for (i in 1:max(nchar(dictionary$Word))) {
  assign(paste0("dictionary", i), dictionary[which(nchar(dictionary$Word) < i + 2 & nchar(dictionary$Word) > i - 2),])
}
# The spelling corrector checks for both non word errors
# and real word errors.
source('NonWordErrors.R')
source('RealWordErrors.R')
words <- findRealWordErrors('althingi_errors/079.csv', TRUE)