for (i in 1:max(nchar(dictionary$Word))) {
  assign(paste0("dictionary", i), dictionary[which(nchar(dictionary$Word) < i + 2 & nchar(dictionary$Word) > i - 2),])
}