library(tidyverse)
printDF <- function(x) {
  print(x[1:10,1:3])
}
printDFinfo <- function(x) {
  print("Data Frame Information")
  print("Data Frame names= samples")
  x %>%
  names() %>%
  print()
  
  print("Data Frame dimensions")
  x %>%
  dim() %>%
  print()
  
  print("Data Frame structure")
  x %>%
  str() %>%
  print()

  print("Data Frame summary")
  x %>%
  summary() %>%
  print()

  print("Data Frame column types")
  x %>%
  sapply(class) %>%
  print()

  print("Data Frame column class unique")
  x %>%
  sapply(class) %>% 
  unique() %>%
  # table() %>%
  print()
}

metadataFunkyFunction <- function(x) {
    metadata <- 
  data.frame(Colname = names(x),
           class = sapply(x, class),
           row.names = NULL)
  return(
    metadata
  )
}
printDFinfo(DATA)

printDF(DATA)
metadataFunkyFunction(DATA)

DATA%>% 
  names()

RepNameFunction <- function(x) {
  NewName <- names(x)
  if (any(grepl("Lib*", NewName))) {
    NewName <- NewName %>%
      gsub("Lib151Humandendritic", "DC.", .) %>%
      gsub("LPSrep*", "pop", .) %>%
      gsub("pop", ".r", .) %>%
      gsub("_IP", "", .)
  } else {
    OriName <- NewName
  }
  
  names(x) <- NewName
  return(x)
}
df <- DATA
df <- RepNameFunction(df)
df%>% names()
