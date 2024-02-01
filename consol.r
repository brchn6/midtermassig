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



# *hint: Use functions `cor(method = x)`, `pheatmap()`*

printDF(df)
#df[1:10,] %>% view()
df %>% names()

FunctionForCorMetSpe <- function(x){
  corMat <- x[,grep("^DC", colnames(x))] %>% 
    cor(method = "spearman")
  return(corMat)
}
FunctionForCorMetpearson <- function(x){
  corMat <- x[,grep("^DC", colnames(x))] %>% 
    cor(method = "pearson")
  return(corMat)
}

FunctionFor_rCor <- function(x){
  corMat <- x[,grep("^DC", colnames(x))] %>% 
    as.matrix() %>%
    rcorr(type = c("pearson")) %>%
  return(corMat)
}

FunForGenHeatMap <- function(x){
  x %>%
    pheatmap(
      color = colorRampPalette(c("#62a1db", "#e7d87d", "#dd9f40","#b4451f","#b01111"))(100),
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      show_rownames = TRUE,
      show_colnames = TRUE,
      main = "Heatmap of Pearson Correlation"
    )
    }

DATA %>%
  select(-c("DC.0hr.r1","DC.0hr.r2","DC.0hr.r3"))%>%
  FunctionForCorMetpearson() %>%  FunForGenHeatMap()

DATA %>%
  select(-c("DC.3hr.r1","DC.3hr.r2","DC.3hr.r3"))%>%
  FunctionForCorMetpearson() %>%  FunForGenHeatMap()

DATA %>%
  select(-c("DC.6hr.r2"))%>%
  FunctionForCorMetpearson() %>%  FunForGenHeatMap()

FunctionForCorMetpearson(DATA)  %>% FunForGenHeatMap()
FunctionForCorMetSpe(DATA)  %>% FunForGenHeatMap()


library(magrittr)
library(Hmisc)


df %>% FunctionFor_rCor() %>% print()

