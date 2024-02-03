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

df <- DATA
DATA [1:10,] %>% print()
df[1:10,2:4] %>% print()
DATA %>% names()

library(reshape2)
meltedData <- melt(DATA, id = )
meltedData[1:10,] %>% print()


meltedData <- DATA %>% 
  pivot_longer(
    cols = c(grep("DC", names(DATA))), 
    names_to = "sample", 
    values_to = "log2Count"
  ) %>% select(c("sample","geneSymbol","log2Count"))


log2.data <- DATA %>% 
  pivot_longer(
    cols = c(grep("DC", names(DATA))), 
    names_to = "sample", 
    values_to = "log2Count"
  ) %>% select(c("sample","geneSymbol","log2Count"))

log2.data


#apply to sun over the log2.data column
fGenes  <- apply(log2.data[,"log2Count"], 2, function(x) {
  #create a logical vector of log2 genes that are between 4 and 9
  (x>=4 & x<=9) 
}) %>% as.logical()




apply(filtered.data[,"log2Count"], 2, function(y) {
  sum(is.na(df$col))
  }) %>% print(y)

sum(is.na(filtered.data$log2Count))


hist(log2.norm, main = "Histogram of normalized log2 gene expression levels", xlab = "Normalized log2 gene expression levels", col = "lightblue")
hist(filtered_dataLog2norm$log2Count, main = "Histogram of normalized log2 gene expression levels", xlab = "Normalized log2 gene expression levels", col = "lightblue")
hist(filtered.data$log2Count, main = "Histogram of normalized log2 gene expression levels", xlab = "Normalized log2 gene expression levels", col = "lightblue")


