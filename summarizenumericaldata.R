SummarizeNumericalData <- function(x)
{
  yn <- readline("1- Summary of every column, 2- Group columns then summarize  ")
  yn <- as.numeric(yn)
  if (yn == 1)
  {
    myresults <- summary(x)
    fix <-as.data.frame(myresults)
    df <- cbindX(x,myresults)
    write.csv(df, "summaryResults.csv")
    print(head(df))
    print("summaryResults.csv is in your working directory")
  }
  
  
  
  
  else{
  readline("Choose a column to summarize, and columns to group. Hit enter to continue.  ")
  str(x)
  
  sCOLUMN <- list(ncol(x))
  sCOLUMN <- colnames(x)
  for (h in 1:ncol(x))
  {
    print(paste(h, sCOLUMN[[h]][1]))
    
  }
  summarrizeTHIScolumn <- readline("Type the NUMBER of the column you want to summarize  ")
  summarrizeTHIScolumn <- as.numeric(summarrizeTHIScolumn)
  r <- as.data.frame(x[[summarrizeTHIScolumn]])
  
  for (h in 1:ncol(x))
  {
    print(paste(h, sCOLUMN[[h]][1]))
    
  }
  
  groupNumber <- readline("How many columns would you like to group by (e.g. 1, 2,..)?  " )
  groupNumber <- as.numeric(groupNumber)
  responseTotal <- list(groupNumber)
 
    for (i in 1:groupNumber)
    {
      for (h in 1:ncol(x))
      {
        print(paste(h, sCOLUMN[[h]][1]))
        
      }
      
      response <- readline("Type the NUMBER of a column to group by  ")
      response <- as.numeric(response)
      responseTotal[[i]] <- x[[response]]
    }
  print(responseTotal)
  
  agregatedDATA1 <- aggregate(x[[summarrizeTHIScolumn]], by=responseTotal,FUN=mean,na.rm=TRUE)
  names(agregatedDATA1)[names(agregatedDATA1)=="x"] <- "mean"
  
  agregatedDATA2 <- aggregate(x[[summarrizeTHIScolumn]], by=responseTotal,FUN=min,na.rm=TRUE)
  names(agregatedDATA2)[names(agregatedDATA2)=="x"] <- "min"
  
  agregatedDATA3 <- aggregate(x[[summarrizeTHIScolumn]], by=responseTotal,FUN=max,na.rm=TRUE)
  names(agregatedDATA3)[names(agregatedDATA3)=="x"] <- "max"
  
  agregatedDATA4 <- aggregate(x[[summarrizeTHIScolumn]], by=responseTotal,FUN=sd,na.rm=TRUE)
  names(agregatedDATA4)[names(agregatedDATA4)=="x"] <- "sd"
  
  ToGetHer1 <- cbindX(agregatedDATA1,agregatedDATA4)
  ToGetHer2 <- cbindX(agregatedDATA2,agregatedDATA3)
  RESULTS <- cbindX(ToGetHer1,ToGetHer2)
  FinalRESULTS <- cbindX(x,RESULTS)
  write.csv(FinalRESULTS,"GROUPED_summaryResults.csv")
  print(head(FinalRESULTS))
  print("GROUPED_summaryResults.csv is in your working directory")
  }
  }



