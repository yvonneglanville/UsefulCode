
file_a <- choose.files(default = "", caption = "Select a file",
                       multi = TRUE, filters = Filters,
                       index = nrow(Filters))


if (grepl(".csv", file_a) ==TRUE)
{
  ctt <- read.csv(file_a,header=TRUE, sep=",", as.is=TRUE)
  ctt <- as.data.frame(ctt)
}
str(ctt)
#Date format e.g. 10-Mar-2001
for (i in 1:ncol(ctt))
  {
      
      for (s in 1:length(ctt[[i]]))
      {
  
        if (grepl("[[:digit:]]{2}[[:punct:]][[:alpha:]]{3}[[:punct:]][[:digit:]]{4}",ctt[s,i]) ==TRUE)
       
        {
          ctt[[i]]<-dmy(ctt[[i]])
        }
        else if (grepl("[[:digit:]]{2}[[:punct:]][[:digit:]]{2}[[:punct:]][[:digit:]]{4}",ctt[s,i])==TRUE)
        {
          ctt[[i]]<-mdy(ctt[[i]])
          
        }
        
        
      }
 
  } 
str(ctt)    







#Date format e.g. 06/28/2003
for (j in 1:ncol(ctt))
{
  for(p in 1:length(ctt[[i]]))
  {
  if (grepl("[[:digit:]]{2}[[:punct:]][[:digit:]]{2}[[:punct:]][[:digit:]]{4}",ctt[p,j])==TRUE)
  {
    ctt[[i]]<-mdy(ctt[[i]])
    
  }
  }
}
str(ctt)
 myData <- as.data.frame(ctt)
 str(myData)
 print("Name of data frame: myData")
