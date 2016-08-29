manipulate.2excel <- function(x)
{
  require(openxlsx)
  
  #choose two .xlsx files to be merged
  file_a <- choose.files(default = "", caption = "Select 2 files",
                         multi = TRUE, filters = Filters,
                         index = nrow(Filters))
  
  #prompt user to choose a base file and read both files
  print(file_a)
  n <- readline("Which file is your base (1 or 2)? ")
  n <- as.integer(n)
  basefile <- read.csv(file_a[n],header=TRUE, sep=",", as.is=TRUE)
  basefile <- as.data.frame(basefile)
  a <- if (n==1) 2 else 1
  joinTobase <- read.csv(file_a[a],header=TRUE, sep=",", as.is=TRUE)
  joinTobase <- as.data.frame(joinTobase)
  #str(basefile)
  #Date format e.g. 10-Mar-2001
  for (i in 1:ncol(basefile))
  {
    
    for (s in 1:length(basefile[[i]]))
    {
      
      if (grepl("[[:digit:]]{2}[[:punct:]][[:alpha:]]{3}[[:punct:]][[:digit:]]{4}",basefile[s,i]) ==TRUE)
      {
        basefile[[i]]<-dmy(basefile[[i]])
      }
    }
    
  } 
  
  #Date format e.g. 06/28/2003
  for (j in 1:ncol(basefile))
  {
    for(p in 1:length(basefile[[i]]))
    {
      if (grepl("[[:digit:]]{2}[[:punct:]][[:digit:]]{2}[[:punct:]][[:digit:]]{4}",basefile[p,j])==TRUE)
      {
        basefile[[i]]<-mdy(basefile[[i]])
        
      }
    }
  }
 ################################################################################################################ 
  #Date format e.g. 10-Mar-2001
  for (i in 1:ncol(joinTobase))
  {
    
    for (s in 1:length(joinTobase[[i]]))
    {
      
      if (  (grepl("[[:digit:]]{2}[[:punct:]][[:alpha:]]{3}[[:punct:]][[:digit:]]{4}",joinTobase[s,i]) ==TRUE)|
          (grepl("[[NA]]",joinTobase[s,i])==TRUE)))
      {
        joinTobase[[i]]<-dmy(joinTobase[[i]])
      }
    }
    
  } 
  
  #Date format e.g. 06/28/2003
  for (j in 1:ncol(joinTobase))
  {
    for(p in 1:length(joinTobase[[i]]))
    {
      if (grepl("[[:digit:]]{2}[[:punct:]][[:digit:]]{2}[[:punct:]][[:digit:]]{4}",joinTobase[p,j])==TRUE)
      {
        joinTobase[[i]]<-mdy(joinTobase[[i]])
        
      }
    }
  }
 
 
 ####################################################################################################### 
  # (Base file) convert character strings to factors to allow for summary and pivot
  
  for (v in 1:ncol(basefile))
  {
    if (typeof(basefile[[v]]) == "character")
    {
      basefile[[v]] <- as.factor(basefile[[v]])
    }
    
  }

######################################################################################################## 
  # (Join file) convert character strings to factors to allow for merge with base file
  
  for (w in 1:ncol(joinTobase))
  {
    if (typeof(joinTobase[[w]]) == "character")
    {
      joinTobase[[w]] <- as.factor(joinTobase[[w]])
    }
    
  }

#########################################################################################################  
  #Choose base and join columns for the join
  
  holdingTank <- list(ncol(basefile))
  holdingTank <- colnames(basefile)
 
  for (g in 1:ncol(basefile))
  {
   print(paste(g, holdingTank[[g]][1]))
    
  }
 
  c <- readline("NUMBER of the base column to join on   ") 
  c <- as.numeric(c)
  c <- holdingTank[[c]][[1]]
  
  
  
  holdingTanka <- list(ncol(joinTobase))
  holdingTanka <- colnames(joinTobase)
  
  for (h in 1:ncol(joinTobase))
  {
    print(paste(h, holdingTanka[[h]][1]))
    
  }
  
  d <- readline("NUMBER of the join column   ") 
  d <- as.numeric(d)
  d <- holdingTanka[[d]][1]
  
##########################################################################################################  
  #Choose type of join
  k <- readline("Choose a join: 1- Full Outer Join on Base, 2- Left Outer Join, 3- Inner Join   ")
  k <- as.integer(k)
  
  if (k==1)
    {
    combination <- merge(basefile,joinTobase, by.x = c,by.y = d,all=TRUE)
    print(head(combination))
    write.csv(combination,"full_outer_join.csv")
    }
  
  if (k==2)
    {
    combination <- merge(basefile,joinTobase, by = c,all.x=TRUE)
    print(head(combination))
    write.csv(combination,"left_outer_join.csv")
    }
  
  if (k==3)
    {
    combination <- merge(basefile,joinTobase, by = c )
    print(head(combination))
    write.csv(combination,"inner_join.csv")
    }
  
#############################################################################################################  
readline("A .csv file has been output to your working directory. 
         Hit enter for more file manipulation options.")

YesNo <- readline("1- Summarize Data   2- Visualize Data  3- End  ")
YesNo <- as.numeric(YesNo)
if (YesNo == "1"){SummarizeNumericalData(combination)}
if (YesNo == "2"){VisualizeMyData(combination)} 
 else return(head(combination))
    
}
