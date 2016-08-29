#convert dates
#if str(x) yeilds dates as factors run part 1 and part 2 to convert to dates
#if str(x) yeilds dates as characters run part 2 to convert to dates


#Part 1
  for(p in 1:ncol(x))
  {
    for (q in 1:length(x[[p]]))
    {
     if (class(x[q,p])== "factor")
     {
      x[[p]] <- as.character(x[[p]])
     }
    }
  }
  

  
#Part 2
  for (i in 1:ncol(x))
  {
    for (j in 1:length(x[[i]]))
    {
      if (class(x[j,i])=="character"& (grepl("[[:digit:]]{4}[[:punct:]][[:digit:]]{2}[[:punct:]][[:digit:]]{2}", x[j,i])==TRUE))
        {
          x[[i]] <- ymd(x[[i]])
        }
    }
    
    
  }
  

