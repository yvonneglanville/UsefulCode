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