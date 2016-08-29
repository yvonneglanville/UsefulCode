#manipulate variable types in base file
str(basefile)
N <- readline("How many of the base file columns need a variable type changed (e.g. 1, 2,...)    ")
N <- as.numeric(N)
if (N > 0){
  for (i in 1:N)
  {
    names(basefile)
    nameOfColumnb <- readline("Type the name of the column you want to change:    ")
    nameOfColumnb <- toString(nameOfColumnb, width=NULL)
    conVertVarTypeb <- readline("Convert to which type: 1- Date, 2- String, 3- Logical, 4- Factor   ")
    conVertVarTypeb <- as.numeric(conVertVarTypeb)
    
    if (conVertVarTypeb == 1)
    {
      basefile[[nameOfColumnb]] <- as.Date(basefile[[nameOfColumnb]],origin="1899-12-30")
      
    }
    
    if (conVertVarTypeb == 2)
    {
      
      basefile[[nameOfColumnb]] <- as.character(basefile[[nameOfColumnb]])
    }
    
    if (conVertVarTypeb == 3)
    {
      basefile[[nameOfColumnb]] <- as.logical(basefile[[nameOfColumnb]])
    }
    
    if (conVertVarTypeb == 4)
    {
      basefile[[nameOfColumnb]] <- as.factor(basefile[[nameOfColumnb]])
    }
    
  }}

############################################################################################################  
#################################################################################
#manipulate variable types in join file
str(joinTobase)
P <- readline("How many of the join file columns need a variable type changed (e.g. 1, 2,...)    ")
P <- as.numeric(P)

if (P > 0){
  
  for (j in 1:P)
  {
    names(joinTobase)
    nameOfColumnj <- readline("Type the name of the column you want to change:    ")
    nameOfColumnj <- toString(nameOfColumnj, width=NULL)
    conVertVarTypej <- readline("Convert to which type: 1- Date, 2- String, 3- Logical, 4- Factor   ")
    conVertVarTypej <- as.numeric(conVertVarTypej)
    
    if (conVertVarTypej == 1)
    {
      joinTobase[[nameOfColumnj]] <- as.Date(joinTobase[[nameOfColumnj]],origin="1899-12-30")
    }
    
    if (conVertVarTypej == 2)
    {
      joinTobase[[nameOfColumnj]] <- as.character(joinTobase[[nameOfColumnj]])
    }
    
    if (conVertVarTypej == 3)
    {
      joinTobase[[nameOfColumnj]] <- as.logical(joinTobase[[nameOfColumnj]])
    }
    
    if (conVertVarTypej == 4)
    {
      joinTobase[[nameOfColumnj]] <- as.factor(joinTobase[[nameOfColumnj]])
    }
    
  }}