manipulate.2excel <- function(x)
{
  require(openxlsx)
  
  #choose two .xlsx files to be merged
  file_a <- choose.files(default = "", caption = "Select 2 files",
                         multi = TRUE, filters = Filters,
                         index = nrow(Filters))
  
  #prompt user to choose a base file, load and read both base and join files
  print(file_a)
  n <- readline("Which file is your base (1 or 2)?")
  n <- as.integer(n)
  loadbase <- loadWorkbook(file_a[n])
  basefile <- readWorkbook(loadbase)
  a <- if (n==1) 2 else 1
  loadjoin <- loadWorkbook(file_a[a])
  joinTobase <- readWorkbook(loadjoin)
  
  #################################################################################
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
#########################################################################################################  
  #Choose base column for the join
  print(names(basefile))
  c <- readline("Base file for join: type name of column?   ") 
  c <- toString(c, width=NULL)
  
  #Choose join column
  print(names(joinTobase))
  d <- readline("Type the name of the column for the join?   ") 
  d <- toString(d, width=NULL)
  
##########################################################################################################  
  #Choose type of join
  k <- readline("Choose a join: 1- Full Outer Join on Base, 2- Left Outer Join, 3- Inner Join   ")
  k <- as.integer(k)
  
  if (k==1)
    {
    combination <- merge(basefile,joinTobase, by.x = c,by.y = d,all=TRUE)
    write.csv(combination,"full_outer_join.csv")
    }
  
  if (k==2){
    combination <- merge(basefile,joinTobase, by = c,all.x=TRUE)
    write.csv(combination,"left_outer_join.csv")}
  
  if (k==3){combination <- merge(basefile,joinTobase, by = c )
  write.csv(combination,"inner_join.csv")}
  
#############################################################################################################  
readline("A .csv file has been output to your working directory. Hit enter for more file manipulation options.")
YesNo <- readline("Would you like to summarize or visualize your data? (y or n)  ")
YesNo <- as.character(YesNo)
if (YesNo == "y")
{
  SunVis <- readline("Type 1 to summarize    Type 2 to visualize   ")
  SUnVis <- as.numeric(SunVis)
  
    if (SunVis == 1){SummarizeNumericalData(combination)}
    if (SunVis == 2){VisualizeMyData(combination)}
  
  
}
  
    
}
