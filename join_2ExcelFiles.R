merge.2excel <- function(x)
{
  require(openxlsx)
  #choose two .xlsx files to be merged
  file_a <- choose.files(default = "", caption = "Select 2 files",
                         multi = TRUE, filters = Filters,
                         index = nrow(Filters))
  
  #prompt user to choose a base file, load and read files
  print(file_a)
  n <- readline("Which file is your base (1 or 2)?")
  n<-as.integer(n)
  loadbase <- loadWorkbook(file_a[n])
  basefile <- readWorkbook(loadbase)
  print("Column names")
  print(names(basefile))
  a <- if (n==1) 2 else 1
  loadjoin <- loadWorkbook(file_a[a])
  joinTobase <- readWorkbook(loadjoin)
  
  
  
  
  
  
  
  
  #Choose column to join on
  c <- readline("Type the name of the reference column for the join?   ") 
  c <- toString(c, width=NULL)
  #print(c)
  IsDate <- readline("Is the column for the join a date? 1 = yes   2 = no   ")
  IsDate <- as.integer(IsDate)
  #str(basefile)
  #print(basefile$Date)
  if (IsDate == 1)
    {
    print("Column names")
    print(names(joinTobase))
    d <- readline("Type the name of the date column for the file you are joining?   ") 
    d <- toString(d, width=NULL)
    as.numeric(basefile[[c]])
    basefile[[c]] <- as.Date(basefile[[c]], origin="1899-12-30")
    as.numeric(joinTobase[[d]])
    joinTobase[[d]] <- as.Date(joinTobase[[d]], origin="1899-12-30")
    } 
  else
    {print("Column names")
    print(names(joinTobase))
    d <- readline("Type the name of the column for the file you are joining?   ") 
    d <- toString(d, width=NULL)}
    
  #print(basefile$Date)
  #Choose type of join
  j <- readline("Choose a join: 1- Full Outer Join on Base, 2- Left Outer Join, 3- Inner Join   ")
  j <- as.integer(j)
  
  if (j==1){
    combination <- merge(basefile,joinTobase, by.x = c,by.y = d,all=TRUE)
    str(combination)
    write.csv(combination,"full_outer_join.csv")
    }
  
  if (j==2){
      combination <- merge(basefile,joinTobase, by = c,all.x=TRUE)
      str(combination)
      write.csv(combination,"left_outer_join.csv")}
  
  if (j==3){combination <- merge(basefile,joinTobase, by = c )
  write.csv(combination,"inner_join.csv")}
  
}
