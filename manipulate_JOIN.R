manipulate.2excel <- function(x)
{
  #require(openxlsx)
  require(readxl)
  #choose two .xlsx files to be merged
  file_a <- choose.files(default = "", caption = "Select 2 files",
                         multi = TRUE, filters = Filters,
                         index = nrow(Filters))
  print(file_a)
  n <- readline("Which file is your base (1 or 2)? ")
  n <- as.integer(n)
  basefile <- read_excel(file_a[n], sheet=1, col_names = TRUE,col_types = NULL, na="", skip=0)
  a <- if (n==1) 2 else 1
  joinTobase <- read_excel(file_a[a], sheet=1, col_names = TRUE,col_types = NULL, na="", skip=0)


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
  k <- readline("Choose a join: 1- Full Outer Join, 2- Left Join, 3- Inner Join, 4- Right Join   ")
  k <- as.integer(k)
  
  if (k==1){
    combination <- merge(basefile,joinTobase, by.x = c,by.y = d,all=TRUE)
    print(head(combination))
    write.csv(combination,"full_outer_join.csv")
    write.xlsx(combination, "excel_full_outer.xlsx")
    }
  
  if (k==2){
    combination <- merge(basefile,joinTobase, by = c, all.x=TRUE)
    print(head(combination))
    write.csv(combination,"left_join.csv")
    write.xlsx(combination, "excel_left_join.xlsx")
    }
  
  if (k==3){
    combination <- merge(basefile, joinTobase, by=c )
    print(head(combination))
    write.csv(combination, "inner_join.csv")
    write.xlsx(combination, "excel_inner_join.xlsx")
    }
  
  if (k==4){
    combination <- merge(basefile, joinTobase, by=c, all.y=TRUE)
    print(head(combination))
    write.csv(combination, "right_join.csv")
    write.xlsx(combination, "excel_right_join.xlsx")
  }
  
#############################################################################################################  
readline("A .csv and a .xlsx file have been output to your working directory. 
         Hit enter for more file manipulation options.")

YesNo <- readline("1- Summarize Data   2- Visualize Data  3- End  ")
YesNo <- as.numeric(YesNo)
if (YesNo == "1"){SummarizeNumericalData(combination)}
if (YesNo == "2"){VisualizeMyData(combination)} 
 else return(head(combination))
    
}
