VisualizeMyData <- function(x)
{
TypeOFPlot <- readline("Choose a plot type: 1- histogram, 2- boxplot  ")
TypeOFPlot <- as.numeric(TypeOFPlot)

if (TypeOFPlot == 2)
{
  sCOLUMN <- list(ncol(x))
  sCOLUMN <- colnames(x)
  for (h in 1:ncol(x))
  {
    print(paste(h, sCOLUMN[[h]][1]))
    
  }
  Xindependent <- readline("Type the NUMBER of the independent variable  ")
  Xindependent <- as.numeric(Xindependent)
  xl<- sCOLUMN[[Xindependent]][1]
  
  for (h in 1:ncol(x))
  {
    print(paste(h, sCOLUMN[[h]][1]))
    
  }
  ydependent <- readline("Type the NUMBER of the dependent variable  ")
  ydependent <- as.numeric(ydependent)
  yl<- sCOLUMN[[ydependent]][1]
  
  
  boxplot(x[[ydependent]]~x[[Xindependent]],data = x, xlab=xl, ylab=yl)
}
if (TypeOFPlot == 1)
{
  sCOLUMN <- list(ncol(x))
  sCOLUMN <- colnames(x)
  for (h in 1:ncol(x))
  {
    print(paste(h, sCOLUMN[[h]][1]))
    
  }
  Xindependent <- readline("Type the NUMBER of the independent variable  ")
  Xindependent <- as.numeric(Xindependent)
  xl<- sCOLUMN[[Xindependent]][1]
  print(paste("The minimum value in this column is ",min(x[[Xindependent]])))
  print(paste("The maximum value in this column is ",max(x[[Xindependent]])))
  print(paste("The length of this column is  ",length(x[[Xindependent]])))
  BINS <- readline("How many bins do you want? ")
  BINS <- as.numeric(BINS)
  hist(x[[Xindependent]], breaks=BINS,col="blue",xlab=xl)
}

  
  
}