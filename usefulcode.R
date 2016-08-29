filedata <- rchoose.files(default = getwd())
filedata <- read_excel(filedata)
filedata <-as.data.frame(filedata)

############################################################################################################
# Fix dates of birth 

for (i in 1:length(cutdown$DOB)){
  if ((nchar(cutdown[i,12])) == 7  && !is.na(cutdown[i,12])){
    cutdown[i,12] <- paste0("0", cutdown[i,12])
    
  }
  
  
}
cutdown$DOB <- as.Date(cutdown$DOB, format = "%m%d%Y")

#################################################
# convert columns to numbers

for (j in 13:ncol(cutdown)){
  cutdown[,j] <- as.character(cutdown[,j])
  cutdown[,j] <- as.numeric(cutdown[,j])
  
}
####################################
################################################################################################

cutdown$BUS_Title <- as.character(cutdown$BUS_Title)
cutdown$BUS_Title <- gsub(".*retired.*", "Retired", cutdown$BUS_Title, ignore.case = TRUE)
########################################
if (cutdown[r,4] == "Retired" && !is.na(cutdown[r,4]))

