setwd("C:/Users/cwyse/OneDrive - Maynooth University/One_Yr_Actigraphy/One_Year")
#make AWD files with this header
# brian
# 19-Jun-2016
# 09:52:00 AM 
# 4 
# 57
# P4012590327
# M

#get files
myfile <- all_files[1]

#function to make text files compatible with Sleep7
functionAWD <- function (mydata) {
  mydata <- read.csv(myfile)
  
  YY <- mydata$Year 
  MM <- mydata$Month
  DD <- mydata$Day
  hh <- mydata$Hour
  mm <- mydata$Minute
  ss <- mydata$Second

  mydata$dat <- datetime <- as.POSIXlt(paste(YY, MM, DD, hh, mm, ss),format = "%Y %m %d %H %M %S")
  mydata$by1min <- cut(mydata$dat, breaks = "60 sec")    #cut into 1 min 
  mydata$act <- mydata$Activity..MW.counts.

  dat.summary <- aggregate(act ~ by1min, FUN=sum, data=mydata)
  dat.summary$act <- ifelse(dat.summary$act < 0, 0, dat.summary$act) # Set negative values to 0

  write.csv(dat.summary, file = paste0("awd_",mydata,".csv"), row.names=FALSE)
}

#apply function to all files
sapply(all_files,functionAWD)
