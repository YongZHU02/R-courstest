pollutantmean<-function(x)
{
  setwd("~/Documents/R Project/For Course/R-courstest/Data")
  i<-1:322
  my_data<-NULL
  mpath<-getwd()
  mpath <- paste0(mpath,sep = "/",x)
  setwd(mpath)
  for(i in 1:332) 
  {
    i <- formatC(i, width = 3, flag = 0) 
    mpath <- paste0(i, ".csv")
    my_data <- rbind(my_data,read_csv(file = mpath))
  }
}
