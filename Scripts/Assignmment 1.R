
text_pollutantmean<-function(x)
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

pollutantmean<-function(x,y,z=1:332)
{
  setwd("~/Documents/R Project/For Course/R-courstest/Data")
  mpath<-list.files(x, full.names=TRUE)
  mdata<-NULL # 或者可以改成 mdata <- data.frame()
  for(i in z)
    {
    mdata<-rbind(mdata,read.csv(mpath[i]))
    }
  subdata<-mdata[,y]
  result<-mean(subdata,na.rm=TRUE)
  result
}

complete<-function(x,y)
{
  setwd("~/Documents/R Project/For Course/R-courstest/Data")
  mpath<-list.files(x,full.names=TRUE)
  mdata<-data.frame()
  result<-data.frame()
  for(i in y)
  {
    mdata<-read.csv(mpath[i])
    c<-complete.cases(mdata)
    subdata<-mdata[c,]
    j<-lengths(subdata)
    k<-j[1]
    result<-rbind(result,data.frame(id=i,nobs=k))
  }
  result
}

corr<-function(x,y=0)
{
  setwd("~/Documents/R Project/For Course/R-courstest/Data")
<<<<<<< HEAD
  mpath<-list.files(x, full.names=TRUE)
=======
  mpath<-list.files(x,full.names=TRUE)
>>>>>>> 83649c2ae6698737b0112255024dba6e2110c36a
  mdata<-NULL
  result<-data.frame()
  for(i in 1:332)
  {
    mdata<-read.csv(mpath[i])
    c<-complete.cases(mdata)
    subdata<-mdata[c,]
    j<-lengths(subdata)
    k<-j[1]
    result<-rbind(result,data.frame(id=i,nobs=k))
  }
  good<-result
  id<-good[which(good[,"nobs"]>y),]
  m_id<-id$id
  result<-NULL
  for(i in m_id)
  {
    mdata<-read.csv(mpath[i])
    subdata1<-mdata$sulfate
    subdata2<-mdata$nitrate
    result<-rbind(result,cor(subdata1,subdata2,use = "complete.obs"))
  }
  result
}










