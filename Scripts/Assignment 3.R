
# 1 Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# 2 Finding the best hospital in a state
best<- function(st, ou)
{
  setwd("~/Documents/R Project/For Course/R-courstest/Data/AS3")
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check whether the outcome (ou) is valid
  ##【这一段记得换个思路去写一写试试：用类似于identical()函数的思路，判断cols的names 
  ##  与输入的character参数是否匹配（当然，其中可能需要paste函数统一格式），
  ##  然后用逻辑值进行选择判断】
  ## Update:上述想法的实行见对于State元素的Check（因为元素实在太多无法用switch()）
  op1<-switch(ou,"heart attack"=1,"heart failure"=2,"pneumonia"=3,0)
  if(!op1)
  {
    stop(print("Invalid outcome !"))
  }
  op11<-switch(op1,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
               "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
               "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",0)
  mid1<-outcome$State
  Mid1<-unique(mid1)
  op2<-any(st==Mid1)
  if(!op2)
  {
    stop(print("Invalid state !"))
  }
  data1<-outcome[which(outcome$State==st),]
  data2<-as.numeric(data1[, op11])
  k<-min(data2,na.rm = TRUE)
  result<-data1[which(as.numeric(data1[,op11])==k),"Hospital.Name"]
  result<-min(result)
  result
}