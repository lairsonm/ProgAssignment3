rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  switch(outcome,
         "heart failure" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
         "heart attack" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
         "pneumonia" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
         stop("invalid outcome"))
  ## For each state, find the hospital of the given rank
  ## get all states names
  lvlStateFactor <- levels(factor(outcomeData$State))
  ## divide the states
  outcomeData <- split (outcomeData, outcomeData$State)
  
  h<-character(0)
  for(i in lvlStateFactor){
    new_data<-outcomeData[[i]]
    vvector<-as.numeric(new_data[[outcome]])
    index<-order(vvector,new_data[["Hospital.Name"]])
    index2<-(!is.na(vvector))
    index2<-index2[index]
    name<-new_data[["Hospital.Name"]][index]
    name<-name[index2]
    if(num=="best"&& length(name)>0){
      h<-append(h,name[1])
    }
    else if (num=="worst" && length(name)>0){
      h<-append(h,name[[length(name)]])
    }
    else if (num<=length(name)){
      h<-append(h,name[num])
    }
    else {
      h<-append(h,"NA")
    }
  }
  
  return(data.frame(hospital=h,state=lvlStateFactor,row.names=lvlStateFactor))
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
