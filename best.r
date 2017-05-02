best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ## check state
  if(!(state %in% outcomeData$State)){
    stop("invalid state")
  }
  ## check outcome
  if(!(outcome %in% c("heart failure","heart attack","pneumonia"))){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  switch(outcome,
         "heart failure" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
         "heart attack" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
         "pneumonia" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
         stop("invalid outcome"))
  
  outcomeData <- split (outcomeData, outcomeData$State)
  data_state <- outcomeData[[state]]
  
  vvector <- as.numeric(data_state[[outcome]])
  index<-which(vvector==min(vvector,na.rm=TRUE))
  
  ##rate 
  vhosp <-sort(data_state[["Hospital.Name"]][index])
  vhosp[1]
  
}
