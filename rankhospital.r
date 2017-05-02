rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ## check state
  if(!(state %in% outcomeData$State)){
    stop("invalid state")
  }
  ## check outcome
  switch(outcome,
         "heart failure" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
         "heart attack" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
         "pneumonia" = {outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
         stop("invalid outcome"))
  ## Return hospital name in that state with the given rank

  
  outcomeData <- split (outcomeData, outcomeData$State)
  data_state <- outcomeData[[state]]
  
  # ranking hospitals
  
  vvector <- as.numeric(data_state[[outcome]])
  index2 <- (!is.na(vvector))
  index<-order(vvector, data_state[["Hospital.Name"]])
  
  index2 <- index2[index]
  
  nvector <- data_state[["Hospital.Name"]][index]
  nvector <- nvector[index2]

  
  ##using num
  
  switch(num,
         "best" = {num = 1},
         "worst" = {num = length(nvector)})
  if (!is.numeric(num)){
    stop("invalid num")
  }
  
  nvector[num]
   
  
}
