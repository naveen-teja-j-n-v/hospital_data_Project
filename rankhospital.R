rankhospital <- function(state, outcome, num = "best"){
  source("utility.R")
  options(warn=-1)
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  data_needed <- data[,c(2,7,11,17,23)]
  data_filtered<-parse_data(data_needed,state,outcome)
  
  # Error checking
  if(data_filtered == 0)
    stop("invalid state")
  else if (data_filtered == 1)
    stop("invalid outcome")
  
  #error Checking
  if(class(num)!= "numeric"){
    if(tolower(num) == "best")
        num =1
    else if(tolower(num) == "worst")
        num=nrow(data_filtered)
    else
      stop("invalid num")
  }
  
  #ordering the Data
  data_filtered <- data_filtered[order(data_filtered[,3],data_filtered[,1]),]
  print(data_filtered[num,1])
}