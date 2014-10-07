best<- function(state,outcome){
  options(warn=-1)
  source("utility.R")
  get_hospital_name <- function(data){
    min_value <- min(data[,3])
    hos_list = NULL
    for(i in 1:nrow(data)){
      if(data[[i,3]] == min_value)
        hos_list<-rbind(hos_list,data[[i,1]])
    }
    sort(as.vector(t(hos_list)))
  }
  
  data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
  data_needed <- data[,c(2,7,11,17,23)]
  data_filtered<-parse_data(data_needed,state,outcome)
  if(data_filtered == 0)
    stop("invalid state")
  else if (data_filtered == 1)
    stop("invalid outcome")
  print(get_hospital_name(data_filtered)[1])
}