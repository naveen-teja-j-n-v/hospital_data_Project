rankall <- function(outcome, num = "best") {
  source("utility.R")
  data <- read.csv("outcome-of-care-measures.csv",colClasses ="character")
  data_needed <- data[,c(2,7,11,17,23)]
  state_names <- sort(unique(data_needed$State))
  hos_list = vector()
  state_list = vector()
  
  ind <- 1
  for(state in state_names){
    data_filtered<-parse_data(data_needed,state,outcome)    
    # Error checking
    num_t <- num
    if(data_filtered == 0)
      stop("invalid state")
    else if (data_filtered == 1)
      stop("invalid outcome")
    
    #error Checking
    if(class(num_t)!= "numeric"){
      if(tolower(num_t)=="worst")
        num_t = nrow(data_filtered)
      else if(tolower(num_t) == "best")
        num_t =1
      else
        stop("invalid num")
    }
    
    #ordering the Data
    data_filtered <- data_filtered[order(data_filtered[,3],data_filtered[,1]),]
    hos_list[ind] = as.vector(data_filtered[num_t,1])
    state_list[ind] = as.vector(state)
    ind <- ind + 1
  }
  data.frame(row.names = state_list,hospital=hos_list,state = state_list)
}