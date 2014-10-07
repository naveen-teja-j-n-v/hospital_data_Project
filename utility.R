get_filtered_data <- function(data_needed,index,state){
  data_filtered <- split(data_needed,data_needed$State)[[state]]
  data_filtered <- data_filtered[,c(1,2,index)]
  data_filtered[,3] <- as.numeric(data_filtered[,3])    
  data_filtered <- data_filtered[complete.cases(data_filtered),]    
}

parse_data <- function(data_needed,state,outcome){
  state_names<-unique(data_needed$State)
  if(!(state %in% state_names))
    return(0)
  
  if(tolower(outcome)=="heart attack"){
    data_filtered <- get_filtered_data(data_needed,3,state)    
  } else if(tolower(outcome)=="heart failure"){
    data_filtered <- get_filtered_data(data_needed,4,state)
  } else if(tolower(outcome)=="pneumonia") {
    data_filtered <- get_filtered_data(data_needed,5,state)
  }else{
    return(1)
  }
  data_filtered  
}