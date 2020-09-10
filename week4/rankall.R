# ERiX Xie - rankall.R
# 
# 1.takes two arguments: an outcome name (outcome) and a hospital rank- ing (num)
# 2.returns a 2-column data frame containing the hospital in each state that has 
# the ranking specified in num.

rankall <- function(outcome_name, num = "best") {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that outcome is valid
  all_outcome_names <- c("heart attack", "pneumonia", "heart failure") # all disease names
  
  all_states_names <- unique(outcome$State) # all states names, in 2 characters
  all_states_names <- all_states_names[order(all_states_names)]
  # # process the num for later use, and store it into variable "number"
  # number <- NULL
  # last_rank <- (dim(sorted_ranked_outcome)[1])
  # if (as.character(num) == "best"){
  #   number <- 1
  # }else if(as.character(num) == "worst"){
  #   number <- last_rank
  # } else{
  #   number <- as.integer(num)
  # }
  
  ## For each state, find the hospital of the given rank
  df_st_hos <- data.frame("hospital" = rep(NA,length(all_states_names)), "state" = all_states_names)# a dataframe storing states & the hospitals in those states ranked the "number" 
  for (i in c(1:length(all_states_names))) {
    st <- all_states_names[i] # st is the current state name
    hos_name <- rankhospital(st, outcome_name, num) # hos_name is the name of the hospital ranked the "number" 
    df_st_hos[i,1] <- hos_name # stores the hospital's name into the dataframe
  }
  return(df_st_hos)
  
  
}