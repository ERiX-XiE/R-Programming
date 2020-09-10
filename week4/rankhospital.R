# ERiX Xie - rankhospital.R

# 1.return a character vector containing the name of the hospital with the 5th 
# lowest 30-day death rate for heart failure. 
# 2.The num argument can take values “best”, “worst”, or an integer indicating 
# the ranking (smaller numbers are better). 
# 3.If the number given by num is larger than the number of hospitals in that 
# state, then the function should return NA.
# 4.Hospitals that do not have data on a particular outcome should be excluded 
# from the set of hospitals when deciding the rankings.

rankhospital <- function(state_name, outcome_name, num = "best") {
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
  all_states_names <- unique(outcome$State) # all states names, in 2 characters
  all_outcome_names <- c("heart attack", "pneumonia", "heart failure") # all disease names
  
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  
  # good_state_outcome is the set of data about the state we're looking at
  good_state_outcome <- outcome[outcome$State == state_name,]
  
  # indexes_outcome_cols is the indexes of the columns of death rates of the 
  # specified outcome/disease AND the column index of hospital.Name, which is 2
  indexes_outcome_cols <-  append(2, switch(outcome_name, 
                                            "heart attack" = 11, 
                                            "pneumonia" = 23, "heart failure"
                                            = 17))
  
  # good_outcome only include hospitals & its mortality rate for the specified disease
  good_outcome <- subset(good_state_outcome, select = indexes_outcome_cols)
  good_outcome[[2]] <- as.numeric(as.character(good_outcome[[2]])) # !! to change a column to numeric
  good_outcome <- good_outcome[complete.cases(good_outcome),] # to remove na's
  
  # !!! use df[order(df[1stCol],df[2ndCol]),] to sort by multiple criteria
  # sort and add a ranking column
  sorted_ranked_outcome <- cbind(good_outcome[order(good_outcome[2],good_outcome[1]),],
                           rank = c(1:dim(good_outcome)[1]))
  
  # process the num
  last_rank <- (dim(sorted_ranked_outcome)[1])
  if (as.character(num) == "best"){
    number <- 1
  }else if(as.character(num) == "worst"){
    number <- last_rank
  } else{
    number <- as.integer(num)
  }
  
  # a <<- sorted_ranked_outcome # for double checking at console
  
  if (number > last_rank){ # return NA if num is larger than the number of hospitals in that state
    return(NA)
  }
  return(sorted_ranked_outcome[number,])
}
