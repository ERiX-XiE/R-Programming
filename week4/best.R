# ERiX Xie - best.R

## 11:16 - Heart Attack, death mortality rates
## 17:22 - Heart Failure, death mortality rates
## 23:28 - Pneumonia, death mortality rates
## 29:34 - Heart Attack, readmission rates
## 35:40 - Heart Failure, readmission rates
## 41:46 - Pneumonia, readmission rates

# The function reads the outcome-of-care-measures.csv file and 
# returns a character vector with the name of the hospital that 
# has the best (i.e. lowest) 30-day mortality for the specified 
# outcome in that state. 

best <- function(state_name, outcome_name) {
  ## Read outcome data

  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  all_states_names <- unique(outcome$State) # all states names, in 2 characters
  all_outcome_names <- c("heart attack", "pneumonia", "heart failure") # all disease names
  
  
  
  ## Check that state and outcome are valid
  if (!state_name %in% all_states_names){
    stop("invalid state")
  }
  if (!outcome_name %in% all_outcome_names){
    stop("invalid outcome")
  }
  
  
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

  
  # sort by death rate
  good_outcome_by_death_rate <- good_outcome[order(good_outcome[2]),] 
  # then, rank those hospitals with the same mortality rateby names alphabetically
  lowest_death_rate <- good_outcome_by_death_rate[[1,2]] # 1st row, 2nd col, the lowest death rate
  best_hospital_names <- subset(good_outcome_by_death_rate,good_outcome_by_death_rate[2] == lowest_death_rate,select = 1)
  sorted_best_hospital_names <- sort(best_hospital_names)
  
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  print(append("lowest_death_rate: ", lowest_death_rate)) # return the lowest death rate
  return(best_hospital_names) # return the hospital name
  
}

