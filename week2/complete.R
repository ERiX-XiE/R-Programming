complete <- function(directory, id = 1:332) {
  the_ids = NULL # list of id to be returned
  the_nobs = NULL # list of nobs to be returned
  
  for (i in id) {
    if (i<10){
      index <- paste("00",i,sep ="")
    } else if (i<100){
      index <- paste("0",i,sep ="")
    } else{
      index <- i
    }
    this_csv_dir = paste(directory,"/",index,".csv",sep = "")
    this_data <- read.csv(file = this_csv_dir)
    count_nob <- length(which(!is.na(this_data[[3]]) & !is.na(this_data[[2]]) ))
    
    # perpare to be returned
    the_ids <- c(the_ids,i) 
    the_nobs <- c(the_nobs,count_nob)
    
    # df <- rbind(df,c(as.integer(index),as.integer(count_nob)))
  }
  result_data <- data.frame(the_ids,the_nobs)
  names(result_data) <- c("id","nobs")
  result_data
}