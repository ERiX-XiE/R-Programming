corr <- function (directory, threshold = 0) {
  complete_data <- complete(directory)
  good_ids <- NULL
  for (i in (1: length(complete_data[[1]]))) { # get all the good ids
    if (complete_data[i,2] > threshold) {
      good_ids <- c(good_ids,complete_data[i,1])
    }
  }
  list_cor <- NULL
  for (i in good_ids) { # compute all cor's
    if (i<10){ # get the index in the right format
      index <- paste("00",i,sep ="")
    } else if (i<100){
      index <- paste("0",i,sep ="")
    } else{
      index <- i
    } 
    this_csv_dir = paste(directory,"/",index,".csv",sep = "")
    this_data <- read.csv(file = this_csv_dir)
    this_cor <- cor(this_data$sulfate,this_data$nitrate,use = "complete.obs")
    list_cor <- c(list_cor,this_cor) # add this correlation to the list
  }
  list_cor
  
  

}