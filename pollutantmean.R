pollutantmean <- function(directory, pollutant, id = 1:332) {
  all_data = NULL
#  data <- read.csv(file = directory)
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
    all_data <- rbind(this_data,all_data)
  }
  mean(all_data[[pollutant]], na.rm = TRUE)
}
