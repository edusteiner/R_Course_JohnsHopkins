# Reads CSV Files from the interval and counts how many observations are completed in each file
complete <- function(directory, id=1:332) {
    completed_obeservation <- 0
    completed_obeservation_df <- data.frame(id = numeric(), nobs = numeric())
    for (i in id) {
        if (i < 10) {
            file_name<-paste("00",i,".csv",sep="")
        } else if (i >=10 && i < 100) {
            file_name<-paste("0",i,".csv",sep="")
        } else {
            file_name<-paste(i,".csv",sep="")
        }
        file_to_be_read <- paste(directory,file_name,sep="/")
        pollutant_data_frame <- read.csv(file_to_be_read)
        completed_obeservation <- sum(!is.na(pollutant_data_frame[["sulfate"]]) & !is.na(pollutant_data_frame[["nitrate"]]))
        this_file <- data.frame(id = i, nobs = completed_obeservation)
        completed_obeservation_df <- rbind(completed_obeservation_df, this_file)
    }
    completed_obeservation_df
}


