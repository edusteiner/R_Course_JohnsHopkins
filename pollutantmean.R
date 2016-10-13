# Reads CSV Files from the interval and calculates the mean of the indicated pollutant discarding NA records
pollutantmean <- function(directory, pollutant, id=1:332) {
    sum_of_all_elements_ignoring_na <- 0
    count_of_all_elements_ignoring_na <- 0
    for (i in id) {
        if (i < 10) {
            file_name<-paste("00",i,".csv",sep="")
        } else if (i >=10 && i < 100) {
            file_name<-paste("0",i,".csv",sep="")
        } else {
            file_name<-paste(i,".csv",sep="")
        }
        file_to_be_read <- paste(directory,file_name,sep="/")
        pollutant_data_frame<-read.csv(file_to_be_read)
        sum_of_all_elements_ignoring_na <- sum_of_all_elements_ignoring_na + sum(pollutant_data_frame[[pollutant]],na.rm=TRUE)
        count_of_all_elements_ignoring_na <- count_of_all_elements_ignoring_na + sum(!is.na(pollutant_data_frame[[pollutant]]))
    }
    mean_of_all_elements_ignoring_na <- sum_of_all_elements_ignoring_na / count_of_all_elements_ignoring_na
    mean_of_all_elements_ignoring_na
}


