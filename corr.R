# Reads CSV Files from a directory, and for the files that meet a certain treshold of completed
# cases, the program calculates the correlation between nitrate and sulfate
corr <- function(directory, treshold = 0) {
    completed_obeservation <- 0
    vector_correlation <- c()
    for (file_name in list.files(directory)) {
        file_to_be_read <- paste(directory,file_name,sep="/")
        pollutant_data_frame <- read.csv(file_to_be_read)
        #print(file_name)
        completed_obeservation <- sum(!is.na(pollutant_data_frame[["sulfate"]]) & !is.na(pollutant_data_frame[["nitrate"]]))
        if (completed_obeservation >= treshold) {
            vector_sulfate <- subset.data.frame(pollutant_data_frame,!is.na(pollutant_data_frame[["sulfate"]])&!is.na(pollutant_data_frame[["nitrate"]]))[["sulfate"]]
            vector_nitrate <- subset.data.frame(pollutant_data_frame,!is.na(pollutant_data_frame[["sulfate"]])&!is.na(pollutant_data_frame[["nitrate"]]))[["nitrate"]]
            correlation <- cor(vector_sulfate, vector_nitrate)
            if(!is.na(correlation)) {
                vector_correlation <- c(vector_correlation, correlation)                
            }
        }
    }
    vector_correlation
}


