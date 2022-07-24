best <- function(state, outcome){
        # This function gets the separated data based on a given state.
        data <- statedata(state, outcome)

        # This function gets the outcome column number. 
        outcome <- colnum(outcome)
        
        # select just 2 columns.
        # the first one is "Hospital.Name" and the other is outcome. 
        data <- data[,c(2, outcome)]
        
        # remove "Not Available" values in the outcome culumn.
        data[, 2] <- gsub("Not Available", "", as.character(data[, 2]))
        
        # change the class of outcome column from "character" to "numeric".
        data[, 2] <- as.numeric(data[, 2])
        
        # subset based on min value and select the "Hospital.Name" which is "1".
        ans <- subset(data, data[, 2] == min(data[, 2], na.rm = T), select = c(1))
        
        ans[1, 1]
}