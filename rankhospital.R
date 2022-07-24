rankhospital <- function(state, outcome, num = "best"){
        # This function gets the separated data based on a given state.
        data <- statedata(state, outcome)
        
        # This function gets the outcome column number. 
        colnum <- colnum(outcome)
        
        # select just 2 columns.
        # the first one is "Hospital.Name" and the other is outcome. 
        data <- data[,c(2, colnum)]
        
        # remove "Not Available" values in the outcome column.
        data[, 2] <- gsub("Not Available", "", as.character(data[, 2]))
        
        # change the class of outcome column from "character" to "numeric".
        data[, 2] <- as.numeric(data[, 2])
        
        # remove Na data
        data <- na.omit(data)
        
        # change the column name for better understanding
        colnames(data) <- c("Hospital Name", outcome)
        
        # sort the data based on outcome
        data <- data[order(data[, outcome]),]
        
        # set Num for "best" and "worst"
        if (num == "best"){num = 1}
        if (num == "worst"){num = nrow(data)}
        
        # showing the data
        data[num, ]
}