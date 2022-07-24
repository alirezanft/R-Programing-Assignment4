statedata <- function(state, outcome){
        
        if((outcome %in% c("heart attack", "heart failure", "pneumonia")) == FALSE){
                print("invalid outcome")
        }
        # complete csv file to search
        outcomedata <- read.csv("outcome-of-care-measures.csv")
        
        if(!any(state == outcomedata$State)){
                print("invalid state")
        } else {
        # split the csv file based on States
        splitfile <- split(outcomedata, outcomedata$State)
        
        # select the State given by user
        data <- splitfile[[state]]
        }
        data
}
