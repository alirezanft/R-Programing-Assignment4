colnum <- function(outcome){
        if (outcome == "heart attack") outcome = 11
        if (outcome == "heart failure") outcome = 17
        if (outcome == "pneumonia") outcome = 23
        return(outcome)
}
