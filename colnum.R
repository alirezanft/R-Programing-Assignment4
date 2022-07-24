colnum <- function(outcome){
        if (outcome == "heart attack") colnum = 11
        if (outcome == "heart failure") colnum = 17
        if (outcome == "pneumonia") colnum = 23
        return(colnum)
}
