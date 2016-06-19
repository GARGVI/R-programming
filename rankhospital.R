setwd("C:\\Users\\Gemma\\Documents\\Practica4")

rankhospital <- function(state, outcome, num = "best") { 
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
        
        ## Check that state and outcome are valid
        validOutCome <- c("heart attack", "heart failure", "pneumonia")
        #rename the columns 30-day death 
        names(df)[c(11,17,23)] <- validOutCome
        
        
        ## Check that state and outcome are valid
        if (!(state %in% as.factor(df$State))) {
                stop("State not found")
        }
        else if (!(outcome %in% validOutCome)) {
                stop("invalid outcome ")
        }
        
        
        ## Return hospital name in that state with the given rank ## 30-day death rate
        dfState<-df[(df$State==state & !is.na(as.numeric(df[,outcome]))),]
        resultdf<-dfState[order(as.numeric(dfState[,outcome]),dfState[,"Hospital.Name"]),"Hospital.Name"]
        
        if (num == "worst") tail(resultdf,1)
        else if (num =="best") resultdf[1]
        else resultdf[num]
        
        
        
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
