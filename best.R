best <- function(state, outcome) {
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
        
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
        
        ## Return hospital name in that state with lowest 30-day death ## rate
        lowestDeath <- min(as.numeric(df[,outcome][df$State==state]), na.rm=T)
        resultdf<-subset(df, as.numeric(df[,outcome])==lowestDeath)[,"Hospital.Name"]
        resultdf[order(resultdf)][1]
        
        
        
}



best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack") 
best("NY", "hert attack") 
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
