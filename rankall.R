setwd("C:\\Users\\Gemma\\Documents\\Practica4")

df <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 

rankall <- function(outcome, num = "best") { 
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
        
        #remove NA
        df<-df[!is.na(as.numeric(df[,outcome])),]
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the ## (abbreviated) state name
        lState <-split(df, as.factor(df$State))
        result <-lapply(lState, function(l) l[order(as.numeric(l[,outcome]),l[,"Hospital.Name"]),"Hospital.Name"])
        
        if (num == "worst") ret <- lapply(result, function(l) tail(l,1))
        else if (num =="best") ret <- lapply(result, function(l) l[1])
        else ret <- lapply(result, function(l) l[num])
        
        data.frame(hospital=unlist(ret), state=names(ret))
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
