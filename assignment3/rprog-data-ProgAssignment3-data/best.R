best <- function(state, outcome)
{
        # best takes two arguments: the 2-character abbreviated name 
        #of a state and an outcome name. The function reads the outcome-of-care-measures.csv 
        #file and returns a character vector with the name of the hospital that has the best
        #(i.e. lowest) 30-day mortality for the specified outcome in that state. The hospital 
        #name is the name provided in the Hospital.Name variable. The outcomes can be one of
        #“heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on 
        #a particular outcome should be excluded from the set of hospitals when deciding the 
        #rankings.
        
        if (outcome=="pneumonia") metric <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        else if (outcome=="heart attack") metric <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        else if (outcome=="heart failure") metric <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        else stop('invalid outcome')
        
        
        #read data
        data <- read.csv("outcome-of-care-measures.csv", na.strings=c("Not Available"), colClasses=c("character"))
        #convert particular field to numeric to order by it
        data[,metric] <- as.numeric(data[,metric])
        
        #check if state is correct
        if (!(state %in% data$State)) stop('invalid state')
        
        #select state data
        data = data[data$State==state,]
        
        #remove hospitals without info about desired outcome  
        data = data[!is.na(data[,metric]),]
        
        data = data[order(data[,metric],data[,"Hospital.Name"], na.last = TRUE),]
        
        data[1, "Hospital.Name"]
}