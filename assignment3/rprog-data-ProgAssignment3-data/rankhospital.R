rankhospital <- function(state, outcome, rank)
{
        #rankhospital that takes three arguments: the 2-character abbreviated name of 
        #a state (state), an outcome (outcome), and the ranking of a hospital in that 
        #state for that outcome (num). The function reads the outcome-of-care-measures.csv 
        #file and returns a character vector with the name of the hospital that has the 
        #ranking specified by the num argument. For example, the call
        #       rankhospital("MD", "heart failure", 5)
        #would return a character vector containing the name of the hospital with the 5th
        #lowest 30-day death rate for heart failure. The num argument can take values 
        #“best”, “worst”, or an integer indicating the ranking (smaller numbers are better).
        #If the number given by num is larger than the number of hospitals in that state, 
        #then the function should return NA. Hospitals that do not have data on a particular
        #outcome should be excluded from the set of hospitals when deciding the rankings.
        
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
        
        if (rank=="best") data[1,"Hospital.Name"]
        else if (rank=="worst") data[nrow(data),"Hospital.Name"]
        else 
        {
                rank <- as.numeric(rank)
                if (rank > nrow(data)) NA
                else data[rank,"Hospital.Name"]
        }
        
}