rankall <- function( outcome, rank="best")
{
        #rankall that takes two arguments: an outcome name (outcome) and a hospital 
        #rank- ing (num). The function reads the outcome-of-care-measures.csv file and 
        #returns a 2-column data frame containing the hospital in each state that has the 
        #ranking specified in num. For example the function call rankall("heart attack", "best") 
        #would return a data frame containing the names of the hospitals that are the best in
        #their respective states for 30-day heart attack death rates. The function should 
        #return a value for every state (some may be NA). The first column in the data frame 
        #is named hospital, which contains the hospital name, and the second column is named 
        #state, which contains the 2-character abbreviation for the state name. Hospitals that 
        #do not have data on a particular outcome should be excluded from the set of hospitals
        #when deciding the rankings.
        
    
        if (outcome=="pneumonia") metric <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        else if (outcome=="heart attack") metric <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        else if (outcome=="heart failure") metric <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        else stop('invalid outcome')
        
        #read data
        data <- read.csv("outcome-of-care-measures.csv", na.strings=c("Not Available"), colClasses=c("character"))
        #convert particular field to numeric to order by it
        data[,metric] <- as.numeric(data[,metric])
        
        data = data[!is.na(data[,metric]),]      
        data = data[order(data[,metric],data[,"Hospital.Name"], na.last = TRUE),]
        
        states <- unique(data$State)
        
        rankhospital <- function(state)
        {
                
                #select state data
                state_data = data[data$State==state,]
                
                if (rank=="best") state_data[1,"Hospital.Name"]
                else if (rank=="worst") state_data[nrow(state_data),"Hospital.Name"]
                else 
                {
                        rank <- as.numeric(rank)
                        if (rank > nrow(state_data)) NA
                        else state_data[rank,"Hospital.Name"]
                }
        }
        
        hospitals <- lapply(states, rankhospital)
        hosp_ranks <- data.frame(unlist(hospitals), states, row.names = states)
        colnames(hosp_ranks) <- c("hospital","state")
        hosp_ranks <- hosp_ranks[order(hosp_ranks[,"state"]),]
        hosp_ranks
}


