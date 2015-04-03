best <- function(state, outcome) {
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        logicalState <- outcomeData[,7] == state
        
        if(length(outcomeData[logicalState, 7]) < 1 ) {
                stop("invalid state")
        }
        
        translatedOutcome <- NULL
        if(outcome == "heart attack") {
                translatedOutcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
                outcomeData[,11] <- as.numeric(outcomeData[,11])
        } else if (outcome == "heart failure") {
                translatedOutcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                outcomeData[,17] <- as.numeric(outcomeData[,17])
        } else if (outcome == "pneumonia") {
                translatedOutcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                outcomeData[,23] <- as.numeric(outcomeData[,23])
        } else {
                stop("invalid outcome")
        }
        
        
        outcomesForState <- outcomeData[logicalState, translatedOutcome]
        hospitalsForState <- outcomeData[logicalState, 2]
        hospitalsAndOutcomes <- data.frame(hospitalsForState, outcomesForState)
        
        logicalOutcomes <- !is.na(hospitalsAndOutcomes[,2])
        hospitalsAndOutcomes <- hospitalsAndOutcomes[logicalOutcomes,]
        hospitalsAndOutcomes <- hospitalsAndOutcomes[ order(hospitalsAndOutcomes$hospitalsForState), ]
        
        minRow <- which.min(hospitalsAndOutcomes[,2])
        as.character(hospitalsAndOutcomes[minRow,1])
}
