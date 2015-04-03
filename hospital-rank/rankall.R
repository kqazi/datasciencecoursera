rankall <- function(outcome, num = "best") {
        outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
                
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
        
        states <- sort(unique(outcomeData[,7]))
        rankToUse <- num
        bestHospitalsByStateRank <- data.frame(hospital = character(54), state = states, stringsAsFactors=FALSE)
        for(i in 1:length(states)) {
                logicalState <- outcomeData[,7] == states[i]
                
                outcomesForState <- outcomeData[logicalState, translatedOutcome]
                hospitalsForState <- outcomeData[logicalState, 2]
                hospitalsAndOutcomes <- data.frame(hospitalsForState, outcomesForState)
                
                logicalOutcomes <- !is.na(hospitalsAndOutcomes[,2])
                hospitalsAndOutcomes <- hospitalsAndOutcomes[logicalOutcomes,]
                hospitalsAndOutcomes <- hospitalsAndOutcomes[ order(hospitalsAndOutcomes$outcomesForState, hospitalsAndOutcomes$hospitalsForState), ]
                rankings <- nrow(hospitalsAndOutcomes)
                hospitalsAndOutcomes$rank <- 1:rankings
                
                if(num == "best") {
                        rankToUse <- 1
                } else if (num == "worst") {
                        rankToUse <- rankings
                }
                
                if(rankToUse > rankings) {
                        bestHospitalsByStateRank[i,1] = as.character("NA")
                } else {
                        logicalRanking <- hospitalsAndOutcomes$rank == rankToUse  
                        bestHospitalsByStateRank[i,1] = as.character(hospitalsAndOutcomes[logicalRanking, 1])
                }
        
        }
        bestHospitalsByStateRank
}