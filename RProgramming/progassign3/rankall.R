#' The current .R file contains a functions that identifies a hospital ranking
#' based on the statistics selected. See more details in:
#'     https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2FProgAssignment3.pdf
#' The rankings are calculated for all the states registered in the input data.
#'     
#' Requires "outcome-of.care.measures.csv" is in the same directory where the
#' R script is called from.
#' 
#' @author Axel Garcia.
# Setup the list of opctions for the outcome.
best_outopt <- vector( mode="list", length=3 )
names( best_outopt ) <- c( "heart attack", "heart failure", "pneumonia" )
best_outopt["heart attack"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" #(11)
best_outopt["heart failure"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"#(17)
best_outopt["pneumonia"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" #(23)


#' @name rankall
#' 
#' Hospital ranking based on the statistic specified.
#' 
#' @param outcome name of the statistic to be evaluated. Valid values:
#'     heart attack.
#'     heat failure.
#'     pneumonia.
#'     
#'     If an invalid outcome is speficied "invalid outcome" will be returned.
#' @param num kind of ranking to be performed. Valid values:
#'     best: Best hospital; i.e. lowest death rate.
#'     worst: Worst hospital; i.e. highest death rate.
#'     <number>: Position in the ranking from 1 to n. Where 1 is the best and
#'         n is the worst.
#' 
#' @return A data.frame with the hospital names and a 2-characters state code.
#' 
#' @examples
#' > head( rankall( "heart attack", 1 ) )
#' hospital state
#' 1             CRESTWOOD MEDICAL CENTER    AL
#' 2     PROVIDENCE ALASKA MEDICAL CENTER    AK
#' 3                 MAYO CLINIC HOSPITAL    AZ
#' 4              ARKANSAS HEART HOSPITAL    AR
#' 5    GLENDALE ADVENTIST MEDICAL CENTER    CA
#' 6 ST MARYS HOSPITAL AND MEDICAL CENTER    CO
#' > 
#' 
#' @export
rankall <- function( outcome, num = "best" ) 
{
    ## Read outcome data
    data <- read.csv( "outcome-of-care-measures.csv", colClasses="character" )
    outcome <- tolower( outcome )
    
    ## Check that state and outcome are valid
    if( outcome %in% names( best_outopt ) )
    {
        uniqueSt <- unique( data[,"State"] )
        result <- data.frame( 
            hospital=character( length(uniqueSt) ), 
            state=character( length(uniqueSt) ),
            stringsAsFactors=FALSE)
        outcome <- toString( best_outopt[outcome][1] )
        i <- 0
        
        data <- validData( data, outcome )
        for( st in uniqueSt )
        {
            i <- i+1
            stateL <- data[, "State"] == st
            dataSt <- sortData( data[stateL, ], outcome )
            
            result[i,] <- c( "state", st )
            if( num == "best" )
            {
                result[i, "hospital"] <- toString( dataSt[1,"Hospital.Name"] )
            }
            else if( num == "worst" )
            {
                result$hospital[i] <- dataSt[nrow(dataSt),"Hospital.Name"]
            }
            else if( nrow(dataSt) >= num )
            {
                result$hospital[i] <- dataSt[num,"Hospital.Name"]
            }
            else
            {
                result$hospital[i] <- NA
            }
        }
        result
    }
    else
    {
        stop( "invalid outcome" )
    }
}


#' @name validData
#' 
#' Helper function.
#' 
#' @param data data frame to be validated.
#' 
#' @return Data frame with valid data.
#' 
#' @examples
#' TBA
#' 
#' @export
validData <- function( data, outcome )
{
    aux <- data[, outcome ]
    heartAttkL <- aux !="Not Available" & !is.na( aux )
    
    data <- data[ heartAttkL, ]
    
    data
}



#' @name sortData
#' 
#' Helper function.
#' 
#' @param data data frame to be sorted.
#' @param outcome statistic to be used as first ordering criteria.
#' 
#' @return Data frame with sorted data.
#' 
#' @examples
#' TBA
#' 
#' @export
sortData <- function( data, outcome )
{
    data[,outcome] <- as.numeric( data[,outcome] )
    data <- data[ order( data[, "Hospital.Name"] ), ]
    data <- data[ order( data[, outcome] ), ]

    data
}