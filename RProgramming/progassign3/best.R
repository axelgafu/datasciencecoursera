#' The current .R file contains a functions that identifies the best hospital
#' based on the statistics selected. See more details in:
#'     https://d396qusza40orc.cloudfront.net/rprog%2Fdoc%2FProgAssignment3.pdf
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

#' @name best
#' 
#' Best hospital based on the statistic specified.
#' 
#' @param state USA state of the hospital.
#'     If an invalid state is speficied "invalid state" will be returned.
#' @param outcome name of the statistic to be evaluated. Valid values:
#'     heart attack.
#'     heat failure.
#'     pneumonia.
#'     
#'     If an invalid outcome is speficied "invalid outcome" will be returned.
#' 
#' @return A character vector with the hospital name.
#' 
#' @examples
#' > best( "TX", "heart attack" )
#' [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#' > 
#' 
#' @export
best <- function( state, outcome ) 
{
    ## Read outcome data
    data <- read.csv( "outcome-of-care-measures.csv", colClasses="character" )
    outcome <- tolower( outcome )
    
    ## Check that state and outcome are valid
    if( nchar( state ) < 2 || state %in% data[,"State"] )
    {
        stateL <- data[, "State" ] == state
        data <- data[stateL,]
        
        if( outcome %in% names( best_outopt ) )
        {
            outcome <- toString( best_outopt[outcome][1] )

            data <- validData( data )
            data <- sortData( data, outcome )
            #print( head( data[,c("Hospital.Name", outcome) ] ) )
            c( data[1,"Hospital.Name"] )
        }
        else
        {
            stop( "invalid outcome" )
        }
    }
    else
    {
        stop( "invalid state" )
    }

    ## Return hospital name in that state with lowest 30-day death
    ## rate
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
    validDataL <- aux !="Not Available" & !is.na( aux )
    
    data <- data[ validDataL, ]
    
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