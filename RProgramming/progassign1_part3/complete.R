

## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) 
{
    if( file.exists( directory ) )
    {
        result <- data.frame( id=numeric( length(id) ), nobs=numeric( length(id) ) )
        workdir <- getwd()
        setwd( directory )
        i <- 0
        
        for( fid in id )
        {
            i <- i+1
            fileName <- sprintf( "%03d.csv", fid )
            data <- read.csv( file=fileName, header=TRUE ) # Read cvs
            
            # Count complete observations for sulfate
            pollDataSulfl <- !is.na( data[,"sulfate"] ) 
            # Count complete observations for nitrate
            pollDataNitrl <- !is.na( data[,"nitrate"] )
            
            # Keep only complete observations
            pollData <- data[ pollDataSulfl&pollDataNitrl, ] 
            
            result$id[i] <- fid
            result$nobs[i] <- nrow( pollData )
        }
        setwd( workdir )
        result
    }
    else
    {
        cat( "File not found: ", directory )
    }
}