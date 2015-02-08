## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".

## 'id' is an integer vector indicating the monitor ID numbers
## to be used

## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
#
# Example:
#      pollutantmean("specdata", "sulfate", 1:10)
#
pollutantmean <- function( directory, pollutant, id =1:332 )
{
  meanData <- c()    # Data to be used for the calculation
  workdir <- getwd()
  
  setwd( directory )
  
  # Create the files list to be processed.
  fileNames <- sprintf( "%03d.csv", id )
  for( filename in fileNames )
  {
    #print( filename )
    #print( names(data) )
    data <- read.csv( file=filename, header=TRUE )
    pollData <- data[pollutant]     # Read cvs
    pollDatal <- !is.na( pollData ) # Remove the NAs
    pollData <- pollData[pollDatal] # Keep only numbers
    
    meanData <- c( meanData, pollData ) # Append join data.
  }

  setwd( workdir )
  c( mean( meanData ) ) # Generate result
}