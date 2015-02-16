## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
corr <- function( directory, threshold = 0)
{
    if( file.exists( directory ) )
    {
        workdir <- getwd()
        
        # compute data
        completeObs <- complete( directory )
        filteredObs <- completeObs$id[completeObs$nobs>threshold]
        nobs <- length( filteredObs )
        setwd( directory )
        result <- numeric()
        
        for( fid in filteredObs )
        {
            fileName <- sprintf( "%03d.csv", fid )
            data <- read.csv( file=fileName, header=TRUE ) # Read cvs
            data <- data[!is.na(data[,"sulfate"]) & !is.na( data[,"nitrate"]), ]
            corData <- cor( data$sulfate, data$nitrate )
            result <- c( result, round( corData, 5 ) )
            
        }
        setwd( workdir )
        
        result
    }
    else
    {
        cat( "File not found: ", directory )
    }
}
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000