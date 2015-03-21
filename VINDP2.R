VINDP2 <- function(
  DatasetX = NULL,
  DatasetY = NULL,
  seriesValues = NULL,
  DistributionPop = 2000,  					#1.1????????????1.0????????????1/3????????????????
  QuantileValue = c(0.75, 0.80, 0.85, 0.90, 0.95, 0.99),
  Graphic = TRUE,CPUs = 1)
{
  library(parallel)
  if (is.null(DatasetX) | is.null(DatasetY) ){
    stop("Error: The dataset must be defined by assigning the arguments DatasetX= and DatasetY= . \n")
  }
  varNums <- ncol(DatasetX)
  DatasetY <- as.factor(DatasetY)
  
  if (is.null(seriesValues)){
    #Get the series
    serieslength <- 1
    #1.1????????????1.0????????????2/3????????????????
    while (2**serieslength < 2*ceiling(sqrt(varNums))){
      serieslength = serieslength + 1
    }
    seriesValues <- rep(NA, serieslength) 
    for ( serieslengthnow in 1:serieslength){
      ifelse ( serieslengthnow < serieslength,
               seriesValues[serieslengthnow] <- 2**serieslengthnow,
               #1.1????????????1.0????????????3/3????????????????
               seriesValues[serieslengthnow] <- 2*ceiling(sqrt(varNums)) )					}
  }else {
    if (seriesValues < 1) {
      stop("Error: The seriesValues must be a integer greater than 1. \n")
    }
    serieslength <- length(seriesValues)
  }
  cat(sprintf("serieslength = %f CPUs = %f",serieslength,CPUs))
  VINDPmatrix = mclapply(1:serieslength,function(serieslengthnow){
    PermutationTimes <- ceiling ( DistributionPop / seriesValues[serieslengthnow] )
    VINDProw = unlist(lapply(1:PermutationTimes,function(PermutationTime){
      DatasetXSample <- DatasetX[ , ( sample(varNums, seriesValues[serieslengthnow]) )]  
      DatasetXSample = sapply(1:ncol(DatasetXSample),
                              function(varNumSample) sample(DatasetXSample[,varNumSample]))
      RFSample <- randomForest( DatasetXSample, DatasetY, importance=TRUE, ntree=500)
      return(importance( RFSample )[ , 3])
    }))
    return(VINDProw[1:DistributionPop])
  },mc.cores = ifelse(CPUs > serieslength,serieslength,CPUs))
  
  VINDPmatrix <- do.call(rbind,VINDPmatrix)
  
  VIBounds <- apply(VINDPmatrix, 1, quantile, QuantileValue)
  
  if (Graphic){
    if (serieslength < 3){
      cat("Note: There are less than 3 points, the graphic will be ignored. \n")
    } else {
      GraphicRows <- floor( sqrt( nrow(VIBounds) ) )
      GraphicCols <- ceiling( sqrt( nrow(VIBounds) ) )
      opar <- par(mfrow = c(GraphicRows, GraphicCols) )
      
      for (NumGraphic in  1:nrow(VIBounds) ) {
        plot(seriesValues, VIBounds[NumGraphic, ],
             main = paste("The quantile is", QuantileValue[NumGraphic]) )
      }
      par(opar)
    }
    
  }
  results <- list(VINDPmatrix=VINDPmatrix, seriesValues=seriesValues, VIBounds=VIBounds )
  class(results) <- "VINDP"
  return(results)
}

