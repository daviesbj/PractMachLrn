classPlot <- function( nPC = 6, thinFac = 0.1, pcObj = NULL, origFrame = NULL, whichClass = NULL ){
  nSamp <- nrow( origFrame )
  whichOnes <- runif( nSamp ) < thinFac & origFrame$classe == whichClass
  pcSmall <- data.frame( pcObj[whichOnes,1:nPC])
  classe <- factor( origFrame$classe[whichOnes])
  plot( pcSmall, col=classe )
}
