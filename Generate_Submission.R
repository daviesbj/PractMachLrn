library(caret); library(ggplot2); library(klaR); library(MASS)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

setwd( "/Users/daviesbj/PractMachLrn" )
load( "RandomForestClassifier.Rdata" )
testSetFull <- read.csv('pml-testing.csv', stringsAsFactors = FALSE )
testResults <- predict( modRF, testSetFull )
setwd( "/Users/daviesbj/PractMachLrn/Submission" )
pml_write_files( testResults )
setwd( "/Users/daviesbj/PractMachLrn" )

