
SplitData <- function(data2split,subpop){
  n.row <- NROW(data2split)
  data.subpop <- list()
  for (i in 1:subpop){
    interv <- n.row/subpop
    sequence <- seq(0,n.row,interv)
    data.subpop[[i]] <- data2split[(sequence[i]+1):sequence[i+1],]
  }
  return(data.subpop)
}
# newObjV <- SplitData(t(ObjV.matrix),3)



  
