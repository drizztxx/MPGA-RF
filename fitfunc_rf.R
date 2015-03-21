


fitfunc_rf <- function(object = Chrom,ntree = 500,data = data,vars = vars,CPUs=CPUs,...){
  ObjV <- numeric()
  if (is.vector(object)) nind <- 1 else nind <- NROW(object)
#   cat(sprintf('object name = %s \n',environmentName(environment())))
#   cat(sprintf('CPUs= %.0f \n',CPUs))
  results <- mclapply(1:nind,function(i){
    if (is.vector(object)) indices <- which(object == 1) else indices <- which(object[i,] == 1) 
    r_rf <- randomForest(as.matrix(data$x[, indices]), as.factor(data$y), ntree = ntree,importance = T)
    class <- length(levels(as.factor(data$y)))
    importances <- rep(NA,vars)
    importances[indices] <- importance(r_rf)[, 3]
    cf <- r_rf$confusion[,1:class]
    errorRate <- 1-sum(diag(cf))/sum(cf)
    return(list(evalVals = -errorRate, evalImportances = importances))
  },mc.cores = CPUs)
  evalVals <- sapply(1:nind,function(object)results[[object]]$evalVals)
  evalImportances <- lapply(1:nind,function(object)results[[object]]$evalImportances)
  evalImportances <- do.call(rbind,evalImportances)
  result <- list(ObjV = evalVals,evalImportances = evalImportances)
  return(result)
}



