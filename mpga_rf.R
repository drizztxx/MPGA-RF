

mpga_rf  <- function(data = data,
                     popSize = 300,
                     MAXGEN = 100,
                     VIND = c(1,2000),
                     aslimitNoise = NULL,
                     initialPop = NULL,
                     limits = 2,
                     subpop = 3,
                     CPUs = 1,
                     ...){
  VINDopt1 <- VIND[1]
  VINDopt2 <- VIND[2]
  X <- data$x
  Y <- data$y
  vars <- NCOL(X)
  prop <- vars/5 - 1
  if (is.null(initialPop))  Chrom <- crtbp2(popSize,vars,prop) else Chrom <- initialPop
  
  if (VINDopt1 == 1){
    if (is.null(aslimitNoise)){
      cat("doing the permutaion, it may last for several minutes \n")
      runtime.VINDP <- system.time(resultVINDP <- VINDP2(DatasetX = X, 
                                                         DatasetY = Y, seriesValues = NULL, DistributionPop = VINDopt2, 
                                                         QuantileValue = c(0.5, 0.75, 0.8, 0.85, 0.9, 0.95), 
                                                         Graphic = FALSE,CPUs = CPUs))
      
      cat(paste("VINDP took ",runtime.VINDP[3],"seconds. \n"))
#       cat(sprintf('object name = %s \n',environmentName(environment())))

      limitNoise <- rbind(resultVINDP$seriesValues, resultVINDP$VIBounds[limits, ])
      #write.table(limitNoise,file = "limitNoise.txt")
      } else limitNoise <- aslimitNoise
  
      cat("noise reduction \n")
#       cat(sprintf('CPUs= %.0f \n',CPUs))
      rfresult <- fitfunc_rf(Chrom,500,data,vars,CPUs)
      Chrom <- DeNoise(Chrom,prop,rfresult,limitNoise)

  }
  cat("evaluating the ObjV of initial population \n")
  ObjV <- fitfunc_rf(Chrom,500,data,vars,CPUs)$ObjV
  
  ObjV.matrix <- matrix(NA,MAXGEN,popSize)
  varimpmatrix <- matrix(NA,MAXGEN,vars)
  Chrom.last <- matrix(NA,popSize,vars)
  cat("preparing for the evolution process \n")
  runtime.garf <- system.time(for (gen in 1:MAXGEN){
    
    FitnV <- ranking(-ObjV,2,subpop)
    SelCh <- select("sus",Chrom,FitnV,1,subpop)
    SelCh <- recombin("xovsp",SelCh,0.7,subpop)
    SelCh <- mutate("mut",SelCh,SUBPOP = subpop)
    for (i in 1:popSize){
      while (sum(SelCh[i, ]) == 0) {
        SelCh[i, ] = sample(c(rep(0, prop), 1), vars, rep = TRUE)
      }    
    }
    oncetime <- system.time(rf.result <- fitfunc_rf(SelCh,500,data,vars,CPUs))
    if (VINDopt1 == 1) SelCh <- DeNoise(Chrom = SelCh,prop,rfresult = rf.result,limitNoise)
    ObjVSel <- rf.result$ObjV
    varimpmatrix[gen,] <- apply(rf.result$evalImportances,2,median,na.rm = TRUE)
    ObjV <- ObjVSel
    Chrom <- SelCh
#     evaluate(matlab,"[Chrom ObjV] = reins(Chrom,SelCh,SUBPOP,1,ObjV,ObjVSel);")
    
    
    #???????Ӳ?  *******?д?????********
    #if (gen %% 20 == 0) evaluate(matlab,"[Chrom,ObjV] = migrate(Chrom,SUBPOP,[0.001,1,1],ObjV);")
    
    ObjV.matrix[gen,] <- ObjV
    #   data.FitnV <- getVariable(matlab,"FitnV")
    #   FitnV.matrix[gen,] <- data.FitnV$FitnV
    data.Chrom <- Chrom
    if (gen == MAXGEN) Chrom.last <- Chrom
    
    cat(paste("GEN = ",gen))
    for (i in 1:subpop){
      sequence <- seq(0,popSize,popSize/subpop)
      best <- round(max(ObjV[(sequence[i]+1):sequence[i+1]]),digits = 2)
      mean <- round(mean(ObjV[(sequence[i]+1):sequence[i+1]]),digits = 2)
      cat(paste(" subpop",i," best=",best," mean=",mean,"|"))
    }
    cat(paste("\n"))
    gc()
    mem.used <- memory.size()
    if (mem.used > 800) cat(paste("memory used = ",mem.used," \n"))
    time.remain <- oncetime[3]*(MAXGEN-gen)/60
    cat(sprintf("The remaining time is about %1.0f minutes %1.0f seconds \n",time.remain,60*(time.remain %% 1)))
    gen <- gen + 1
  })
  
  

  if (VINDopt1 == 1 & is.null(limitNoise)) elapsed.time <- c(VINDP = runtime.VINDP[3],ga = runtime.garf[3]) else elapsed.time <- runtime.garf[3]
  
  ObjV.matrix <- t(ObjV.matrix)
  ObjV.list <- SplitData(ObjV.matrix,subpop)
  ObjV.result <- matrix()
  for (i in 1:subpop){
    ObjV <- as.matrix(unlist(ObjV.list[[i]]),popSize/subpop,MAXGEN)
    max.mean <- cbind(apply(ObjV,2,max),apply(ObjV,2,mean))
    colnames(max.mean) <- c(paste("subpop",i," best",sep=""),
                            paste("subpop",i," mean",sep=""))
    if (i == 1) ObjV.result <- cbind(GEN = 1:MAXGEN,max.mean) else ObjV.result <- cbind(ObjV.result,max.mean)
  }
 
  best.Chrom <- list()
  for(i in 1:subpop){
    sequence <- seq(0,popSize,popSize/subpop)
    best.Chrom[[i]] <- Chrom.last[(sequence[i]+1):sequence[i+1],]
  }
  
  evalMedian <- apply(varimpmatrix,2,median,na.rm = TRUE)
  varfreq <- apply(Chrom.last,2,sum)

  subpop_freq <- matrix(NA,vars,subpop)
  for (i in 1:subpop){
    subpop_freq[,i] <- apply(best.Chrom[[i]],2,sum)
  }
  colnames(subpop_freq) <- paste("subpop",1:subpop,"_freq",sep="")

  sort.var <- cbind(evalMedian, varfreq,subpop_freq)
  rownames(sort.var) <- colnames(X)
  sort.var <- sort.var[order(-varfreq, -evalMedian, na.last = TRUE), ]
  
  
  result <- list(time = elapsed.time,
                 ObjV = ObjV.result,
                 Chrom = best.Chrom,
                 Selectedvar = sort.var)
  class(result) <- "mpga"
  return(result)
}