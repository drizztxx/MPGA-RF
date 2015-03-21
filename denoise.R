
DeNoise <- function(Chrom = Chrom,prop = 1,rfresult = rfresult,limitNoise = limitNoise,...){
  nn <- NROW(Chrom)
  cc <- NCOL(Chrom)
  for (individual in 1:nn) {
    numObjectNow <- sum(Chrom[individual, ])
    if (numObjectNow > limitNoise[1, ncol(limitNoise)]) {
      limitNoiseNow <- limitNoise[2, ncol(limitNoise)]/2
    } else {
      limitNoiseNow_i <- 1
      while (numObjectNow > limitNoise[1, limitNoiseNow_i]) {
        limitNoiseNow_i <- limitNoiseNow_i + 1
      }
      limitNoiseNow <- limitNoise[2, limitNoiseNow_i]
    }
#     cat(paste("individual = ",individual,"\n"))
#     if (individual == 301) cat(rfresult$evalImportances[individual, ])
    Chrom[individual, rfresult$evalImportances[individual, 
                                               ] <= limitNoiseNow] <- 0
    
    while (sum(Chrom[individual, ]) == 0) {
#       debug.vars = NCOL(Chrom)
#       cat(paste("individual = ",individual,"\n"))
#       cat(paste("Chrom = ",NCOL(Chrom),"\n"))
#           cat(paste("vars = ",debug.vars,"\n"))
#               cat(paste("prop = ",prop,"\n"))
      Chrom[individual, ] = sample(c(rep(0, prop), 1), cc, rep = TRUE)
     }
      
    }
    return(Chrom)
}
