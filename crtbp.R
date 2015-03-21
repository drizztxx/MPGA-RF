crtbp <- function(popSize,vars,prop = 1){
  chrom = matrix(nrow = popSize, ncol = vars)
  for (child in 1:popSize) {
    chrom[child, ] = sample(c(rep(0, prop), 1), vars, rep = TRUE)
    while (sum(chrom[child, ]) == 0) {
      chrom[child, ] = sample(c(rep(0, prop), 1), vars, rep = TRUE)
    }
  }
  return(chrom)
}
