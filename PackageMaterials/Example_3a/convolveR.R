
convolveR <- function(a, b) {
  if(length(a) < length(b)) {
    Recall(b, a)
  } else {
    ab <- rep(0, length(a) + length(b) - 1)
    ind <- 1:length(a)
    for(j in 1:length(b)) {
      ab[ind] <- ab[ind] + a*b[j]
      ind <- ind + 1
    }
    ab
  }
}



convolveJMC <- function(x, y) {  ## John Chambers, "Extending R"
  nx <- length(x)
  ny <- length(y)
  if(nx < ny) Recall(y, x) else {
   nxy <- nx + ny - 1
   XY <- outer(x, y)
   XY <- rbind(XY, matrix(0, ny, ny))
   length(XY) <- nxy * ny
   dim(XY) <- c(nxy, ny)
   rowSums(XY)
  }
}


convolveFFT <- function (x, y) { ## Ross Ihaka' FFT version
  nx <- length(x)
  ny <- length(y)
  x <- c(rep.int(0, ny - 1), x)
  y <- c(rep.int(0, nx - 1), rev(y))
  xy <- Re(fft(fft(x) * Conj(fft(y)), inverse = TRUE))/(nx + ny - 1)
  iy <- 1:ny
  c(xy[-iy], xy[iy])
}


convolveSTATS <- function(x, y) ## adapted from the stats package
  stats::convolve(x, rev(y), type = "open")

