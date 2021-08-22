smootherMatrixLoess <-function(x
                               ,span=NULL,
                               enp.target=NULL,...) {
  n <- length(x)
  S <- matrix(0, n, n)
  for (i in 1:n) {
    ei =rep(0, n)
    ei[i] <-1
    if (is.null(span) &is.null(enp.target))
    {
      S[,i]  <-predict(loess(ei ~x, ...))
    } else {
      if (is.null(span)) {
        S[,i]  <-predict(loess(
          ei ~x,enp.target=enp.target,...))
      } else {
        S[,i]  <-predict(loess(ei ~x,
                               span=span,...))
      }
    }
  }
  S
}