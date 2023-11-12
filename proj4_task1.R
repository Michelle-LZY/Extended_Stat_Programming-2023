# task 1
netup <- function(d){
  h <- list()
  w <- list()
  b <- list()
  h[[1]] <- runif(d[1], 0, 1)
  for (i in 1:(length(d)-1)){
    w[[i]] <- matrix(runif(d[i] * d[i+1], 0, 0.2), d[i+1], d[i])
    b[[i]] <- runif(d[i+1], 0, 0.2)
    h[[i+1]] <- as.vector(w[[i]] %*% matrix(h[[i]],d[i],1) + matrix(b[[i]],d[i+1],1))
  }
  
  return(list(h=h,w=w,b=b))
}
