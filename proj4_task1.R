# task 1
# netup() is a function to return a list of vector containing:
# h: a list of nodes for each layer where the length of each list is equal to the length of each layer
# w: a list of weight matrices where the length of the list is the number of steps
# and for each matrix, the dimension of the matrix is equal to the length of next layer multiply 
# the length of current layer
# b: a list of offset vectors where the length of the list is also the number of steps
# and for each vector, the length of the vector is equal to the length of next layer
netup <- function(d){
  h <- list() # 'h' is a list to store the list of nodes for each layer
  w <- list() # 'w' is a list to store the weight matrix for each layer
  b <- list() # 'b' is a list to store the offset vector for each layer
  
  layer_size <- length(d) # number of layers
  for (i in 1:layer_size){
    # Initialize the value of nodes for each layer to 0
    h[[i]] <- rep(0,d[i])
  }
  for (i in 1:(layer_size-1)){
    # Initialize the elements of 'w' and 'b' with U(0,0.2) random deviates
    # Store the weight matrix linking the later i to layer i+1 to the i-th element of 'w'
    w[[i]] <- matrix(runif(d[i] * d[i+1], 0, 0.2), d[i+1], d[i])
    # Store the offset vector linking the later i to layer i+1 to the i-th element of 'w'
    b[[i]] <- runif(d[i+1], 0, 0.2)
  }
  
  return(list(h=h,w=w,b=b))
}

