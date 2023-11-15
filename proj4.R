# netup() is a function to return a list of vector containing:
# h: a list of nodes for each layer. h[[l]] is a vector of length d[l] which 
# contains the node values for layer l.
# W: a list of weight matrices W[[l]] is the weight matrix linking layer l to 
# layer l+1, so W[[l]] is a d[l+1]*d[l] matrix and the length of W list will be 
# L-1 (L is the number of total layers). Initialize the elements with U(0, 0.2) 
# random deviates.
# b: a list of offset vectors. b[[l]] is the offset vector linking layer l to 
# layer l+1, so the length of b[[l]] is equal to d[l+1] and there are L-1 vectors
# in list b (L is the maximum layer). Initialize the elements with U(0, 0.2) 
# random deviates.
netup <- function(d){
  # "lapply" applies function (length){return(rep(0,length))} to each element in
  # d. For example, h[[i]] is a zero vector and length(h[[i]]) = d[i] 
  h <- lapply(d, function(length) rep(0,length))
  W <- list()
  b <- list()
  L <- length(d) # The number of layers
  
  for (i in 1:(L-1)){
    # Initialize the size of 'W' and W[[i]] is a d[i+1]*d[i] matrix
    # Fill in the W matrices with uniformly distributed numbers within 0 and 0.2
    W[[i]] <- matrix(runif(d[i] * d[i+1], 0, 0.2), d[i+1], d[i])
    # Same idea to initialize b
    b[[i]] <- runif(d[i+1], 0, 0.2)
  }
  
  return(list("h"=h, "W"=W, "b"=b))
}



# nn: a network list as returned by netup
# inp: a vector of input values for the first layer. 
# This forward function should compute the remaining node values implied by inp, 
# and return the updated network list (as the only return object).
forward <- function(nn, inp){
  # "netup_" marks network list returned by function netup
  netup_h <- nn$h
  netup_W <- nn$W
  netup_b <- nn$b
  
  update_h <- netup_h
  update_h[[1]] <- inp # Fill in updated h^1 with inp
  
  netup_L <- length(netup_h) # The number of layers
  
  for(l in 1:(netup_L-1)){
    # For layer l, Whb = W %*% h + b. Whb^l is a vector 
    # with length(Whb^l) = length(h^l+1)
    Whb <- netup_W[[l]] %*% update_h[[l]] + netup_b[[l]]
    # For h in layer l+1, h_j^(l+1) = max(Whb_j^l, 0)
    # "sapply" applies function(x){return(pmax(0, x))} to each value in Whb^l and
    # return a vector with positive values in Whb^l unchanged and change negative 
    # values to zeros
    update_h[[l+1]] <- sapply(Whb, function(x) pmax(0, x))
  }
  
  networklist <- list("h" = update_h, "W" = netup_W, "b" = netup_b)
  return(networklist)
}

# Write a function backward(nn,k) for computing the derivatives of the loss 
# corresponding to output class k for network nn (returned from forward). 
# Derivatives w.r.t. the nodes, weights and offsets should be computed and added
# to the network list as lists dh, dw, db. The updated list should be the return
# object



# nn: network returned from forward function
# k: an integer representing class
backward<-function(nn, k){
  # "f_" marks h, W and b returned from forward function.
  # The length of lists h, W, b are the same as explained above for netup function
  # and forward function
  f_L <- length(f_h) # The number of layers
  
  f_h <- nn$h # length: f_L
  f_W <- nn$W # length: f_L - 1
  f_b <- nn$b # length: f_L - 1
  
  # Create empty lists dh, db, dW with sublists
  dh <- vector("list", f_L)   # length: f_L
  db <- vector("list", f_L-1) # length: f_L - 1
  dW <- vector("list", f_L-1) # length: f_L - 1
  
  # First, calculate the derivative of the loss k w.r.t. nodes on the last layer
  sumq <- sum(exp(f_h[[f_L]])) # sum exp(h_q) for all q in the last layer
  dh[[f_L]] <- f_h[[f_L]]/sumq
  dh[[f_L]][k] <- f_h[[f_L]][k]/sumq - 1
  
  # Second, compute derivatives of L w.r.t all other nodes by working backwards
  # through the layers applying the chain rule (back-propagation)
  for (l in (f_L-1):1){
    # For layer l
    d <- sapply(dh[[l+1]], function(x) pmax(0, x))
    dh[[l]] <- t(f_W[[l]]) %*% d
    db[[l]] <- d
    dW[[l]] <- d %*% t(f_h[[l]])
  }
  
  # Update the network lists
  network = list("h" = f_h, "W" = f_W, "b" = f_b, "dh" = dh, "dW" = dW, "db" = db)
  return (network)
}

###### "b_" marks the network list returned from backward function
###### b_nn <- backward(f_nn, k)

### ? mb return
train <-function(nn, inp, k, eta = .01, mb = 10, nstep = 10000){
  mb
  
  for (istep in nstep){
    nn <- forward(nn, inp)
    b_nn <- backward(nn, k)
    nn$w <- mapply(function(x, y) x - eta*y, b_nn$w, b_nn$dw, SIMPLIFY = FALSE)
    nn$b <- mapply(function(x, y) x - eta*y, b_nn$b, b_nn$db, SIMPLIFY = FALSE)
  }
  return (nn)
}

# we use the dataset 'iris' from R to train a 4-8-7-3 network
data(iris)

# find the class for species in 'iris'
class <- unique(iris[,"Species"])
# let different speices represent in different number
iris[,"Species"]<-as.numeric(iris[,"Species"])

# 'indices' is the rows for 'iris'
indices <- 1:nrow(iris)
# select the test data consists of every 5 rows
iris_test <- iris[indices %% 5 == 0,]
# select the train data consists of iris which are not in test data
iris_train <- iris[indices %% 5 != 0,]

iris_nn <- netup(c(4,8,7,3))
# look through each row in train data
for (i in iris_train){
  # use train() to train the network 4-8-7-3
  # given train data in the rows of matrix which is the first four elements for each row in train data
  iris_nn <- train(iris_nn, i[1:4], i[5])
}
# look through each row in test data
for (i in iris_test){
  # use forward() to compute the remaining node values implied by the first four elements 
  # for each row in test data and update the network list
  i_nn <- forward(iris_nn, i[1:4])
  
}




