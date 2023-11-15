# task 1
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
  f_h <- nn$h
  f_W <- nn$W
  f_b <- nn$b
  
  f_L <- length(f_h) # The number of layers
  
  # First, calculate the derivative of the loss k_i w.r.t. nodes on layer L
  
  # Define a function to calculate dlj by nodes for layer l
  # hl is a vector containing node values for layer l
  cal_dlj<- function(hl, k) {
    # Calculate derivatives of the loss for ki w.r.t hlj: Dl[j]
    # "sapply" applies exp() to all nodes in hl and return a vector that the q-th
    # element in the vector is exp(hl[q]), then sum up all exp(hl[q])
    sumq <- sum(exp(hl))
    # Except for j=k, derivative of the loss of ki w.r.t hlj is 
    # exp(hl[j])/sum(exp(hl[q]))
    Dl = hl/sumq
    # When j=k, derivative is equal to exp(hl[j])/(sum(exp(hl[q]))-1)
    Dl[k] = hl[k]/(sumq-1)
    # If hl[j] <= 0, dl[j] = 0; otherwise, dl[j] = Dl[j]
    dl <- Dl
    dl[which(hl < 0)] <- 0
    return (dl)
  }
  # "lapply" will apply cal_dlj function to each sublist in f_h, the vectors hl 
  # for each layer, and return a new list with sublists containing dl for each 
  # layer.
  d <- lapply(f_h, cal_dlj)
  # Then, compute the derivatives of Li w.r.t all the other hl[j] by working
  # backwards through layers applying the chain rule
  # "lapply" will make all sublists in f_W, weight matrices, transpose and return
  # a new list containing transposed matrices
  tW <- lapply(f_W, t)
  
  adjusted_tW <- tW[1:f_L-1]
  adjusted_d <- d[2:f_L]
  # "mapply" use function crossprod() on each sublist in adjusted_tW and adjusted_d
  # equally, dh[i] = adjusted_tW[i] %*% adjusted_d[i] for all i
  dh <- mapply(crossprod, adjusted_tW, adjusted_d, SIMPLIFY = FALSE)
  
  db <- adjusted_d
  
  # "lapply" will make all sublists in adjusted_f_h, derivative of the loss, 
  # transpose and return a new list containing transposed matrices
  # "mapply" use function crossprod() on each sublist in adjusted_d and adjusted_tf_h
  # equally, dW[i] = adjusted_d[i] %*% adjusted_tf_h[i] for all i
  # Set "SIMPLIFY = FALSE" to make the function return a list
  adjusted_tf_h <- lapply(f_h[1:L-1], t)
  dW <- mapply(crossprod, adjusted_d, adjusted_tf_h, SIMPLIFY = FALSE)
    
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




