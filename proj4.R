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
start_time <- Sys.time()

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
  
  return(list("h" = h, "W" = W, "b" = b))
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
  
  for(l in 1:(netup_L - 1)){
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
  f_h <- nn$h # length: f_L
  f_W <- nn$W # length: f_L - 1
  f_b <- nn$b # length: f_L - 1
  f_L <- length(f_h) # The number of layers
  
  # Define function cal_derivative_L to calculate the derivative of the loss k 
  # w.r.t. nodes on the last layer
  # hL is h^L, a vector of nodes in the last layer in h list: f_h[[L]]
  cal_derivative_L <- function(hL){
    # Create an empty list having f_L sublists
    dh <- vector("list", f_L)
    # expj is a vector that expj[j] = exp(hL[j])
    expj <- exp(hL)
    # Sum exp(hL[q]) for all q in hL
    sumq <- sum(expj)
    # dh_L[j] = exp(h_L)[j]/sum(h_L[q]), if j is not equal to k
    dh[[f_L]] <- expj/sumq
    # dh_L[k] = exp(h_L)[k]/sum(h_L[q])
    dh[[f_L]][k] <- expj[k]/sumq - 1
    return(dh)
  } 
  dh <- cal_derivative_L(f_h[[f_L]])
  
  # Anytime we code up gradients we need to test them, by comparing the coded
  # gradients with finite difference approximations
  esp <- 1e-7 ## finite difference interval
  dh_0 <- cal_derivative_L(c(f_h[[f_L]] + esp))
  # cat("Check derivatives at the last layer:", "\n", dh[[f_L]], "\n", dh_0[[f_L]])
  
  # Second, compute derivatives of L w.r.t all other nodes by working backwards
  # through the layers applying the chain rule (back-propagation)
  db <- vector("list", f_L - 1)   # length: f_L - 1
  db_0 <- vector("list", f_L - 1) # length: f_L - 1
  dW <- vector("list", f_L - 1)   # length: f_L - 1
  dW_0 <- vector("list", f_L - 1) # length: f_L - 1
  for (l in (f_L-1):1){
    # For layer l
    # If h(l+1)[j] is positive, d[j] = dh(l+1)[j]
    d <- dh[[l+1]]
    # If h(l+1)[j] is negative, d[j] = 0
    d[which(f_h[[l+1]] < 0)] <- 0
    dh[[l]] <- t(f_W[[l]]) %*% d
    db[[l]] <- d
    dW[[l]] <- d %*% t(f_h[[l]])
    
    # Again, finite difference check
    d_0 <- dh_0[[l+1]]
    d_0[which(f_h[[l+1]] < 0)] <- 0
    dh_0[[l]] <- t(f_W[[l]]+ esp)%*% d_0
    db_0[[l]] <- d_0
    dW_0[[l]] <- d_0 %*% t(f_h[[l]] + esp)
  }
  #cat("Check derivatives:", "\n")
  #cat("dh and dh_0:", "\n")
  #dh; dh_0
  #cat("dW and dW_0:", "\n")
  #dW; dW_0
  #cat("db and db_0:", "\n")
  #db; db_0
  
  # Update the network lists
  network = list("h" = f_h, "W" = f_W, "b" = f_b, "dh" = dh, "dW" = dW, "db" = db)
  return (network)
}

# nn: a network list as returned by netup
# inp: a vector of input values for the first layer. 
# k: an integer representing class
# eta: a float representing step size
# mb: an integer representing the number of data to randomly sample
# nstep: an integer representing the the number of optimization steps
# This train function ??????????? 
# and return the updated network list (as the only return object).
train <- function(nn, inp, k, eta = .01, mb = 10, nstep = 10000){
  # loops nstep times toupdate W and b
  for (istep in 1:nstep){
    # randomly choose mb rows from inp
    s <- sample((1:nrow(inp)), mb)
    # define sum
    sum_dW <- list()
    sum_db <- list()
    for (i in s){
      f_nn <- forward(nn, as.numeric(inp[i,]))
      b_nn <- backward(f_nn, k[i])
      b_nn_dW <- b_nn$dW
      b_nn_db <- b_nn$db
      if(length(sum_db) == 0){
        sum_dW <- b_nn_dW
        sum_db <- b_nn_db
      }
      else {
        for (j in 1:length(b_nn_db)){
          sum_dW[[j]] <- sum_dW[[j]] + b_nn_dW[[j]]
          sum_db[[j]] <- sum_db[[j]] + b_nn_db[[j]]
        }
      }
    }
    nn$W <- mapply(function(x, y) x - eta*(y/mb), nn$W, sum_dW, SIMPLIFY = FALSE)
    nn$b <- mapply(function(x, y) x - eta*(y/mb), nn$b, sum_db, SIMPLIFY = FALSE)
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

# set the network structures and initialize it
layers <- c(4,8,7,3)
iris_nn <- netup(layers)

# train the network based on train dataset
iris_nn <- train(iris_nn, iris_train[, 1:4], iris_train[, 5])

#!!!!!
test_result<-list()

# define misclassification to store the number of misclassification
misclassification <- 0
# look through each row in test data
for (i in 1:nrow(iris_test)){
  # compute the remaining node values implied by the first four elements 
  # for each row in test data and update the network list
  i_nn <- forward(iris_nn, as.numeric(iris_test[i, 1:4]))
  # find the maximum in the last layers, which is the predicted class
  i_class <- which.max(i_nn$h[[length(layers)]])
  
  # !!!!!!!!!!
  test_result <- append(test_result, i_class)
  
  # if the predicted class is equal to the label, it is true
  # but if the result is different from the label, it means it is wrong
  # so the number of misclassification should be plus 1
  if (i_class != iris_test[i, 5]){
    misclassification <- misclassification + 1
  }
}

end_time <- Sys.time()
end_time - start_time

print(test_result)
print(misclassification/nrow(iris_test))
