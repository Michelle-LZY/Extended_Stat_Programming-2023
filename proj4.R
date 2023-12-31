# Group members: Bo Gao(s2511232), Zheyue Lin(s2519324) and Xinran Zhu(s2508695)
# https://github.com/Michelle-LZY/Extended_Stat_Programming-2023
# Xinran Zhu(s2508695) wrote netup() function.
# Zheyue Lin(s2519324) coded forward() and backward() functions.
# Bo Gao(s2511232) worked on train() function and tested training data set.

# Briefly Introduction of the project:
# This project sets up a simple neural network for classification, and to train 
# it using stochastic gradient descent.
# There are different layers in the network and on each layer there are nodes. 
# Nodes of the network each contain values h^l_j. 
# The values for the first layer nodes are set to the values of input data: inp. 
# The network then combines and transforms the values in each layer to produce 
# the values at the next layer, until we reach the output layer L. We wrote a 
# forward function to compute the remaining node values implied by inp. The output
# layer nodes are used to predict output data (aka response data) associated with 
# the input data. 
# The network has parameters controlling the combinations and transformations 
# linking each layer to the next.(matrix W) These parameters are adjusted in 
# order to make the input data best predict the output data according to some loss
# function, L. This is known as training the network. 
# Stochastic Gradient Descent is an optimization approach that helps to maintain 
# a good generalizing power, without getting too fixated on fitting any particular 
# set of data. The idea is to repeatedly find the gradient of the loss function 
# w.r.t. the parameters for small randomly chosen subsets of the training data, 
# and to adjust the parameters by taking a step in the direction of the negative 
# gradient.

# Set seed to make the results more stable
set.seed(2)

# netup() is the first function to use, a function to return a list of vector 
# containing:
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
  h <- lapply(d, function(length) rep(0, length))
  W <- list()
  b <- list()
  # The number of layers
  L <- length(d)
  
  # There are only L-1 W matrices and L-1 b vectors
  for (i in 1:(L - 1)){
    # Initialize the size of 'W' and W[[i]] is a d[i+1]*d[i] matrix
    # Fill in the W matrices with uniformly distributed numbers within 0 and 0.2
    W[[i]] <- matrix(runif(d[i] * d[i + 1], 0, 0.2), d[i + 1], d[i])
    # Same idea to initialize b
    b[[i]] <- runif(d[i + 1], 0, 0.2)
  }
  
  # Return initialized network list
  return(list("h" = h, "W" = W, "b" = b))
}


# forward() is the second function to use, compute the remaining node values 
# implied by inp, and return the updated network list (as the only return object).
# nn: a network list as returned by netup
# inp: a vector of input values for the first layer. 
# This forward function should 
forward <- function(nn, inp){
  # "netup_" marks network list returned by function netup
  update_h <- nn$h
  netup_W <- nn$W
  netup_b <- nn$b
  # Fill in updated h^1 with inp
  update_h[[1]] <- inp 
  
  # The number of layers
  netup_L <- length(update_h) 
  
  for(l in 1:(netup_L - 1)){
    # For layer l, Whb = W %*% h + b. Whb is a vector.
    # W[[l]] is the matrix linking l and l+1
    # h[[l]] is a nodes vector
    # b[[l]] is an offset vector
    # with length(Whb^l) = length(h^(l+1))
    Whb <- netup_W[[l]] %*% update_h[[l]] + netup_b[[l]]
    # For h in layer l+1, h_j = max(Whb_j, 0)
    update_h[[l + 1]] <- Whb
    update_h[[l + 1]][Whb < 0] <- 0
  }
  
  # Return updated network list
  networklist <- list("h" = update_h, "W" = netup_W, "b" = netup_b)
  return(networklist)
}


# Write a function backward(nn,k) for computing the derivatives of the loss 
# corresponding to output class k for network nn (returned from forward function). 
# Derivatives w.r.t. the nodes, weights and offsets should be computed and added
# to the network list as lists dh, dw, db. 
# nn: network returned from forward function
# k: an integer representing class
backward<-function(nn, k){
  # "f_" marks h, W and b returned from forward function.
  # The length of lists h, W, b are the same as explained above for netup function
  f_h <- nn$h ## length: f_L
  f_W <- nn$W ## length: f_L - 1
  f_b <- nn$b ## length: f_L - 1
  f_L <- length(f_h) ## The number of layers
  
  # Create an empty list having L sublists
  dh <- vector("list", f_L)
  # Calculate the derivative of the loss k w.r.t. nodes on the last layer L
  # dh = exp(hL[j])/sum(hL[q]), where f_h[[f_L]] = hL and sum all q in layer L 
  # Except for j= k, dh = exp(hL[j])/sum(hL[q]) - 1
  exphL <- exp(f_h[[f_L]])
  expj_sumq <- exphL/sum(exphL)
  dh[[f_L]] <- expj_sumq
  dh[[f_L]][k] <- expj_sumq[k] - 1
  
  # Compute derivatives of L w.r.t all other nodes by working backwards through 
  # the layers applying the chain rule (back-propagation)
  db <- vector("list", f_L - 1) ## length: f_L - 1
  dW <- vector("list", f_L - 1) ## length: f_L - 1
  
  # Calculate derivatives backwards
  for (l in (f_L - 1):1){
    # For layer l
    # If h(l+1)[j] is positive, d[j] = dh(l+1)[j]
    # If h(l+1)[j] is negative, d[j] = 0
    d <- dh[[l + 1]]*(f_h[[l + 1]] > 0)
    dh[[l]] <- t(f_W[[l]]) %*% d ## dh = W^T %*% d
    db[[l]] <- d                 ## db = d
    dW[[l]] <- d %*% t(f_h[[l]]) ## dW = d %*% h^T
  }
  
  # Update the network lists
  network = list("h" = f_h, "W" = f_W, "b" = f_b, "dh" = dh, "dW" = dW, "db" = db)
  return (network)
}


# train function combined forward and backward functions to find the optimal 
# value of W and b to get the best result for the current tasks and return the 
# updated network list
# nn: a network list as returned by netup function
# inp: a vector of input values for the first layer. 
# k: an integer representing class
# eta: a float representing step size
# mb: an integer representing the number of data to randomly sample
# nstep: an integer representing the the number of optimization steps
train <- function(nn, inp, k, eta = .01, mb = 10, nstep = 10000){
  # The number of layers
  L <- length(nn$h)
  
  # Iterative nstep times to train W and b
  for (istep in 1:nstep){
    # Randomly choose mb rows as first layer values inp
    s <- sample(nrow(inp), mb)
    
    # Create empty lists sum_dW and sum_db to store the sum of derivatives dW, db
    # There are L-1 sublists in sum_dW and sum_db
    # Giving zeros to empty sublists
    sum_dW <- as.list(rep(0, L - 1))
    sum_db <- as.list(rep(0, L - 1))
    
    # Iterative the randomly chose first layer inputs to get the different dW 
    # and db values, and then sum them up
    for (i in s){
      # Use forward function to update all nodes in h list
      f_nn <- forward(nn, as.numeric(inp[i,]))
      # Use backward function to get dW and db
      b_nn <- backward(f_nn, k[i])
      
      # b_nn_dW and b_nn_db are used to store updated dW and db
      b_nn_dW <- b_nn$dW
      b_nn_db <- b_nn$db
      for (j in 1:length(b_nn_db)){
        sum_dW[[j]] <- sum_dW[[j]] + b_nn_dW[[j]] ## Add b_nn_dW to sum_dW
        sum_db[[j]] <- sum_db[[j]] + b_nn_db[[j]] ## Add b_nn_db to sum_db 
      }
    }
    
    # Get the average values of dW and db
    # new W = old W - eta*sum_dW/mb, where sum_dW/mb is the average value of dW
    # new b = old b - eta*sum_dw/mb, where sum_db/mb is the average value of db
    for (j in 1:(L - 1)){
      nn$W[[j]] <- nn$W[[j]] - sum_dW[[j]]*eta/mb
      nn$b[[j]] <- nn$b[[j]] - sum_db[[j]]*eta/mb
    }
  }
  
  return (nn)
}

# Use the dataset 'iris' from R to train a 4-8-7-3 network
data(iris)

# Find the species in 'iris'
class <- unique(iris[, "Species"])
# Use 1,2,3 to represent setosa, versicolor and virginica
iris[,"Species"] <- as.numeric(iris[, "Species"])

# Get row index for 'iris'
indices <- 1:nrow(iris)
# Select the test data by every 5 rows
iris_test <- iris[indices %% 5 == 0,]
# Except for test data, left of the dataset is training data
iris_train <- iris[indices %% 5 != 0,]

# Set the network structure and initialize it by netup function
layers <- c(4, 8, 7, 3)
iris_nn <- netup(layers)

# Train the network based on training dataset
iris_nn <- train(iris_nn, iris_train[, 1:4], iris_train[, 5])

misclassification <- 0 ## The number of mis-classification
# Iterative rows to check the classification result produced by trained network model 
for (i in 1:nrow(iris_test)){
  # Compute the remaining node values implied by the first four elements 
  i_nn <- forward(iris_nn, as.numeric(iris_test[i, 1:4]))
  # Find the maximum value in the last layer, the predicted class
  i_class <- which.max(i_nn$h[[length(layers)]])
  # If the predicted class is equal to the label, the predicted result is right
  # Otherwise it is wrong
  # Update mis-classification numbers
  if (i_class != iris_test[i, 5]){
    misclassification <- misclassification + 1
  }
}

# Calculate the mis-classification rate (i.e. the proportion mis-classified) for 
# the test set.
misclassification_rate <- misclassification/nrow(iris_test)
print(paste("mis-classification rate: ", misclassification_rate))