# Write a function backward(nn,k) for computing the derivatives of the loss 
# corresponding to output class k for network nn (returned from forward). 
# Derivatives w.r.t. the nodes, weights and offsets should be computed and added
# to the network list as lists dh, dw, db. The updated list should be the return
# object

# "f_" marks h, W and b returned from forward function.
# f_h: a list of nodes for each layer. f_h[[l]] is a vector of length d[l]
# which contains the node values for layer l.
# f_W: a list of weight matrices. f_W[[l]] is the weight matrix linking layer l 
# to layer l+1.
# f_b: a list of offset vectors. b[[l]] is the offset vector linking layer l to 
# layer l+1.
c(f_h, f_W, f_b) <- forward(nn, inp)

# k is an integer representing class
backward<-function(nn, k){
  # First, calculate derivative of the loss for k_i w.r.t h^L_j: dlj
  # L: the number of layers in total
  L <- length(f_h)
  for (l in range(L)){
    hl <- f_h[[l]]
    # Apply exp() to all elements in h_l 
    hlq_exp <- sapply(hl, exp)
    sumq <- sum(hlq_exp)
    Dl = hl/sumq
    Dl[k] = hl[k]/(sumq-1)
    dl = pmax(Dl, 0)
  }
    
  
  result = list("h" = b_h, "W" = b_W, "b" = b_b, "dh" = dh, "dW" = dW, "db" = db)
  return (result)
}