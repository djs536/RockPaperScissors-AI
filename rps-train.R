rpsTrain <- function(x, epochs, learning_rate = 0.01, h = 10, sharing = F) {
  
  ### Confirm format & prep data
  if(ncol(x) != 11 | sum(x[,c(1:5,11)] %in% c(1:3)) != 6 * nrow(x) | sum(x[,6:10] %in% c(-1:1)) != 5 * nrow(x)) {
    print("Error: x[,1:5] should be human plays, x[,6:10] should be outcomes, and x[,11] should be next move.")
    return(NULL)
  } # confirms format of 5 plays, 5 outcomes, new play is correct and by row
  
  ### One-hot encoding of data
  historical <- matrix(nrow = nrow(x), ncol = 0) # create storage matrix for encoded data
  
  for(i in 1:5) {
    rocks <- ifelse(x[,i] == 1, 1, 0) # ID cases of rock played on a given turn
    papers <- ifelse(x[,i] == 2, 1, 0) # ID cases of paper played on a given turn
    scissors <- ifelse(x[,i] == 3, 1, 0) # ID cases of scissors played on a given turn
    
    plays <- matrix(nrow = nrow(x), ncol = 3) # create new matrix to store encoded plays
    
    plays[,1] <- rocks
    plays[,2] <- papers
    plays[,3] <- scissors
    historical <- cbind(historical, plays) # append plays to storage matrix
    
  }
  
  for(i in 6:10) {
    wins <- ifelse(x[,i] == 1, 1, 0) # ID wins
    ties <- ifelse(x[,i] == 0, 1, 0) # ID ties
    losses <- ifelse(x[,i] == -1, 1, 0) # ID losses
    
    outcomes <- matrix(nrow = nrow(x), ncol = 3) # create new matrix to store encoded outcomes
    
    outcomes[,1] <- wins
    outcomes[,2] <- ties
    outcomes[,3] <- losses
    
    historical <- cbind(historical, outcomes)
    
  }
  
  # Follow same process for output variable
  
  rocks <- ifelse(x[,11] == 1, 1, 0)
  papers <- ifelse(x[,11] == 2, 1, 0)
  scissors <- ifelse(x[,11] == 3, 1, 0)
  
  output <- matrix(nrow = nrow(x), ncol = 3)
  
  output[,1] <- rocks
  output[,2] <- papers
  output[,3] <- scissors
  
  
  ### Define other functions
  
  # Function to initialize parameters
  initializeParams <- function(n, h) {
    W1 <- matrix(runif(n = n * h, min = -0.1, max = 0.1), nrow = n, ncol = h) # W1 is input vector length by h hidden neurons
    b1 <- rep(0, h) # b1 is vector of h hidden neurons length
    W2 <- matrix(runif(n = h * 3, min = -0.1, max = 0.1), nrow = h, ncol = 3) # W2 is h hidden neurons by output vector length (3)
    b2 <- rep(0, 3) # b2 is output vector length
    return(list(W1 = W1, b1 = b1, W2 = W2, b2 = b2))
  }
  
  # Forward pass function
  forwardPass <- function(X, params = params) {
    
    # Define softmax function
    softmax <- function(p) {
      exp_p <- exp(p - apply(p, 1, max))   # subtract max from each row
      return(exp_p / rowSums(exp_p))
    }
    
    # extract weights, biases from list params
    W1 <- params$W1
    b1 <- params$b1
    W2 <- params$W2
    b2 <- params$b2
    
    # Hidden layer pre-activation
    Z1 <- X %*% W1 + b1
    
    # Hidden layer activation
    A1 <- matrix(pmax(0, Z1), nrow = nrow(Z1))
    
    # Output layer pre-activation
    Z2 <- A1 %*% W2 + b2
    
    # Get Yhat (softmax of Z2)
    Yhat <- softmax(Z2)
    
    return(list(Yhat = Yhat, A1 = A1, Z1 = Z1))
    
  }
  
  # Loss function
  computeLoss <- function(Y, Yhat) {
    loss <- -sum(Y * log(Yhat + 1e-8)) / nrow(Y)
    return(loss)
  }
  
  # Backpropagation function
  backprop <- function(X, Y, forward_output, params) {
    
    # extract weights, biases from list params
    W1 <- params$W1
    b1 <- params$b1
    W2 <- params$W2
    b2 <- params$b2
    
    # extract Y-hat, A1, Z1 from forward pass list
    Yhat <- forward_output$Yhat
    A1 <- forward_output$A1
    Z1 <- forward_output$Z1
    
    # compute output layer gradient
    n <- nrow(X)
    dZ2 <- Yhat - Y
    
    # compute gradients for W2, b2
    dW2 <- t(A1) %*% dZ2 / n
    db2 <- colSums(dZ2) / n
    
    # backpropagate to hidden layer
    dA1 <- dZ2 %*% t(W2)
    
    # apply ReLU derivative
    dZ1 <- dA1 * (Z1 > 0)
    
    # compute gradients for W1, b1
    dW1 <- t(X) %*% dZ1 / n
    db1 <- colSums(dZ1) / n
    
    # return all gradients as list
    return(list(dW1 = dW1, db1 = db1, dW2 = dW2, db2 = db2))
    
  }
  
  # update parameters function
  updateParams <- function(params, grads, learning_rate) {
    params$W1 <- params$W1 - learning_rate * grads$dW1
    params$b1 <- params$b1 - learning_rate * grads$db1
    params$W2 <- params$W2 - learning_rate * grads$dW2
    params$b2 <- params$b2 - learning_rate * grads$db2
    
    return(params)
  }
  
  # training function
  trainModel <- function(X, Y, params, epochs, learning_rate, print) {
    loss_history <- numeric(epochs)
    
    for(i in 1:epochs) {
      
      # forward pass
      forward_output <- forwardPass(X, params)
      
      # compute loss
      loss <- computeLoss(Y, forward_output$Yhat)
      loss_history[i] <- loss
      
      # backprop
      grads <- backprop(X, Y, forward_output, params)
      
      #update weights
      params <- updateParams(params, grads, learning_rate)
      
      if(sharing == T & i %% 10 == 0) {
        cat("Epoch", i, "- Loss:", round(loss, 4), "\n")
      }
    }
    
    return(list(params = params, loss = loss_history))
  }
  
  
  ### Initialize parameters
  params <- initializeParams(n = 30, h = h)
  
  ### Train model
  trained <- trainModel(X = historical, Y = output, params = params, epochs = epochs, learning_rate = learning_rate, print = print)
  
  return(trained)
  
}