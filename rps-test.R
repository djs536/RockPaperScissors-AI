rpsTest <- function(x, trainedModel) {
  
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
  
  # Extract params
  params <- trainedModel$params
  
  
  ### Define other functions
  
  # Forward pass function
  forwardPass <- function(X, params) {
    
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
    
    return(Yhat)
    
  }
  
  # Loss function
  computeLoss <- function(Y, Yhat) {
    loss <- -sum(Y * log(Yhat + 1e-8)) / nrow(Y)
    return(loss)
  }
  
  predicted <- forwardPass(X = historical, params = params)
  
  return(computeLoss(Y = output, Yhat = predicted))
  
}