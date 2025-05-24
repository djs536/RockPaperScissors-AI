rpsPredictor <- function(hist, model) { # hist = 2x5 matrix; row 1 = plays, row 2 = outcomes
  
  # one-hot encode data
  
  X <- c()
  
  for(i in 1:5) {
    if(hist[1, i] == 1) {play <- c(1, 0, 0)}
    if(hist[1, i] == 2) {play <- c(0, 1, 0)}
    if(hist[1, i] == 3) {play <- c(0, 0, 1)}
    X <- c(X, play)
  }
  
  for(i in 6:10) {
    if(hist[1, i] == -1) {wins <- c(1, 0, 0)}
    if(hist[1, i] == 0) {wins <- c(0, 1, 0)}
    if(hist[1, i] == 1) {wins <- c(0, 0, 1)}
    X <- c(X, wins)
  }
  
  # extract params
  params <- model$params
  
  # extract weights and biases
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  # Define softmax function
  softmax <- function(p) {
    exp_p <- exp(p - apply(p, 1, max))   # subtract max from each row
    return(exp_p / rowSums(exp_p))
  }
  
  # Hidden layer pre-activation
  Z1 <- X %*% W1 + b1
  
  # Hidden layer activation
  A1 <- matrix(pmax(0, Z1), nrow = nrow(Z1))
  
  # Output layer pre-activation
  Z2 <- A1 %*% W2 + b2
  
  # Get Yhat (softmax of Z2)
  Yhat <- softmax(Z2)
  
  return(which.max(Yhat))
  
}



rpsAI <- function(i = 10, model, random = F) {
  rules <- matrix(c(0, 1, -1,
                    -1, 0, 1,
                    1, -1, 0),
                  nrow = 3, byrow = T)
  translate <- c("Rock", "Paper", "Scissors")
  
  n_games <- 0
  
  log <- matrix(nrow = 0, ncol = 11) # log game's results
  
  win_stat <- NULL
  
  while(n_games < i) {
    human_play <- readline(prompt = "Enter your move (1 = Rock, 2 = Paper, 3 = Scissors, 0 = quit): ")
    if(human_play != "1" & human_play != "2" & human_play != "3") {
      print("Ending game")
      break
    } else {
      human_play <- as.numeric(human_play)
      
      
      if(n_games == 0) {
        last5 <- matrix(c(sample(1:3, 5, T), sample(-1:1, 5, T)), ncol = 10)
      } else {
        last5 <- matrix(c(log[nrow(log), c(2:5, 11, 7:10)], win_stat), ncol = 10)
      }
      
      calc <- rpsPredictor(hist = last5, model = model)
      comp_play <- ifelse(random == F, ifelse(calc == 1, 2, ifelse(calc == 2, 3, 1)), sample(c(1:3), 1))
      
      win_stat <- rules[comp_play, human_play]
      response <- ifelse(win_stat == 1, "Winner!", ifelse(win_stat == 0, "Tie!", "Loser!"))
      cat("\nYou:", translate[human_play], "\nMe:", translate[comp_play], "\n\n", response)
      n_games <- n_games + 1
      
      log <- rbind(log, c(last5[1,1:10], human_play))
      
    }
  }
  
  if(n_games > 5) {
    log_adjusted <- log[6:nrow(log),]
    return(log_adjusted)
  }
  
}
