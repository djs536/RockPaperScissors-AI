### STEP 1: Create RPS Game that includes human input

rps <- function(i = 1000) {
  rules <- matrix(c(0, 1, -1,
                    -1, 0, 1,
                    1, -1, 0),
                  nrow = 3, byrow = T)
  translate <- c("Rock", "Paper", "Scissors")
  
  n_games <- 0
  outcomes <- matrix(nrow = 2, ncol = 0)
  
  while(n_games < i) {
    human_play <- readline(prompt = "Enter your move (1 = Rock, 2 = Paper, 3 = Scissors, 0 = quit): ")
    if(human_play != "1" & human_play != "2" & human_play != "3") {
      print("Ending game")
      break
    } else {
      human_play <- as.numeric(human_play)
      comp_play <- sample(c(1:3), 1)
      win_stat <- rules[comp_play, human_play]
      response <- ifelse(win_stat == 1, "Winner!", ifelse(win_stat == 0, "Tie!", "Loser!"))
      cat("\nYou:", translate[human_play], "\nMe:", translate[comp_play], "\n\n", response)
      n_games <- n_games + 1
      outcomes <- cbind(outcomes, c(human_play, win_stat))
    }
  }
  
  if(n_games > 0) {
    return(outcomes)
  }
}

### NOTE: Use game record naming protocol: "series1", "series2", ... "seriesx"
### BACKUP INCLDUES ALL OUTCOMES. lengths: 132, 100, 100, 100, 100, 100...




