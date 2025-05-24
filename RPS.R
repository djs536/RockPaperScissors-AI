### Step 1: Download and install required files

# AI model object
download.file("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/rps-model.rds",
              destfile = "rps-model.rds")
rpsModel <- readRDS("rps-model.rds")

# Run RPS script with AI and non-AI play functions
source("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/rps-game-ai.R")


### Step 2: Define storage object for new training data
if (file.exists("playLog.rds")) {
  playLog <- readRDS("playLog.rds")
} else {
  playLog <- matrix(nrow = 0, ncol = 11)
}

### Step 3: Define wrapper function to continually append playLog upon exit
play <- function(games = 100, x = playLog) {
  round1 <- rpsAI(i = games, model = rpsModel)
  Sys.sleep(2)
  addAffirm <- readline(prompt = "Use games as training data? (Type y or n)")
  addAffirm <- ifelse(addAffirm == "n", 0, 1)
  print(addAffirm)
  if(addAffirm == 1) {
    playLog <<- rbind(playLog, round1)
    saveRDS(playLog, "playLog.rds")
  }
  
  nLoss <- as.vector(table(round1[,10])[1]) / nrow(round1)
  nTie <- as.vector(table(round1[,10])[2]) / nrow(round1)
  nWin <- as.vector(table(round1[,10])[3]) / nrow(round1)
  
  cat("\nWin Percentage:", nWin, "\nTie Percentage:", nTie, "\nLoss Percentage:", nLoss)
  
}

