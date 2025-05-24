### Step 1: Download and install required files

# AI model object
download.file("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/rps-model.rds",
              destfile = "rps-model.rds")
rpsModel <- readRDS("rps-model.rds")

if (file.exists("trainDataAI.csv")) {
  trainDataAI <- as.matrix(read.csv("trainDataAI.csv")[,2:12])
} else {
  trainDataAI <- as.matrix(read.csv("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/trainDataAI.csv")[,2:12])
}

colnames(trainDataAI) <- NULL

# Run RPS script with AI and non-AI play functions
source("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/rps-game-ai.R")


### Step 2: Define wrapper function to continually append trainDataAI upon exit
play <- function(games = 1000) {
  round1 <- rpsAI(i = games, model = rpsModel)
  Sys.sleep(2)
  addAffirm <- readline(prompt = "Use games as training data? (Type y or n)")
  addAffirm <- ifelse(addAffirm == "n", 0, 1)

  if(addAffirm == 1) {
    trainDataAI <<- rbind(trainDataAI, round1)
    write.csv(trainDataAI, "trainDataAI.csv")
  }
  
  nLoss <- as.vector(table(round1[,10])[1]) / nrow(round1)
  nTie <- as.vector(table(round1[,10])[2]) / nrow(round1)
  nWin <- as.vector(table(round1[,10])[3]) / nrow(round1)
  
  cat("\nWin Percentage:", nWin, "\nTie Percentage:", nTie, "\nLoss Percentage:", nLoss)
  
}

### Step 3: Set up high-play round
play(1000)

