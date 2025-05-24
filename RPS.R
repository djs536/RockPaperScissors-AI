### Step 1: Download and install required files

# AI model object
download.file("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/rps-model.rds",
              destfile = "rps-model.rds")
rpsModel <- readRDS("rps-model.rds")

# Run RPS script with AI and non-AI play functions
source("https://github.com/djs536/RockPaperScissors-AI/raw/refs/heads/main/rps-game-ai.R")


### Step 2: Play game
round1 <- rpsAI(100, model = rpsModel) 

