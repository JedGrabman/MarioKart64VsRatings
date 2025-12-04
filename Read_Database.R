library(lubridate)

# Set to TRUE only if regenerating all data from scratch. 
from_scratch = FALSE

if (file.exists("match_level_data.csv")){
  match_level_data=read.csv("match_level_data.csv")
  match_level_data$MatchId = as.integer(match_level_data$MatchId)
  match_level_data$MatchDate=ymd(match_level_data$MatchDate)
  match_idx = max(match_level_data$MatchId) + 1
} else {
  if (!from_scratch){
    stop("File match_level_data is expected, but does not exist")
  }
}

if (file.exists("player_results.csv")){
  player_results = read.csv("player_results.csv")
} else {
  if (!from_scratch){
    stop("File player_results.csv is expected, but does not exist")
  }
}

if (file.exists("update_history.csv")){
  update_history = read.csv("update_history.csv")
  update_history$MatchDate = as.Date(update_history$MatchDate)
}

if (file.exists("RegisteredPlayers.txt")){
  registered_players = read.delim("RegisteredPlayers.txt",header=FALSE,sep="\n")[,1]
} else {
  registered_players = character()
}
