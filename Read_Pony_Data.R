library(lubridate)
library(dplyr)
library(googlesheets4)

# Set to TRUE only if regenerating all data from scratch. 
from_scratch = FALSE
vs_results_URL = "https://docs.google.com/spreadsheets/d/1S5FvX5Z3jjvivywdzrviyXLKSfKxkoD-siB6rpyCsp8/"
result_sheets = c("(2025) Matches")

if (file.exists("match_level_data.csv")){
  match_level_data=read.csv("match_level_data.csv")
  match_level_data$MatchId = as.double(match_level_data$MatchId)
  match_level_data$MatchDate=ymd(match_level_data$MatchDate)
  match_idx = max(match_level_data$MatchId) + 1
} else {
  if (from_scratch){
    match_level_data = data.frame(MatchId = integer(),
                                  MatchDate = Date(),
                                  Status = character())   
    match_idx = 1
  } else {
    stop("File match_level_data is expected, but does not exist")
  }
}

if (file.exists("player_results.csv")){
  player_results = read.csv("player_results.csv")
} else {
  if (from_scratch){
    player_results = data.frame(MatchId = integer(),
                                Player = character(),
                                RacePoints = integer(),
                                PlacePoints = integer(),
                                PenaltyPoints = integer(),
                                Total = integer())   
  } else {
    stop("File player_results.csv is expected, but does not exist")
  }
}

create_match_df = function(match_data){
  players = match_data[,1]
  race_points = match_data[,2]
  place_points = 4 * (rank(race_points) - 1) # 3 for 1st, 2 for 2nd, etc.
  penalty_points = rep(0, 4)
  total_points = race_points + place_points + penalty_points
  
  match_df = data.frame(players,
                        race_points,
                        place_points,
                        penalty_points,
                        total_points)
  colnames(match_df) = c("Player", "RacePoints", "PlacePoints", "PenaltyPoints", "Total")
  return(match_df)
}

is_match_known = function(match_df, match_date, player_results){
  potential_duplicate_count = match_df %>%
                                inner_join(player_results, join_by(Player, RacePoints)) %>%  
                                group_by(MatchId) %>%
                                count() %>%
                                filter(n == 4) %>%
                                nrow()
    
  is_potential_duplicate = potential_duplicate_count > 0
  return(is_potential_duplicate)
}

aliases = list()
aliases["Grey"] = "grey"
aliases["Nico"] = "Nicocrack"
aliases["Justase"] = "justase"

for (result_sheet in result_sheets){
  match_results = read_sheet(vs_results_URL, 
                           result_sheet,
                           col_names=FALSE)
  for(alias_idx in c(1:length(aliases))){
    alias_list = aliases[alias_idx]
    actual_name = names(alias_list)
    for (alias in alias_list){
      match_results[match_results == alias] = actual_name
    }
  }
  cols_per_match = 3
  rows_per_match = 6
  match_cols = (ncol(match_results) +1) / cols_per_match
  match_rows = (nrow(match_results) + 1) / rows_per_match
  race_points_per_match = 6 * 16
  
  for (j in 1:match_rows){
    row_idx = rows_per_match * (j - 1) + 2
    for (i in 1:match_cols){
      col_idx = cols_per_match * (i - 1) + 1
      if (!(is.na(match_results[row_idx, col_idx]))){
        match_data_raw = match_results[row_idx:(row_idx + 3),col_idx:(col_idx + 1)]
        match_df = create_match_df(match_data_raw)
        match_date = mdy(match_results[row_idx - 1, col_idx][[1]])
        if (!is_match_known(match_df, match_date, player_results) ){
          match_level_row_idx = nrow(match_level_data) + 1
          match_level_data[match_level_row_idx,] = list(match_idx, match_date, "Loading")
          match_df$MatchId = match_idx
          player_results = rbind(player_results, match_df)
          if (sum(match_df$RacePoints) == race_points_per_match){
            match_level_data[match_level_row_idx,]$Status = "Loaded"
          } else {
            match_level_data[match_level_row_idx,]$Status = "Invalid"
          }
          match_idx = match_idx + 1
        }
      }
    }
  }
}

player_names = sort(unique(player_results$Player))
lowercase_player_names = tolower(player_names)
if (any(duplicated(lowercase_player_names))){
  duplicated_names = lowercase_player_names[which(duplicated(lowercase_player_names))]
  dup_name_string = paste(player_names[which(tolower(player_names) %in% duplicated_names)], 
                          collapse = ", ")
  stop(paste("The following names appear to be duplicates: ", dup_name_string))
  
}

write.csv(match_level_data, "match_level_data.csv", row.names = FALSE)
write.csv(player_results, "player_results.csv", row.names = FALSE)
