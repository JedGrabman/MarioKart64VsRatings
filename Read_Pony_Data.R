source("vsDataClass.R")
source("read_Database.R")

library(lubridate)
library(dplyr)
library(googlesheets4)

RACE_POINTS_PER_MATCH = 6 * 16
vs_results_URL = "https://docs.google.com/spreadsheets/d/1S5FvX5Z3jjvivywdzrviyXLKSfKxkoD-siB6rpyCsp8/"
result_sheets = c("(2025) Matches")

if(exists("match_level_data")){
  vs_ratings = vsData(match_level_data, player_results, update_history)  
}


for (result_sheet in result_sheets){
  match_results = read_sheet(vs_results_URL, 
                           result_sheet,
                           col_names=FALSE)
  cols_per_match = 3
  rows_per_match = 6
  match_cols = (ncol(match_results) +1) / cols_per_match
  match_rows = (nrow(match_results) + 1) / rows_per_match
  
  for (j in 1:match_rows){
    row_idx = rows_per_match * (j - 1) + 2
    for (i in 1:match_cols){
      col_idx = cols_per_match * (i - 1) + 1
      if (!(is.na(match_results[row_idx, col_idx]))){
        match_data_raw = match_results[row_idx:(row_idx + 3),col_idx:(col_idx + 1)]
        players_match = match_data_raw[,1]
        race_points = match_data_raw[,2]
        match_date = mdy(match_results[row_idx - 1, col_idx][[1]])
        vs_ratings$insert_new_match(players_match, race_points, match_date)
      }
    }
  }
}

player_names = sort(unique(vs_ratings$player_results$Player))
lowercase_player_names = tolower(player_names)
if (any(duplicated(lowercase_player_names))){
  duplicated_names = lowercase_player_names[which(duplicated(lowercase_player_names))]
  dup_name_string = paste(player_names[which(tolower(player_names) %in% duplicated_names)], 
                          collapse = ", ")
  stop(paste("The following names appear to be duplicates: ", dup_name_string))
  
}

write.csv(vs_ratings$match_level_data, "match_level_data.csv", row.names = FALSE)
write.csv(vs_ratings$player_results, "player_results.csv", row.names = FALSE)
vs_ratings$process_matches()
write.csv(vs_ratings$update_history, "update_history.csv", row.names = FALSE)
