get_player_data = function(player, as_of_date, update_history, c_val = 0.5, match_id = Inf){
  if (player %in% update_history$Player){
    player_data = update_history %>%
      filter(MatchDate < as_of_date | 
               ((MatchDate == as_of_date) & (MatchId < match_id))) %>%
      filter(Player == player) 
    if (nrow(player_data) > 0){
      player_data = player_data %>%
        filter(MatchDate == max(MatchDate)) %>%
        filter(MatchId == max(MatchId))
        date_delta = as.double(as_of_date - player_data$MatchDate)
        player_data$RD = min(player_data$RD + c_val * date_delta, 300)
        player_data = player_data %>%
                        select(Player, Elo, RD)
        return(player_data)
    }
  }
  player_data = data.frame(Player = player,
                  Elo = 1000,
                  RD = 300)
  return(player_data)
}

get_players_data = function(players, as_of_date, update_history, c_val = 0.5, match_id = Inf){
  players_data = lapply(players, function(player) get_player_data(player, as_of_date, update_history, c_val, match_id))
  players_data_df = bind_rows(players_data)
  return(players_data_df)
}

calculate_new_status = function(match_and_status_df){
  status = data.frame(Player = as.character(),
                      Rating = as.double(),
                      Deviation = as.double())
  match_data = data.frame(period = as.numeric(),
                          player_1 = as.character(),
                          player_2 = as.character(),
                          score = as.double())
  possible_points_per_opponent = 20
  total_possible_points = 3 * possible_points_per_opponent
  for(player_idx in c(1:nrow(match_and_status_df))){
    player_df = match_and_status_df[player_idx,]
    player = player_df$Player
    opponent_df = match_and_status_df[-player_idx,] 
    player_expected_points = sum(possible_points_per_opponent  / (1 + 10**(-(player_df$Elo - opponent_df$Elo) / 400))) 
    player_expected_score = player_expected_points / (3 * possible_points_per_opponent)
    dummy_rating = player_df$Elo + 400 * log((1 / player_expected_score) - 1, 10)
    dummy_deviation = mean(opponent_df$RD**2)**0.5  
    dummy_name = paste0("Dummy_", player)
    status[(2 * player_idx) - 1,] = list(player, player_df$Elo, player_df$RD)
    status[2 * player_idx,] = list(dummy_name, dummy_rating, dummy_deviation)
    match_data[player_idx,] = list(1, player, dummy_name, player_df$Total / (3 * possible_points_per_opponent))
  }
  status_new = glicko(match_data, status)$rating %>%
    filter(substr(Player, 0, 6) != "Dummy_") %>%
    select(Player, Rating, Deviation) %>%
    rename(Elo = Rating) %>%
    rename(RD = Deviation)
  return(status_new)
}

if (file.exists("match_level_data.csv")){
  match_level_data=read.csv("match_level_data.csv")
  match_level_data$MatchId = as.double(match_level_data$MatchId)
  match_level_data$MatchDate=ymd(match_level_data$MatchDate)
  match_idx = max(match_level_data$MatchId) + 1
} else {
  stop("match_level_data.csv does not exist")
}

if (file.exists("player_results.csv")){
  player_results = read.csv("player_results.csv")
} else {
  stop("player_results.csv does not exist")
}

if (file.exists("update_history.csv")){
  update_history = read.csv("update_history.csv")
  update_history$MatchDate = as.Date(update_history$MatchDate)
} else {
  update_history = data.frame(Date = Date(),
                              MatchId = integer(),
                              Player = character(), 
                              Elo = double(), 
                              RD = double())
}


# If we are processing a match that is older than a previously discovered match
# we reprocess matches that came after it due to changed ratings.
# Technically, we only need to reprocess matches including players whose ratings
# were affected (and then those that player with players with changed ratings,
# etc), but its easier to just do all of them.
first_match = match_level_data %>%
                filter(Status == "Loaded") %>%
                arrange(MatchDate, MatchId) %>%
                slice_head(n = 1)

match_ids_to_process = match_level_data %>%
                          filter(MatchDate >= first_match$MatchDate | 
                                 (MatchDate == first_match$MatchDate) & MatchId >= first_match$MatchId) %>%
                          filter(Status != "Invalid") %>%
                          arrange(MatchDate, MatchId) %>%
                          select(MatchId) %>%
                          unlist()

match_level_data[match_level_data$MatchId %in% match_ids_to_process,]$Status = "Loaded"
update_history = update_history %>%
                    filter(!(MatchId %in% match_ids_to_process))


for (match_id in match_ids_to_process){
  match_level_data[match_level_data$MatchId == match_id,]$Status = "Processing"
  match_df = player_results %>%
    filter(MatchId == match_id) %>%
    select(Player, Total)
  match_date = (match_level_data %>%
                  filter(MatchId == match_id) %>%
                  select(MatchDate))[1,]
  
  player_status_df = get_players_data(match_df$Player, match_date, update_history, match_id = match_id)
  
  match_and_status_df = match_df %>%
    inner_join(player_status_df, join_by(Player)) %>%
    select(Player, Total, Elo, RD)
  status_new = calculate_new_status(match_and_status_df)
  status_updates = cbind(MatchId = match_id, MatchDate = match_date, status_new)
  update_history = rbind(update_history, status_updates) 
  match_level_data[match_level_data$MatchId == match_id,]$Status = "Processed"
}

write.csv(update_history, "update_history.csv", row.names = FALSE)
write.csv(player_results, "player_results.csv", row.names = FALSE)
write.csv(match_level_data, "match_level_data.csv", row.names = FALSE)
