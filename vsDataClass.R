library(PlayerRatings)
library(dplyr)

vsData = setRefClass("vsData",
                      fields = list(match_level_data = "data.frame",
                      player_results = "data.frame",
                      update_history = "data.frame",
                      max_match_id = "integer",
                      registered_players = "character",
                      aliases = "list"),
                     methods = list(
                       initialize = function(match_level_data = NULL, 
                                             player_results = NULL, 
                                             update_history = NULL,
                                             registered_players = NULL){
                         .self$aliases = list("Grey" = "grey",
                                              "Justase" = "justase",
                                              "Pony" = "MrPonytale",
                                              "Elsuper100" = "ElSuper100",
                                              "Mauro" = "Ratauro")
                         if (is.null(match_level_data)){
                           .self$match_level_data = data.frame(MatchId = integer(),
                                                               MatchDate = Date(),
                                                               Status = character())
                           .self$max_match_id = 0L
                         } else {
                           .self$match_level_data = match_level_data
                           .self$max_match_id = max(match_level_data$MatchId)
                         }
                         if (is.null(player_results)){
                           .self$player_results = data.frame(MatchId = integer(),
                                                             Player = character(),
                                                             RacePoints = integer(),
                                                             PlacePoints = integer(),
                                                             PenaltyPoints = integer(),
                                                             Total = integer()) 
                         } else {
                           .self$player_results = player_results
                         }
                         if (is.null(update_history)){
                           .self$update_history = data.frame(Date = Date(),
                                                             MatchId = integer(),
                                                             Player = character(), 
                                                             Elo = double(), 
                                                             RD = double())
                         } else {
                           .self$update_history = update_history
                         }
                         if (is.null(registered_players)){
                           .self$registered_players = character(0)
                         } else {
                           .self$registered_players = registered_players
                         }
                       },
                       create_match_df = function(players, race_points){
                         players = .self$dealias(players)
                         place_points = 4 * (rank(race_points) - 1) # 3 for 1st, 2 for 2nd, etc.
                         penalty_points = rep(0, 4)
                         total_points = race_points + place_points + penalty_points
                         match_id = .self$max_match_id + 1L
                         
                         match_df = data.frame(players,
                                               race_points,
                                               place_points,
                                               penalty_points,
                                               total_points,
                                               match_id)
                         colnames(match_df) = c("Player", "RacePoints", "PlacePoints", "PenaltyPoints", "Total", "MatchId")
                         return(match_df)
                       },
                       insert_new_match = function(players, race_points, match_date){
                         match_df = .self$create_match_df(players, race_points)
                         if (!is_match_known(match_df)){
                           .self$max_match_id = .self$max_match_id + 1L
                           match_level_row_idx = nrow(match_level_data) + 1
                           .self$match_level_data[match_level_row_idx,] = list(match_df$MatchId[1], match_date, "Loading")
                           if (sum(match_df$RacePoints) == RACE_POINTS_PER_MATCH & all(match_df$Player %in% .self$registered_players)){
                             match_level_data[match_level_row_idx,]$Status <<- "Loaded"
                           } else {
                             match_level_data[match_level_row_idx,]$Status <<- "Invalid"
                           }
                           .self$player_results = rbind(.self$player_results, match_df)
                         }
                       },
                       is_match_known = function(match_df){
                         potential_duplicate_count = match_df %>%
                           select(-MatchId) %>%
                           inner_join(.self$player_results, join_by(Player, RacePoints)) %>% 
                           group_by(MatchId) %>%
                           count() %>%
                           ungroup() %>%
                           filter(n == 4) %>%
                           nrow()
                         
                         is_potential_duplicate = potential_duplicate_count > 0
                         return(is_potential_duplicate)
                       },
                       dealias = function(players){
                         for (alias_list_idx in c(1:length(.self$aliases))){
                           alias_list = .self$aliases[alias_list_idx]
                           name_actual = names(alias_list)
                           for (alias in unlist(alias_list)){
                             players[players == alias] = name_actual
                           }
                         }
                         return(players)
                       },
                       get_player_data = function(player, as_of_date, c_val_sq = 10, match_id = Inf){
                         if (player %in% .self$update_history$Player){
                           player_data = .self$update_history %>%
                             filter(MatchDate < as_of_date | 
                                      ((MatchDate == as_of_date) & (MatchId < match_id))) %>%
                             filter(Player == player) 
                           if (nrow(player_data) > 0){
                             player_data = player_data %>%
                               filter(MatchDate == max(MatchDate)) %>%
                               filter(MatchId == max(MatchId))
                             date_delta = as.double(as_of_date - player_data$MatchDate)
                             player_data$RD = min(sqrt(player_data$RD**2 + ((c_val_sq) * date_delta)), 200)
                             player_data = player_data %>%
                               select(Player, Elo, RD)
                             return(player_data)
                           }
                         }
                         player_data = data.frame(Player = player,
                                                  Elo = 1000,
                                                  RD = 200)
                         return(player_data)
                       },
                       get_players_data = function(players, as_of_date, c_val_sq = 10, match_id = Inf){
                         players_data = lapply(players, function(player) .self$get_player_data(player, as_of_date, c_val_sq, match_id))
                         players_data_df = bind_rows(players_data)
                         return(players_data_df)
                       },
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
                         # TODO: try setting cval to 0 here. It might be artificially boosting RD with the default 15 currently.
                         status_new = glicko(match_data, status, cval = 0)$rating %>%
                           filter(substr(Player, 0, 6) != "Dummy_") %>%
                           select(Player, Rating, Deviation) %>%
                           rename(Elo = Rating) %>%
                           rename(RD = Deviation)
                         return(status_new)
                       },
                       process_matches = function(){
                         # If we are processing a match that is older than a previously discovered match
                         # we reprocess matches that came after it due to changed ratings.
                         # Technically, we only need to reprocess matches including players whose ratings
                         # were affected (and then those that player with players with changed ratings,
                         # etc), but its easier to just do all of them.
                         first_match = .self$match_level_data %>%
                           filter(Status == "Loaded") %>%
                           arrange(MatchDate, MatchId) %>%
                           slice_head(n = 1)
                         
                         if (nrow(first_match) == 1){
                           match_ids_to_process = .self$match_level_data %>%
                             filter(MatchDate >= first_match$MatchDate | 
                                      (MatchDate == first_match$MatchDate) & MatchId >= first_match$MatchId) %>%
                             filter(Status != "Invalid") %>%
                             arrange(MatchDate, MatchId) %>%
                             select(MatchId) %>%
                             unlist()
                           
                           .self$match_level_data[.self$match_level_data$MatchId %in% match_ids_to_process,]$Status <- "Loaded"
                           .self$update_history <- .self$update_history %>%
                                                      filter(!(MatchId %in% match_ids_to_process))
                           
                           for (match_id in match_ids_to_process){
                             .self$match_level_data[.self$match_level_data$MatchId == match_id,]$Status = "Processing"
                             match_df = .self$player_results %>%
                               filter(MatchId == match_id) %>%
                               select(Player, Total)
                             match_date = (.self$match_level_data %>%
                                             filter(MatchId == match_id) %>%
                                             select(MatchDate))[1,]
                             
                             player_status_df = get_players_data(match_df$Player, match_date, match_id = match_id)
                             
                             match_and_status_df = match_df %>%
                               inner_join(player_status_df, join_by(Player)) %>%
                               select(Player, Total, Elo, RD)
                             status_new = calculate_new_status(match_and_status_df)
                             status_updates = cbind(MatchId = match_id, MatchDate = match_date, status_new)
                             .self$update_history = rbind(.self$update_history, status_updates) 
                             .self$match_level_data[.self$match_level_data$MatchId == match_id,]$Status = "Processed"
                           }
                         }
                       }
                     ))
