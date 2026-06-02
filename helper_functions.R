get_registered_players = function(){
  return(unlist(read.delim("RegisteredPlayers.txt", header = FALSE)))
}

update_player_name = function(old_name, new_name, vs_ratings){
  if (!any(vs_ratings$player_results$Player == old_name)){
    warning(paste("No player named", old_name, "found. No changes made."))
    return()
  }
  vs_ratings$player_results$Player[vs_ratings$player_results$Player == old_name] = new_name
  vs_ratings$update_history$Player[vs_ratings$update_history$Player == old_name] = new_name
  registered_players = get_registered_players()
  registered_players[registered_players == old_name] = new_name
  registered_players = sort(registered_players)
  write.table(registered_players, "RegisteredPlayers.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
  write.csv(vs_ratings$match_level_data, "match_level_data.csv", row.names = FALSE)
  write.csv(vs_ratings$player_results, "player_results.csv", row.names = FALSE)
}
