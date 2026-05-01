update_player_name = function(old_name, new_name, vs_ratings){
  if (!any(vs_ratings$player_results$Player == old_name)){
    warning(paste("No player named", old_name, "found. No changes made."))
    return()
  }
  vs_ratings$player_results$Player[vs_ratings$player_results$Player == old_name] = new_name
  vs_ratings$update_history$Player[vs_ratings$update_history$Player == old_name] = new_name
  registered_players = read.delim("RegisteredPlayers.txt", header = FALSE)
  registered_players[registered_players == old_name] = new_name
  write.table(registered_players, "RegisteredPlayers.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
