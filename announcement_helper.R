first_day_month = ymd("2026-03-01")
last_day_month = ymd("2026-03-31")

vs_ratings$player_results %>%
  left_join(vs_ratings$match_level_data) %>%
  select(Player, MatchDate) %>%
  group_by(Player) %>%
  filter(MatchDate == min(MatchDate)) %>%
  filter(MatchDate >= first_day_month, MatchDate <= last_day_month)

played_matches = vs_ratings$match_level_data %>%
  filter(MatchDate >= first_day_month, MatchDate <= last_day_month)

played_matches %>%
  nrow()

get_leaderboard_players = function(last_date){
  active_players = vs_ratings$player_results %>%
    left_join(vs_ratings$match_level_data,
              join_by(MatchId)) %>%
    filter(MatchDate >= last_date - 60) %>%
    filter(Status %in% c("Processed", "Reported")) %>%
    group_by(Player) %>%
    arrange(desc(MatchDate)) %>%
    slice(5) %>%
    ungroup() %>%
    select(Player) %>%
    unlist()
  
  ratings_df = vs_ratings$get_players_data(unique(vs_ratings$player_results$Player), 
                                           last_match_date) %>%
    mutate(Rating = compute_rating(Elo, RD)) %>%
    filter(!is.na(Rating)) %>%
    filter(Player %in% active_players) %>%
    mutate(across(where(is.numeric), round)) %>%
    relocate(Rating, .before = Elo) %>%
    arrange(desc(Rating))
  
  leaderboard_players = sort(ratings_df$Player)
  return(leaderboard_players)
}

leaderboard_players_current = get_leaderboard_players(last_day_month)
leaderboard_players_prev = get_leaderboard_players(first_day_month - 1)
setdiff(leaderboard_players_current, leaderboard_players_prev) # new players
setdiff(leaderboard_players_prev, leaderboard_players_current) # dropped players


new_years_ratings = vs_ratings$get_players_data(unique(vs_ratings$player_results$Player), 
                            ymd("2026-01-01")) %>% filter(RD <= 125)

current_ratings = vs_ratings$get_players_data(unique(vs_ratings$player_results$Player), 
                                                     ymd("2026-03-31")) %>% filter(RD <= 125)

current_ratings %>% inner_join(new_years_ratings, join_by(Player)) %>%
    mutate(elo_diff = Elo.x - Elo.y) %>% arrange(desc(elo_diff))
