library(googlesheets4)

ratings_sheet_URL = "https://docs.google.com/spreadsheets/d/1kF4QJiV2VrxIJC-6ciaQcC1CExTJ5KlRo3MztaN4W_Y/"
existing_sheets = sheet_properties(ratings_sheet_URL)$name
unreported_months = vs_ratings$match_level_data %>%
                      filter(Status == "Processed") %>%
                      mutate(Month = month(MatchDate), Year = year(MatchDate)) %>%
                      distinct(Year, Month) %>%
                      arrange(Year, Month)

RD_cutoff_placement = 125

compute_rating = function(elo, RD, stable_rd = 65, max_rd = 125){
  results = rep(NA, length(elo))
  results[RD < stable_rd] = elo[RD < stable_rd]
  results[RD >= stable_rd] = elo[RD >= stable_rd] - 2 * (RD[RD >= stable_rd] - stable_rd)
  results[RD > max_rd] = NA
  return(results)
}


for (month_idx in c(1:nrow(unreported_months))){
  month_info = unreported_months[month_idx,]
  month_abb = month.abb[month_info$Month]
  month_sheet_name = paste(month_abb, (month_info$Year) %% 100, sep = "-")
  if (month_sheet_name %in% existing_sheets){
    sheet_delete(ratings_sheet_URL, month_sheet_name)
  }
  sheet_copy(ratings_sheet_URL, from_sheet = "Template", to_sheet = month_sheet_name)
  
  match_ids_to_report = vs_ratings$match_level_data %>%
    filter(Status %in% c("Processed", "Reported")) %>%
    arrange(MatchDate, MatchId) %>%
    mutate(Month = month(MatchDate), Year = year(MatchDate)) %>%
    filter(Month == month_info$Month, Year == month_info$Year) %>%
    select(MatchId) %>%
    unlist() %>%
    rev()
  reporting_df = data.frame(matrix(ncol = 7, nrow = 0))
  for (match_id in match_ids_to_report){
    match_df = vs_ratings$player_results %>%
      filter(MatchId == match_id) %>%
      select(Player, RacePoints, PlacePoints, Total) %>%
      arrange(desc(Total))
    players = match_df$Player
    match_date = (vs_ratings$match_level_data %>% 
                    filter(MatchId == match_id) %>% 
                    select(MatchDate))[1,]
    status_match_before = vs_ratings$get_players_data(players, 
                                           match_date, 
                                           match_id = match_id)
    status_match_before = status_match_before %>%
      mutate(Rating = compute_rating(Elo, RD)) %>%
      mutate(across(where(is.numeric), round)) %>%
      rename_with(function(x) paste0(x, "Before"), -where(is.character))
    status_match_after = vs_ratings$get_players_data(players, 
                                          match_date, 
                                          match_id = match_id + 0.1)
    status_match_after = status_match_after %>%
      mutate(Rating = compute_rating(Elo, RD)) %>%
      mutate(across(where(is.numeric), round)) %>%
      rename_with(function(x) paste0(x, "After"), -where(is.character))
    summary_table = match_df %>%
      left_join(status_match_before, join_by(Player)) %>%
      left_join(status_match_after, join_by(Player)) %>% 
      select(Player, RacePoints, PlacePoints, Total, RatingBefore, RatingAfter) %>%
      mutate(RatingChange = RatingAfter - RatingBefore) %>%
      relocate(RatingChange, .after = RatingBefore)
    
    summary_table = summary_table %>%
      mutate(RatingChange = ifelse(is.na(RatingBefore) & !is.na(RatingAfter), "New Rating!", RatingChange))
    summary_table[is.na(summary_table)] = "-"
    summary_table = summary_table %>% 
      rename("Race\nPoints" = RacePoints) %>%
      rename("Place\nPoints" = PlacePoints) %>%
      rename("Rating\nBefore" = RatingBefore) %>%
      rename("Rating\nChange" = RatingChange) %>%
      rename("Rating\nAfter" = RatingAfter)
    
    reporting_df[nrow(reporting_df) + 1, 1:2] = c(paste0("Match ID: ", match_id), as.character(match_date))
    reporting_df[nrow(reporting_df) + 1,] = colnames(summary_table)
    reporting_df[(nrow(reporting_df) + 1):(nrow(reporting_df) + 4), ] = summary_table
    reporting_df[nrow(reporting_df) + 1,] = ""
  }
  reporting_df$X0 = ""
  reporting_df = reporting_df %>%
    relocate(X0)
  
  range_write(ratings_sheet_URL, 
              reporting_df, 
              month_sheet_name, 
              col_names = FALSE, 
              range = "A1", 
              reformat = FALSE)
  sheet_resize(ratings_sheet_URL, 
               month_sheet_name, 
               nrow(reporting_df), 
               ncol(reporting_df), 
               exact = TRUE)
  vs_ratings$match_level_data$Status[vs_ratings$match_level_data$MatchId %in% match_ids_to_report] = "Reported"
}


last_match_date = max(vs_ratings$match_level_data$MatchDate)
# Must have played 5 games in last 60 days
active_players = vs_ratings$player_results %>%
                  left_join(vs_ratings$match_level_data,
                            join_by(MatchId)) %>%
                  filter(MatchDate >= last_match_date - 60) %>%
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

sheet_write(ratings_df, ratings_sheet_URL, "Standings")
write.csv(vs_ratings$match_level_data, "match_level_data.csv", row.names = FALSE)
