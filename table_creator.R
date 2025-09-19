library(tidyr)
library(kableExtra)
library(stringr)

date_new = today()
date_old = today() - 30

rankings_new = vs_ratings$get_players_data(unique(vs_ratings$player_results$Player), 
                                         date_new) %>%
  mutate(Rating = compute_rating(Elo, RD)) %>%
  filter(!is.na(Rating)) %>%
  filter(Player %in% active_players) %>%
  mutate(across(where(is.numeric), round)) %>%
  relocate(Rating, .before = Elo) %>%
  arrange(desc(Rating)) %>%
  mutate(Rank = row_number())

rankings_old = vs_ratings$get_players_data(unique(vs_ratings$player_results$Player), 
                                           date_old) %>%
  mutate(Rating = compute_rating(Elo, RD)) %>%
  filter(!is.na(Rating)) %>%
  filter(Player %in% active_players) %>%
  mutate(across(where(is.numeric), round)) %>%
  relocate(Rating, .before = Elo) %>%
  arrange(desc(Rating)) %>%
  mutate(Rank = row_number())

rankings_old = rankings_old %>%
  rename_with(function(x) paste0(x, "Before"), -where(is.character))

standings_new = rankings_new %>%
  left_join(rankings_old, join_by(Player)) %>%
  mutate(RatingChange = Rating - RatingBefore) %>%
  mutate(RankChange = RankBefore - Rank) %>%
  select(Rank, Player, Rating, RankChange, RatingChange) %>%
  mutate(RankChange = sprintf("%+d", RankChange)) %>%
  mutate(RatingChange = sprintf("%+d", RatingChange)) %>%
  mutate(across(everything(), ~replace(., . ==  "+0" , "="))) %>%
  mutate(across(everything(),~ gsub("\\+","↑", .))) %>%
  mutate(across(everything(),~ gsub("\\-","↓", .))) %>%
  mutate(across(everything(), ~replace(., . ==  "NA" , "-")))

  
standings_new %>%
  mutate(Rank = str_pad(Rank, max(nchar(Rank)), pad = "\u00A0")) %>%
  mutate(RankChange = ifelse(RankChange == "-",
                             strrep("\u00A0", max(nchar(RankChange))+2),
                             str_pad(paste0("(", RankChange, ")"), max(nchar(RankChange) + 2), pad = "\u00A0"))) %>%
  mutate(Rank = paste(Rank, RankChange)) %>%
  mutate(Rating = str_pad(Rating, max(nchar(Rating)), pad = "\u00A0")) %>%
  mutate(RatingChange = ifelse(RatingChange == "-",
                               strrep(" ", max(nchar(RatingChange))+2),
                               str_pad(paste0("(", RatingChange, ")"), max(nchar(RatingChange) + 2), pad = "\u00A0"))) %>%
  mutate(Rating = paste(Rating, RatingChange)) %>%
  select(Rank, Player, Rating) %>%
  kable()
  