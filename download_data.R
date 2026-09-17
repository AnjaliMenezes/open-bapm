library(wehoop)
library(readr)

wnba_pbp <- wehoop::load_wnba_pbp(season=2025)                                                            
wnba_player_box <- wehoop::load_wnba_player_box(seasons=2025)

readr::write_csv(wnba_pbp, "input/data/wnba_pbp_2025.csv")
readr::write_csv(wnba_player_box, "input/data/wnba_player_box_2025.csv")
