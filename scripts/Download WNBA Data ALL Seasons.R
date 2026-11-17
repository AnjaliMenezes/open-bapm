library(wehoop)
library(readr)

output_dir <- "input/data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# wehoop's play-by-play and player box data start in 2002
seasons <- 2002:wehoop::most_recent_wnba_season()

for (s in seasons) {
  message("Season ", s)
  
  # Play-by-play
  tryCatch({
    pbp <- wehoop::load_wnba_pbp(seasons = s)
    if (nrow(pbp) > 0) {
      readr::write_csv(pbp, file.path(output_dir, sprintf("wnba_pbp_%d.csv", s)))
      message("  pbp: ", nrow(pbp), " rows")
    } else {
      message("  pbp: empty")
    }
  }, error = function(e) message("  pbp failed: ", conditionMessage(e)))
  
  # Player box scores
  tryCatch({
    box <- wehoop::load_wnba_player_box(seasons = s)
    if (nrow(box) > 0) {
      readr::write_csv(box, file.path(output_dir, sprintf("wnba_player_box_%d.csv", s)))
      message("  player box: ", nrow(box), " rows")
    } else {
      message("  player box: empty")
    }
  }, error = function(e) message("  player box failed: ", conditionMessage(e)))
}

# Pre-2002 player box scores are not in wehoop
for (s in 1997:2001) {
  tryCatch({
    gl <- wehoop::wnba_leaguegamelog(season = s, player_or_team = "P")$LeagueGameLog
    readr::write_csv(gl, file.path(output_dir, sprintf("wnba_stats_player_box_%d.csv", s)))
    message("  stats player box ", s, ": ", nrow(gl), " rows")
  }, error = function(e) message("  stats player box ", s, " failed: ", conditionMessage(e)))
  Sys.sleep(3)
}

message("Done")