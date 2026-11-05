library(wehoop)
library(readr)
library(dplyr)

download_all_wnba_seasons <- function(output_dir = "input/data", 
                                      start_season = 1997, 
                                      end_season = wehoop::most_recent_wnba_season()) {
  
  # Create destination directory if it doesn't exist
  dir.create(output_dir, recursive = TRUE, show.warnings = FALSE)
  
  seasons <- start_season:end_season
  message(sprintf("Starting download for WNBA seasons %d through %d...", start_season, end_season))
  
  for (s in seasons) {
    message(sprintf("Processing Seasons", s))
    
    # 1. Download & Save Play-by-Play Data
    pbp_file <- file.path(output_dir, sprintf("wnba_pbp_%d.csv", s))
    tryCatch({
      pbp_data <- wehoop::load_wnba_pbp(seasons = s)
      if (nrow(pbp_data) > 0) {
        readr::write_csv(pbp_data, pbp_file)
        message(sprintf(" Saved PBP: %s (%d rows)", pbp_file, nrow(pbp_data)))
      } else {
        message(sprintf("PBP empty for season %d", s))
      }
    }, error = function(e) {
      message(sprintf("Failed to load PBP for %d: %s", s, e$message))
    })
    
    # 2. Download & Save Player Box Score Data
    box_file <- file.path(output_dir, sprintf("wnba_player_box_%d.csv", s))
    tryCatch({
      box_data <- wehoop::load_wnba_player_box(seasons = s)
      if (nrow(box_data) > 0) {
        readr::write_csv(box_data, box_file)
        message(sprintf("Saved Player Box: %s (%d rows)", box_file, nrow(box_data)))
      } else {
        message(sprintf("Player Box empty for season %d", s))
      }
    }, error = function(e) {
      message(sprintf("Failed to load Player Box for %d: %s", s, e$message))
    })
  }
  
  message("\nFinished downloading all available WNBA seasons.")
}