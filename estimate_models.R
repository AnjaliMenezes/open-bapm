library(glmnet)

wnba_pbp <- read.csv("input/data/wnba_pbp_2025.csv")
wnba_player_box <- read.csv("input/data/wnba_player_box_2025.csv")

# Identify starting players from box score 
starters <- wnba_player_box |>                                                               
  dplyr::filter(starter) |>                                                                       
  dplyr::mutate(                                                  
    score_diff_final = (1 - 2 * (home_away == "away")) * (team_score - opponent_team_score)       
  ) |>                                                                        
  dplyr::select(game_id, home_away, athlete_id, score_diff_final)  

# Extract substitutions from play-by-play data 
substitutions <- wnba_pbp |>                                                                      
  dplyr::filter(type_text == "Substitution") |>                                                   
  dplyr::mutate(                                                                                  
    time_start = 10 * (qtr - 1) + (9 - clock_minutes) + (60 - clock_seconds) / 60,                
    time_remaining = end_game_seconds_remaining / 60,                                             
    score_diff_start = home_score - away_score                                                    
  ) |>                                                                                            
  dplyr::select(game_id, time_start, time_remaining, score_diff_start, athlete_id_1, athlete_id_2)

# Create a dataframe to hold times when players come on the court (starting with the starters)    
player_on <- starters |>                                                                          
  dplyr::group_by(game_id, home_away) |>                                                          
  dplyr::mutate(index = 1:dplyr::n(), time_start = 0, score_diff_start = 0, time_remaining = NA) |>
  dplyr::ungroup()  

# Each time a substitution occurs, track the "lineup slot" into which the player enters  
for (i in 1:nrow(substitutions)) {                                                                
  
  new_player_on <- player_on |>                                                                   
    # Find the "lineup slot" based on the outgoing player                                         
    dplyr::filter(                                                                                
      game_id == substitutions$game_id[i],                                                        
      athlete_id == substitutions$athlete_id_2[i]            
    ) |>                                                                                          
    dplyr::slice(dplyr::n()) |>                                                                   
    # Replace the information based on the incoming player, current time and current score        
    dplyr::mutate(                                                                                
      athlete_id = substitutions$athlete_id_1[i],               
      time_start = substitutions$time_start[i],                                                   
      time_remaining = substitutions$time_remaining[i],                                           
      score_diff_start = substitutions$score_diff_start[i]                                        
    )                                                                                             
  
  player_on <- dplyr::bind_rows(player_on, new_player_on)                                         
}                                

# If multiple substitutions happen for the same "lineup slot" without the clock time changing,    
# keep only the last player to enter the game in that "lineup slot".                              
player_on_collapsed <- player_on |>                                                               
  dplyr::group_by(                                                                                
    game_id, time_start, time_remaining, score_diff_start, score_diff_final, home_away, index     
  ) |>                                                                                            
  dplyr::summarize(athlete_id = athlete_id[dplyr::n()], .groups = "drop")                         

# Pivot wider to spread lineup slots across the columns instead of being gathered into rows       
lineups <- player_on_collapsed |>                                                                 
  dplyr::mutate(name = glue::glue("{home_away}_{index}")) |>                                      
  dplyr::select(-home_away, -index) |>                                                            
  tidyr::pivot_wider(names_from = name, values_from = athlete_id) |>                              
  # Once a player enters a lineup slot, they stay in it until someone replaces them               
  tidyr::fill(dplyr::matches("^away|^home"), .direction = "down")                                 

# Create a dataframe of athlete data                                                            
athlete <- wnba_player_box |>                                                                     
  dplyr::count(athlete_id, athlete_display_name, team_abbreviation) |>                            
  # Make sure we only have one row per athlete_id                                                 
  dplyr::group_by(athlete_id) |>                                                                  
  dplyr::arrange(-n) |>                                                                           
  dplyr::slice(1) |>                                                                              
  dplyr::select(-n) |>                                                                            
  dplyr::ungroup()

head(lineups)                                                                                
head(athlete) 

data <- lineups |>                                                                                
  dplyr::group_by(game_id) |>                                                                     
  dplyr::mutate(                                                                                  
    minutes = dplyr::coalesce(dplyr::lead(time_start, 1) - time_start, time_remaining),           
    score_diff = dplyr::coalesce(dplyr::lead(score_diff_start, 1), score_diff_final) - score_diff_start,
  ) |>                                                                                            
  dplyr::ungroup() |>                                                                             
  dplyr::filter(minutes > 0)

data_long <- data |>                                                                            
  dplyr::mutate(stint = 1:dplyr::n()) |>             
  dplyr::select(game_id, stint, minutes, score_diff, dplyr::matches("^away|^home")) |>            
  tidyr::pivot_longer(cols = c(dplyr::matches("^away|^home")), values_to = "athlete_id")          
  
head(data_long)

#total cumulative plus minus
athlete_summary <- data_long |>                                                                 
  dplyr::group_by(athlete_id) |>                                                                  
  dplyr::summarize(                                                                               
    minutes = sum(minutes),                                                                       
    plus_minus = sum((1 - 2 * grepl("away", name)) * score_diff),                                 
    .groups = "drop"                                                                              
  ) 

X_data <- data_long |>            
  dplyr::mutate(                                                                                  
    row = stint,                                                                                  
    column = as.numeric(as.factor(athlete_id)),            
    value = ifelse(substring(name, 1, 4) == "home", 1, -1)                                    
  ) 
x <- Matrix::sparseMatrix(                                                                  
  i = X_data$row,                                                                              
  j = X_data$column,                                                                         
  x = X_data$value                                                                              
)                                                                                         
y <- data$score_diff / data$minutes                                                
w <- data$minutes   
                                                                                                

#model rapm
model_rapm <- glmnet::cv.glmnet(                                                                  
  x = x,                                                                                          
  y = y,                                                                                          
  weights = w,                                                                                    
  alpha = 0,                                                                                      
  standardize = FALSE                                                                             
) 

athlete_coef_rapm <- X_data |>                                                                    
  dplyr::distinct(column, athlete_id) |>                                                          
  dplyr::arrange(column) |>                                                                       
  dplyr::mutate(                                              
    coef = coef(model_rapm, s = "lambda.min")[-1, 1]                                              
  ) |>                                                                                            
  dplyr::select(athlete_id, coef)                                                                 

athlete_summary |>                                                                                
  dplyr::inner_join(athlete_coef_rapm, by = "athlete_id") |>                                      
  dplyr::left_join(athlete, by = "athlete_id") |>                                                 
  dplyr::arrange(-coef) |>
  head(10) 

plot(model_rapm)

# things i played around with

box_totals <- wnba_player_box |>                                                                
  dplyr::group_by(athlete_id) |>                                                             
  dplyr::summarize(                                                                            
    box_minutes = sum(minutes, na.rm = TRUE),                                               
    points = sum(points, na.rm = TRUE),                                          
    rebounds = sum(rebounds, na.rm = TRUE),                                        
    assists = sum(assists, na.rm = TRUE),                                    
    steals = sum(steals, na.rm = TRUE),                                    
    blocks = sum(blocks, na.rm = TRUE),                                                       
    turnovers = sum(turnovers, na.rm = TRUE),                                                    
    fg_missed = sum(field_goals_attempted - field_goals_made, na.rm = TRUE),                     
    ft_missed = sum(free_throws_attempted - free_throws_made, na.rm = TRUE),                     
    .groups = "drop"                                                                            
  ) |>                                                                                            
  dplyr::filter(box_minutes > 0) |>                                                               
  dplyr::mutate(                                                                                  
    pts_per40  = 40 * points / box_minutes,                                                    
    reb_per40  = 40 * rebounds / box_minutes,                                                     
    ast_per40  = 40 * assists / box_minutes,                                                     
    stl_per40  = 40 * steals / box_minutes,                                                     
    blk_per40  = 40 * blocks / box_minutes,                                                     
    tov_per40  = 40 * turnovers / box_minutes,                                                     
    miss_per40 = 40 * (fg_missed + ft_missed) / box_minutes                                        
  )                                                                                                


prior_data <- athlete_summary |>                                                                  
  dplyr::inner_join(box_totals, by = "athlete_id") |>                                             
  dplyr::mutate(pm_per_min = plus_minus / minutes)                                                 

box_model <- lm(                                                                                   
  pm_per_min ~ pts_per40 + reb_per40 + ast_per40 + stl_per40 + blk_per40 + tov_per40 + miss_per40, 
  data = prior_data,                                                                                
  weights = minutes                                                                                 
)                                                                                                   

prior_data <- prior_data |>                                                                        
  dplyr::mutate(prior_rating = predict(box_model, newdata = prior_data))                            

summary(box_model)      

prior_lookup <- X_data |>                                                                          
  dplyr::distinct(column, athlete_id) |>                                                   
  dplyr::arrange(column) |>                                                                        
  dplyr::left_join(prior_data |> dplyr::select(athlete_id, prior_rating), by = "athlete_id") |>    
  dplyr::mutate(prior_rating = dplyr::coalesce(prior_rating, 0))                                    

mu <- prior_lookup$prior_rating                                                                  
offset_vec <- as.numeric(x %*% mu)                                                               

model_rapm_prior <- glmnet::cv.glmnet(                                                             
  x = x,                                                                                            
  y = y,                                                                                            
  weights = w,                                                                                      
  offset = offset_vec,                                                                              
  alpha = 0,                                                                                        
  standardize = FALSE                                                                               
)                                                                                                    

athlete_coef_rapm_prior <- X_data |>                                                                
  dplyr::distinct(column, athlete_id) |>                                                            
  dplyr::arrange(column) |>                                                                         
  dplyr::mutate(                                                                                     
    prior = mu,                                                                                      
    delta = coef(model_rapm_prior, s = "lambda.min")[-1, 1],                                        
    coef  = prior + delta                                                                            
  ) |>                                                                                               
  dplyr::select(athlete_id, prior, delta, coef)                                                      


athlete_coef_rapm |>                                                         
  dplyr::rename(coef_rapm = coef) |>                                                                
  dplyr::inner_join(                                                                                
    athlete_coef_rapm_prior |> dplyr::rename(coef_rapm_prior = coef),                               
    by = "athlete_id"                                                                                
  ) |>                                                                                               
  dplyr::inner_join(athlete_summary, by = "athlete_id") |>                                          
  dplyr::left_join(athlete, by = "athlete_id") |>                                                   
  dplyr::filter(team_abbreviation == "DAL") |>                                                       
  dplyr::arrange(-coef_rapm_prior) |>                                                                
  dplyr::select(athlete_display_name, minutes, prior, coef_rapm, coef_rapm_prior) 


athlete_coef_rapm |>                                                                                
  dplyr::rename(coef_rapm = coef) |>                                                                
  dplyr::inner_join(                                                                                
    athlete_coef_rapm_prior |> dplyr::rename(coef_rapm_prior = coef),                               
    by = "athlete_id"                                                                                
  ) |>                                                                                              
  dplyr::inner_join(athlete_summary, by = "athlete_id") |>                                          
  dplyr::left_join(athlete, by = "athlete_id") |>                                                   
  dplyr::arrange(-coef_rapm_prior) |>                                                                
  dplyr::select(athlete_display_name, team_abbreviation, minutes, prior, coef_rapm, coef_rapm_prior) |> 
  head(10)               

# dont use raw plus minus in the prior
# take prior and insert in final coef_rapm_prior to estimate at same time
# regularize z

# Edits after 09/17/26

# List of the 7 box-score features that make up each player's z_p vector
box_feature_names <- c("pts_per40", "reb_per40", "ast_per40",
                       "stl_per40", "blk_per40", "tov_per40", "miss_per40")


# one row per player, holding just their box-stat feature vector
z_p <- box_totals |>
  dplyr::select(athlete_id, dplyr::all_of(box_feature_names))

z_p_matrix <- as.matrix(z_p[, box_feature_names])

# Z_i = sum_{p in H_i} z_p - sum_{p in A_i} z_p', per stint, using the
# same +1/-1 sign already computed in X_data$value
Z_data <- X_data |>
  dplyr::left_join(z_p, by = "athlete_id") |>
  dplyr::mutate(dplyr::across(dplyr::all_of(box_feature_names), ~ dplyr::coalesce(.x, 0) * value)) |>
  dplyr::group_by(row) |>
  dplyr::summarize(dplyr::across(dplyr::all_of(box_feature_names), sum), .groups = "drop") |>
  dplyr::arrange(row)

# Z as a plain matrix: one row per stint, 7 columns = Z_i for that stint
Z <- as.matrix(Z_data[, box_feature_names])

# lineup dummies and box features combined into one design matrix,
# so a single model can use both at once
x_full <- Matrix::cbind2(x, Matrix::Matrix(Z, sparse = TRUE))

# Single joint ridge fit: beta_adj (player residuals) and c (box
# coefficients) are estimated together in one model, both are
# penalized by the same lambda - this is the main edit that replaces the 
# old two-stage lm() + offset approach
model_joint <- glmnet::cv.glmnet(
  x = x_full,
  y = y,
  weights = w,
  alpha = 0,
  standardize = FALSE
)

# Pull the fitted coefficients apart: first block = player deltas (beta_adj),
# last block = box-stat coefficients (c)
joint_coefs <- coef(model_joint, s = "lambda.min")[-1, 1]
player_delta <- joint_coefs[seq_len(ncol(x))]
c_coef <- joint_coefs[(ncol(x) + 1):length(joint_coefs)]
names(c_coef) <- box_feature_names

print(c_coef)

# O_p = c^T z_p for each player: their box-score-implied prior rating,
# computed using the c that was just estimated
athlete_prior <- z_p |>
  dplyr::mutate(prior_rating = as.numeric(z_p_matrix %*% c_coef)) |>
  dplyr::select(athlete_id, prior_rating)


# Final per-player table: attach beta_adj (by column order) and prior
# (by athlete_id), then coef = prior + beta_adj (delta) is the final RAPM-with-prior rating
athlete_coef_rapm_prior <- X_data |>
  dplyr::distinct(column, athlete_id) |>
  dplyr::arrange(column) |>
  dplyr::mutate(delta = player_delta) |>
  dplyr::left_join(athlete_prior, by = "athlete_id") |>
  dplyr::mutate(
    prior_rating = dplyr::coalesce(prior_rating, 0),
    coef = prior_rating + delta
  ) |>
  dplyr::select(athlete_id, prior = prior_rating, delta, coef)

# looking at dallas wings
athlete_coef_rapm |>
  dplyr::rename(coef_rapm = coef) |>
  dplyr::inner_join(
    athlete_coef_rapm_prior |> dplyr::rename(coef_rapm_prior = coef),
    by = "athlete_id"
  ) |>
  dplyr::inner_join(athlete_summary, by = "athlete_id") |>
  dplyr::left_join(athlete, by = "athlete_id") |>
  dplyr::filter(team_abbreviation == "DAL") |>
  dplyr::arrange(-coef_rapm_prior) |>
  dplyr::select(athlete_display_name, minutes, prior, coef_rapm, coef_rapm_prior)

# looking at top 10
athlete_coef_rapm |>
  dplyr::rename(coef_rapm = coef) |>
  dplyr::inner_join(
    athlete_coef_rapm_prior |> dplyr::rename(coef_rapm_prior = coef),
    by = "athlete_id"
  ) |>
  dplyr::inner_join(athlete_summary, by = "athlete_id") |>
  dplyr::left_join(athlete, by = "athlete_id") |>
  dplyr::arrange(-coef_rapm_prior) |>
  dplyr::select(athlete_display_name, team_abbreviation, minutes, prior, coef_rapm, coef_rapm_prior) |>
  head(10)

summary(athlete_coef_rapm_prior$delta)
hist(athlete_coef_rapm_prior$delta, breaks = 30,
     main = "Distribution of beta_adj across all players", xlab = "beta_adj")




