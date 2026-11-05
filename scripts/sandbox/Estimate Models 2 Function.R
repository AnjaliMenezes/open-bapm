calculate_bapm <- function(pbp_data,
                          player_box_data,
                          player_box_prior_data = NULL,
                          window_days = 365,
                          min_poss_scale = 200,
                          K_outer = 10,
                          K_inner = 5,
                          c_penalty = 1,
                          seed = 2025) {
  # Required R packages: glmnet, Matrix, slider, dplyr, tidyr, glue
  if (!is.null(seed)) set.seed(seed)
  
  # 1. Helper functions
  elapsed_min <- function(qtr, clock_minutes, clock_seconds) {
    q_start <- ifelse(qtr <= 4, 10 * (qtr - 1), 40 + 5 * (qtr - 5))
    q_len <- ifelse(qtr <= 4, 10, 5)
    q_start + q_len - (clock_minutes + clock_seconds / 60)
  }
  
  fit_ridge <- function(xm, ym, wm, game_ids, nfolds = K_inner, penalty_factor = NULL) {
    if (is.null(penalty_factor)) penalty_factor <- rep(1, ncol(xm))
    gids <- unique(game_ids)
    gf <- setNames(sample(rep(seq_len(nfolds), length.out = length(gids))), gids)
    glmnet::cv.glmnet(
      x = xm, y = ym, weights = wm, alpha = 0, standardize = FALSE,
      foldid = unname(gf[game_ids]), penalty.factor = penalty_factor
    )
  }
  
  fit_bpm  <- function(Zm, ym, wm) lm.wfit(cbind(1, Zm), ym, wm)$coefficients
  pred_bpm <- function(coefs, Zm) as.numeric(cbind(1, Zm) %*% coefs)
  wrmse    <- function(y, yhat, w) sqrt(weighted.mean((y - yhat)^2, w))
  wmae     <- function(y, yhat, w) weighted.mean(abs(y - yhat), w)
  
  # 2. Extract starting lineups from current season box score
  starters <- player_box_data |>
    dplyr::filter(starter) |>
    dplyr::mutate(
      score_diff_final = (1 - 2 * (home_away == "away")) * (team_score - opponent_team_score)
    ) |>
    dplyr::select(game_id, home_away, athlete_id, score_diff_final)
  
  # 3. Process substitutions from play-by-play data
  substitutions <- pbp_data |>
    dplyr::filter(type_text == "Substitution") |>
    dplyr::mutate(
      time_start = elapsed_min(qtr, clock_minutes, clock_seconds),
      time_remaining = end_game_seconds_remaining / 60,
      score_diff_start = home_score - away_score
    ) |>
    dplyr::select(game_id, time_start, time_remaining, score_diff_start, athlete_id_1, athlete_id_2)
  
  # 4. Reconstruct on-court lineups per substitution
  player_on <- starters |>
    dplyr::group_by(game_id, home_away) |>
    dplyr::mutate(index = dplyr::row_number(), time_start = 0, score_diff_start = 0, time_remaining = NA) |>
    dplyr::ungroup()
  
  for (i in seq_len(nrow(substitutions))) {
    new_player_on <- player_on |>
      dplyr::filter(
        game_id == substitutions$game_id[i],
        athlete_id == substitutions$athlete_id_2[i]
      ) |>
      dplyr::slice(dplyr::n()) |>
      dplyr::mutate(
        athlete_id = substitutions$athlete_id_1[i],
        time_start = substitutions$time_start[i],
        time_remaining = substitutions$time_remaining[i],
        score_diff_start = substitutions$score_diff_start[i]
      )
    player_on <- dplyr::bind_rows(player_on, new_player_on)
  }
  
  # 5. Collapse simultaneous subs and pivot to wide stint lineups
  player_on_collapsed <- player_on |>
    dplyr::group_by(
      game_id, time_start, time_remaining, score_diff_start, score_diff_final, home_away, index
    ) |>
    dplyr::summarize(athlete_id = athlete_id[dplyr::n()], .groups = "drop")
  
  lineups <- player_on_collapsed |>
    dplyr::mutate(name = glue::glue("{home_away}_{index}")) |>
    dplyr::select(-home_away, -index) |>
    tidyr::pivot_wider(names_from = name, values_from = athlete_id) |>
    tidyr::fill(dplyr::matches("^away|^home"), .direction = "down")
  
  # 6. Extract unique athlete mapping
  athlete <- player_box_data |>
    dplyr::count(athlete_id, athlete_display_name, team_abbreviation) |>
    dplyr::group_by(athlete_id) |>
    dplyr::arrange(-n) |>
    dplyr::slice(1) |>
    dplyr::select(-n) |>
    dplyr::ungroup()
  
  # 7. Compute stint-level duration and score differentials
  stint_data <- lineups |>
    dplyr::group_by(game_id) |>
    dplyr::mutate(
      minutes = dplyr::coalesce(dplyr::lead(time_start, 1) - time_start, time_remaining),
      score_diff = dplyr::coalesce(dplyr::lead(score_diff_start, 1), score_diff_final) - score_diff_start
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(minutes > 0)
  
  # 8. Calculate possessions per stint from PBP events
  poss_events <- pbp_data |>
    dplyr::mutate(
      time = elapsed_min(qtr, clock_minutes, clock_seconds),
      is_ft = grepl("Free Throw", type_text, ignore.case = TRUE),
      is_fga = dplyr::coalesce(as.logical(shooting_play), FALSE) & !is_ft,
      is_oreb = grepl("Offensive Rebound", type_text, ignore.case = TRUE),
      is_tov = grepl("Turnover", type_text, ignore.case = TRUE),
      poss_event = is_fga + 0.44 * is_ft - is_oreb + is_tov
    ) |>
    dplyr::filter(poss_event != 0) |>
    dplyr::select(game_id, time, poss_event)
  
  stint_poss_vec <- numeric(nrow(stint_data))
  unique_games <- unique(stint_data$game_id)
  
  for (g in unique_games) {
    idx <- which(stint_data$game_id == g)
    ev <- poss_events[poss_events$game_id == g, ]
    bin <- findInterval(ev$time, stint_data$time_start[idx])
    bin[bin == 0] <- 1
    sums <- tapply(ev$poss_event, factor(bin, levels = seq_along(idx)), sum)
    sums[is.na(sums)] <- 0
    stint_poss_vec[idx] <- as.numeric(sums) / 2
  }
  stint_data$poss <- stint_poss_vec
  stint_data <- stint_data |> dplyr::filter(poss > 0)
  
  # 9. Format long stint table and sparse design matrix (X)
  data_long <- stint_data |>
    dplyr::mutate(stint = dplyr::row_number()) |>
    dplyr::select(game_id, stint, minutes, poss, score_diff, dplyr::matches("^away|^home")) |>
    tidyr::pivot_longer(cols = c(dplyr::matches("^away|^home")), values_to = "athlete_id") |>
    dplyr::mutate(game_id = as.character(game_id))
  
  X_data <- data_long |>
    dplyr::mutate(
      row    = stint,
      column = as.numeric(as.factor(athlete_id)),
      value  = ifelse(substring(name, 1, 4) == "home", 1, -1)
    )
  
  x <- Matrix::sparseMatrix(i = X_data$row, j = X_data$column, x = X_data$value)
  y <- 100 * stint_data$score_diff / stint_data$poss
  w <- stint_data$poss
  stint_data$game_id <- as.character(stint_data$game_id)
  
  athlete_summary <- data_long |>
    dplyr::group_by(athlete_id) |>
    dplyr::summarize(
      minutes = sum(minutes),
      poss = sum(poss),
      plus_minus = sum((1 - 2 * grepl("away", name)) * score_diff),
      .groups = "drop"
    ) |>
    dplyr::mutate(pm_per100 = 100 * plus_minus / poss)
  
  # 10. Process rolling box score metrics (over past window_days)
  required_cols <- c("game_id", "game_date", "team_id", "athlete_id", "minutes", "points", "rebounds",
                     "assists", "steals", "blocks", "turnovers", "offensive_rebounds",
                     "field_goals_attempted", "field_goals_made", "free_throws_attempted",
                     "free_throws_made")
  
  prep_box <- function(d) {
    if (is.null(d)) return(NULL)
    d |>
      dplyr::select(dplyr::all_of(required_cols)) |>
      dplyr::mutate(game_id = as.character(game_id), game_date = as.character(game_date))
  }
  
  box_combined <- dplyr::bind_rows(prep_box(player_box_data), prep_box(player_box_prior_data)) |>
    dplyr::mutate(
      game_id = as.character(game_id),
      game_date = as.Date(substr(as.character(game_date), 1, 10)),
      dplyr::across(c(minutes, points, rebounds, assists, steals, blocks, turnovers,
                      offensive_rebounds, field_goals_attempted, field_goals_made,
                      free_throws_attempted, free_throws_made),
                    ~ dplyr::coalesce(as.numeric(.x), 0))
    ) |>
    dplyr::filter(minutes > 0)
  
  team_poss <- box_combined |>
    dplyr::group_by(game_id, team_id) |>
    dplyr::summarize(
      team_min = sum(minutes),
      t_poss = sum(field_goals_attempted) + 0.44 * sum(free_throws_attempted) -
        sum(offensive_rebounds) + sum(turnovers),
      .groups = "drop"
    ) |>
    dplyr::group_by(game_id) |>
    dplyr::mutate(game_poss = mean(t_poss)) |>
    dplyr::ungroup() |>
    dplyr::select(game_id, team_id, team_min, game_poss)
  
  box_combined <- box_combined |>
    dplyr::left_join(team_poss, by = c("game_id", "team_id")) |>
    dplyr::mutate(
      poss_played = minutes / (team_min / 5) * game_poss,
      miss = (field_goals_attempted - field_goals_made) + (free_throws_attempted - free_throws_made)
    ) |>
    dplyr::arrange(athlete_id, game_date)
  
  stat_cols <- c("points", "rebounds", "assists", "steals", "blocks", "turnovers", "miss")
  box_feature_names <- c("pts_per100", "reb_per100", "ast_per100",
                         "stl_per100", "blk_per100", "tov_per100", "miss_per100")
  roll_cols <- c(stat_cols, "poss_played")
  
  box_combined <- box_combined |>
    dplyr::group_by(athlete_id) |>
    dplyr::arrange(game_date, .by_group = TRUE) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(roll_cols),
      ~ slider::slide_index_dbl(.x, game_date, sum, .before = window_days, .after = -1),
      .names = "roll_{.col}"
    )) |>
    dplyr::ungroup()
  
  roll <- as.matrix(box_combined[, paste0("roll_", roll_cols)])
  colnames(roll) <- roll_cols
  
  rate <- sapply(stat_cols, function(s) {
    ifelse(roll[, "poss_played"] > 0, 100 * roll[, s] / roll[, "poss_played"], NA_real_)
  })
  colnames(rate) <- box_feature_names
  
  ok <- roll[, "poss_played"] >= min_poss_scale
  feat_mean <- colMeans(rate[ok, , drop = FALSE])
  feat_sd <- apply(rate[ok, , drop = FALSE], 2, sd)
  z_mat <- sweep(sweep(rate, 2, feat_mean, "-"), 2, feat_sd, "/")
  z_mat[is.na(z_mat)] <- 0
  
  z_pg <- data.frame(game_id = box_combined$game_id, athlete_id = box_combined$athlete_id, z_mat, check.names = FALSE)
  z_pg <- z_pg[!duplicated(z_pg[, c("game_id", "athlete_id")]), ]
  
  # 11. Build stint-level box score matrix (Z) and full design matrix (x_full)
  Z_long <- X_data |>
    dplyr::left_join(z_pg, by = c("game_id", "athlete_id")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(box_feature_names), ~ dplyr::coalesce(.x, 0)))
  
  Z_data <- Z_long |>
    dplyr::mutate(dplyr::across(dplyr::all_of(box_feature_names), ~ .x * value)) |>
    dplyr::group_by(row) |>
    dplyr::summarize(dplyr::across(dplyr::all_of(box_feature_names), sum), .groups = "drop") |>
    dplyr::arrange(row)
  
  Z <- as.matrix(Z_data[, box_feature_names])
  x_full <- Matrix::cbind2(x, Matrix::Matrix(Z, sparse = TRUE))
  pen_factor <- c(rep(1, ncol(x)), rep(c_penalty, ncol(Z)))
  
  # 12. K-Fold Game-Grouped Cross-Validation
  games <- unique(stint_data$game_id)
  game_fold <- setNames(sample(rep(seq_len(K_outer), length.out = length(games))), games)
  fold <- unname(game_fold[stint_data$game_id])
  
  model_names <- c("NULL", "RAPM", "BPM", "BAPM")
  oos <- matrix(NA_real_, nrow(x), length(model_names), dimnames = list(NULL, model_names))
  
  for (k in seq_len(K_outer)) {
    te <- which(fold == k)
    tr <- which(fold != k)
    
    oos[te, "NULL"] <- weighted.mean(y[tr], w[tr])
    
    f_rapm <- fit_ridge(x[tr, , drop = FALSE], y[tr], w[tr], stint_data$game_id[tr])
    oos[te, "RAPM"] <- as.numeric(predict(f_rapm, newx = x[te, , drop = FALSE], s = "lambda.min"))
    
    oos[te, "BPM"] <- pred_bpm(fit_bpm(Z[tr, , drop = FALSE], y[tr], w[tr]), Z[te, , drop = FALSE])
    
    f_bapm <- fit_ridge(x_full[tr, , drop = FALSE], y[tr], w[tr], stint_data$game_id[tr], penalty_factor = pen_factor)
    oos[te, "BAPM"] <- as.numeric(predict(f_bapm, newx = x_full[te, , drop = FALSE], s = "lambda.min"))
  }
  
  fold_metrics <- do.call(rbind, lapply(seq_len(K_outer), function(k) {
    te <- which(fold == k)
    do.call(rbind, lapply(model_names, function(m) {
      data.frame(
        fold = k, model = m,
        rmse = wrmse(y[te], oos[te, m], w[te]),
        mae  = wmae(y[te], oos[te, m], w[te])
      )
    }))
  }))
  
  cv_summary <- fold_metrics |>
    dplyr::group_by(model) |>
    dplyr::summarize(
      rmse_se = sd(rmse) / sqrt(dplyr::n()), rmse = mean(rmse),
      mae_se  = sd(mae) / sqrt(dplyr::n()), mae  = mean(mae),
      .groups = "drop"
    )
  
  sse_null <- sum(w * (y - oos[, "NULL"])^2)
  skill <- sapply(model_names, function(m) 1 - sum(w * (y - oos[, m])^2) / sse_null)
  cv_summary$oos_skill_vs_null <- skill[cv_summary$model]
  cv_summary <- cv_summary[match(model_names, cv_summary$model), ]
  
  # 13. Fit final models on full data
  model_rapm  <- fit_ridge(x, y, w, stint_data$game_id)
  model_joint <- fit_ridge(x_full, y, w, stint_data$game_id, penalty_factor = pen_factor)
  bpm_coefs   <- fit_bpm(Z, y, w)
  
  players <- X_data |> dplyr::distinct(column, athlete_id) |> dplyr::arrange(column)
  
  rapm_coef   <- coef(model_rapm, s = "lambda.min")[-1, 1]
  joint_coefs <- coef(model_joint, s = "lambda.min")[-1, 1]
  delta       <- joint_coefs[seq_len(ncol(x))]
  c_coef      <- joint_coefs[(ncol(x) + 1):length(joint_coefs)]
  names(c_coef) <- box_feature_names
  
  c_bpm <- bpm_coefs[-1]
  names(c_bpm) <- box_feature_names
  
  # 14. Compute player-level aggregated prior ratings and final metrics
  prior_by_player <- Z_long |>
    dplyr::mutate(
      prior_bapm = as.numeric(as.matrix(Z_long[, box_feature_names]) %*% c_coef),
      prior_bpm  = as.numeric(as.matrix(Z_long[, box_feature_names]) %*% c_bpm)
    ) |>
    dplyr::group_by(athlete_id) |>
    dplyr::summarize(
      prior_bapm = weighted.mean(prior_bapm, poss),
      prior_bpm  = weighted.mean(prior_bpm, poss),
      .groups = "drop"
    )
  
  athlete_ratings <- players |>
    dplyr::mutate(rapm = rapm_coef, delta = delta) |>
    dplyr::left_join(prior_by_player, by = "athlete_id") |>
    dplyr::mutate(bpm = prior_bpm, bapm = prior_bapm + delta) |>
    dplyr::left_join(athlete_summary, by = "athlete_id") |>
    dplyr::left_join(athlete, by = "athlete_id") |>
    dplyr::arrange(dplyr::desc(bapm)) |>
    dplyr::select(
      athlete_id, athlete_display_name, team_abbreviation, minutes, poss,
      pm_per100, rapm, bpm, prior_bapm, delta, bapm
    )
  
  # Return structured results
  list(
    athlete_ratings = athlete_ratings,
    cv_summary      = cv_summary,
    fold_metrics    = fold_metrics,
    box_coefs_bapm  = c_coef,
    box_coefs_bpm   = c_bpm,
    model_rapm      = model_rapm,
    model_bapm      = model_joint
  )
  head(bapm_results$athlete_ratings, 10)
}
