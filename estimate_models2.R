library(glmnet)
library(Matrix)
library(slider)

window_days <- 365
min_poss_scale <- 200
K_outer <- 10
K_inner <- 5
c_penalty <- 1
set.seed(2025)

wnba_pbp <- read.csv("input/data/wnba_pbp_2025.csv")
wnba_player_box <- read.csv("input/data/wnba_player_box_2025.csv")
wnba_player_box_2024 <- read.csv("input/data/wnba_player_box_2024.csv")

starters <- wnba_player_box |>
  dplyr::filter(starter) |>
  dplyr::mutate(
    score_diff_final = (1 - 2 * (home_away == "away")) * (team_score - opponent_team_score)
  ) |>
  dplyr::select(game_id, home_away, athlete_id, score_diff_final)

# elapsed game minutes; regulation quarters are 10 min, overtimes are 5 min
elapsed_min <- function(qtr, clock_minutes, clock_seconds) {
  q_start <- ifelse(qtr <= 4, 10 * (qtr - 1), 40 + 5 * (qtr - 5))
  q_len <- ifelse(qtr <= 4, 10, 5)
  q_start + q_len - (clock_minutes + clock_seconds / 60)
}

substitutions <- wnba_pbp |>
  dplyr::filter(type_text == "Substitution") |>
  dplyr::mutate(
    time_start = elapsed_min(qtr, clock_minutes, clock_seconds),
    time_remaining = end_game_seconds_remaining / 60,
    score_diff_start = home_score - away_score
  ) |>
  dplyr::select(game_id, time_start, time_remaining, score_diff_start, athlete_id_1, athlete_id_2)

player_on <- starters |>
  dplyr::group_by(game_id, home_away) |>
  dplyr::mutate(index = 1:dplyr::n(), time_start = 0, score_diff_start = 0, time_remaining = NA) |>
  dplyr::ungroup()

for (i in 1:nrow(substitutions)) {
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

athlete <- wnba_player_box |>
  dplyr::count(athlete_id, athlete_display_name, team_abbreviation) |>
  dplyr::group_by(athlete_id) |>
  dplyr::arrange(-n) |>
  dplyr::slice(1) |>
  dplyr::select(-n) |>
  dplyr::ungroup()

data <- lineups |>
  dplyr::group_by(game_id) |>
  dplyr::mutate(
    minutes = dplyr::coalesce(dplyr::lead(time_start, 1) - time_start, time_remaining),
    score_diff = dplyr::coalesce(dplyr::lead(score_diff_start, 1), score_diff_final) - score_diff_start
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(minutes > 0)

# Possessions per stint
poss_events <- wnba_pbp |>
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

stint_poss_vec <- numeric(nrow(data))
for (g in unique(data$game_id)) {
  idx <- which(data$game_id == g)
  ev <- poss_events[poss_events$game_id == g, ]
  bin <- findInterval(ev$time, data$time_start[idx])
  bin[bin == 0] <- 1
  sums <- tapply(ev$poss_event, factor(bin, levels = seq_along(idx)), sum)
  sums[is.na(sums)] <- 0
  stint_poss_vec[idx] <- as.numeric(sums) / 2
}
data$poss <- stint_poss_vec

print(summary(tapply(data$poss, data$game_id, sum)))

data <- data |> dplyr::filter(poss > 0)

data_long <- data |>
  dplyr::mutate(stint = 1:dplyr::n()) |>
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

y <- 100 * data$score_diff / data$poss
w <- data$poss
data$game_id <- as.character(data$game_id)

# Raw on-court summary, per possession
athlete_summary <- data_long |>
  dplyr::group_by(athlete_id) |>
  dplyr::summarize(
    minutes = sum(minutes),
    poss = sum(poss),
    plus_minus = sum((1 - 2 * grepl("away", name)) * score_diff),
    .groups = "drop"
  ) |>
  dplyr::mutate(pm_per100 = 100 * plus_minus / poss)

# Rolling Window
required <- c("game_id", "game_date", "team_id", "athlete_id", "minutes", "points", "rebounds",
              "assists", "steals", "blocks", "turnovers", "offensive_rebounds",
              "field_goals_attempted", "field_goals_made", "free_throws_attempted",
              "free_throws_made")

prep_box <- function(d) {
  d |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::mutate(game_id = as.character(game_id), game_date = as.character(game_date))
}

box <- dplyr::bind_rows(prep_box(wnba_player_box), prep_box(wnba_player_box_2024)) |>
  dplyr::mutate(
    game_id = as.character(game_id),
    game_date = as.Date(substr(as.character(game_date), 1, 10)),
    dplyr::across(c(minutes, points, rebounds, assists, steals, blocks, turnovers,
                    offensive_rebounds, field_goals_attempted, field_goals_made,
                    free_throws_attempted, free_throws_made),
                  ~ dplyr::coalesce(as.numeric(.x), 0))
  ) |>
  dplyr::filter(minutes > 0)

# game possessions = mean of both teams' (FGA + 0.44 FTA - OREB + TOV)
# player possessions = share of team minutes * game possessions
team_poss <- box |>
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

box <- box |>
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

# For each player-game: sum stats over that player's games in the window
# [date - window_days, date - 1 day], a game never feeds its own prior
box <- box |>
  dplyr::group_by(athlete_id) |>
  dplyr::arrange(game_date, .by_group = TRUE) |>
  dplyr::mutate(dplyr::across(
    dplyr::all_of(roll_cols),
    ~ slider::slide_index_dbl(.x, game_date, sum, .before = window_days, .after = -1),
    .names = "roll_{.col}"
  )) |>
  dplyr::ungroup()

roll <- as.matrix(box[, paste0("roll_", roll_cols)])
colnames(roll) <- roll_cols

# Rolling per-100 rate = 100 * (stat summed over the window) / (possessions over the window).
# A player with no games in the window has no rate (0/0), so it is NA here and gets z = 0 below
rate <- sapply(stat_cols, function(s) {
  ifelse(roll[, "poss_played"] > 0, 100 * roll[, s] / roll[, "poss_played"], NA_real_)
})
colnames(rate) <- box_feature_names

# Standardize to z-scores
ok <- roll[, "poss_played"] >= min_poss_scale
feat_mean <- colMeans(rate[ok, , drop = FALSE])
feat_sd <- apply(rate[ok, , drop = FALSE], 2, sd)
z_mat <- sweep(sweep(rate, 2, feat_mean, "-"), 2, feat_sd, "/")
z_mat[is.na(z_mat)] <- 0

z_pg <- data.frame(game_id = box$game_id, athlete_id = box$athlete_id, z_mat, check.names = FALSE)
z_pg <- z_pg[!duplicated(z_pg[, c("game_id", "athlete_id")]), ]

# stint level box matrix
Z_long <- X_data |>
  dplyr::left_join(z_pg, by = c("game_id", "athlete_id")) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(box_feature_names), ~ dplyr::coalesce(.x, 0))) 

Z_data <- Z_long |>
  dplyr::mutate(dplyr::across(dplyr::all_of(box_feature_names), ~ .x * value)) |>
  dplyr::group_by(row) |>
  dplyr::summarize(dplyr::across(dplyr::all_of(box_feature_names), sum), .groups = "drop") |>
  dplyr::arrange(row)

Z <- as.matrix(Z_data[, box_feature_names])
stopifnot(nrow(Z) == nrow(x), length(y) == nrow(x))
x_full <- Matrix::cbind2(x, Matrix::Matrix(Z, sparse = TRUE))
pen_factor <- c(rep(1, ncol(x)), rep(c_penalty, ncol(Z)))

# Ridge with lambda chosen by CV whose inner folds are also made by game
fit_ridge <- function(xm, ym, wm, game_ids, nfolds = K_inner, penalty_factor = NULL) {
  if (is.null(penalty_factor)) penalty_factor <- rep(1, ncol(xm))
  gids <- unique(game_ids)
  gf <- setNames(sample(rep(seq_len(nfolds), length.out = length(gids))), gids)
  glmnet::cv.glmnet(
    x = xm, y = ym, weights = wm, alpha = 0, standardize = FALSE,
    foldid = unname(gf[game_ids]), penalty.factor = penalty_factor
  )
}

# BPM
fit_bpm  <- function(Zm, ym, wm) lm.wfit(cbind(1, Zm), ym, wm)$coefficients
pred_bpm <- function(coefs, Zm) as.numeric(cbind(1, Zm) %*% coefs)

wrmse <- function(y, yhat, w) sqrt(weighted.mean((y - yhat)^2, w))
wmae <- function(y, yhat, w) weighted.mean(abs(y - yhat), w)

# 10-fold cross validation (folds = games; every model sees identical folds)

games <- unique(data$game_id)
game_fold <- setNames(sample(rep(seq_len(K_outer), length.out = length(games))), games)
fold <- unname(game_fold[data$game_id])

model_names <- c("NULL", "RAPM", "BPM", "BAPM")
oos <- matrix(NA_real_, nrow(x), length(model_names), dimnames = list(NULL, model_names))

for (k in seq_len(K_outer)) {
  te <- which(fold == k)
  tr <- which(fold != k)
  
  # Null benchmark
  oos[te, "NULL"] <- weighted.mean(y[tr], w[tr])
  
  # RAPM
  f_rapm <- fit_ridge(x[tr, , drop = FALSE], y[tr], w[tr], data$game_id[tr])
  oos[te, "RAPM"] <- as.numeric(predict(f_rapm, newx = x[te, , drop = FALSE], s = "lambda.min"))
  
  # BPM
  oos[te, "BPM"] <- pred_bpm(fit_bpm(Z[tr, , drop = FALSE], y[tr], w[tr]), Z[te, , drop = FALSE])
  
  # BAPM
  f_bapm <- fit_ridge(x_full[tr, , drop = FALSE], y[tr], w[tr], data$game_id[tr],
                      penalty_factor = pen_factor)
  oos[te, "BAPM"] <- as.numeric(predict(f_bapm, newx = x_full[te, , drop = FALSE], s = "lambda.min"))
  
  message("finished fold ", k, " of ", K_outer)
}

# Per-fold metrics (possession-weighted, in points per 100 possessions)
fold_metrics <- do.call(rbind, lapply(seq_len(K_outer), function(k) {
  te <- which(fold == k)
  do.call(rbind, lapply(model_names, function(m) {
    data.frame(fold = k, model = m,
               rmse = wrmse(y[te], oos[te, m], w[te]),
               mae  = wmae(y[te], oos[te, m], w[te]))
  }))
}))

cv_summary <- fold_metrics |>
  dplyr::group_by(model) |>
  dplyr::summarize(
    rmse_se = sd(rmse) / sqrt(dplyr::n()), rmse = mean(rmse),
    mae_se = sd(mae) / sqrt(dplyr::n()), mae = mean(mae),
    .groups = "drop"
  )

# Pooled out-of-sample skill vs. the null: 1 - SSE_model / SSE_null
sse_null <- sum(w * (y - oos[, "NULL"])^2)
skill <- sapply(model_names, function(m) 1 - sum(w * (y - oos[, m])^2) / sse_null)
cv_summary$oos_skill_vs_null <- skill[cv_summary$model]
cv_summary <- cv_summary[match(model_names, cv_summary$model), ]

cat("10-fold CV (folds by game), per-100-possession scale, possession-weighted")
print(as.data.frame(cv_summary), digits = 4)

# RMSE: BAPM vs RAPM
rm_w <- reshape(fold_metrics[, c("fold", "model", "rmse")], idvar = "fold",
                timevar = "model", direction = "wide")
d <- rm_w$rmse.BAPM - rm_w$rmse.RAPM
cat(sprintf("\nBAPM - RAPM RMSE: mean %.4f, SE %.4f (negative favors BAPM)\n",
            mean(d), sd(d) / sqrt(length(d))))


# Final fits on all data + player ratings per 100 possessions
model_rapm <- fit_ridge(x, y, w, data$game_id)
model_joint <- fit_ridge(x_full, y, w, data$game_id, penalty_factor = pen_factor)
bpm_coefs <- fit_bpm(Z, y, w)

players <- X_data |> dplyr::distinct(column, athlete_id) |> dplyr::arrange(column)

rapm_coef <- coef(model_rapm, s = "lambda.min")[-1, 1]
joint_coefs <- coef(model_joint, s = "lambda.min")[-1, 1]
delta <- joint_coefs[seq_len(ncol(x))]
c_coef <- joint_coefs[(ncol(x) + 1):length(joint_coefs)]
names(c_coef) <- box_feature_names
print(c_coef)
c_bpm <- bpm_coefs[-1]
names(c_bpm) <- box_feature_names

# Because the box prior now changes game to game, a player's prior is the
# possession-weighted average of c' z_{p,g} over the stints they played
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
  dplyr::select(athlete_display_name, team_abbreviation, minutes, poss, pm_per100,
                rapm, bpm, prior_bapm, delta, bapm)

print(head(dplyr::arrange(athlete_ratings, -bapm), 10), digits = 3)

print(dplyr::arrange(dplyr::filter(athlete_ratings, team_abbreviation == "DAL"), -bapm), digits = 3)
