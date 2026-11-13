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
    score_diff_final = (1 - 2 * (home_away == "away")) * (team_score - opponent_team_score),
    pts_h_final = ifelse(home_away == "home", team_score, opponent_team_score),
    pts_a_final = ifelse(home_away == "home", opponent_team_score, team_score)
  ) |>
  dplyr::select(game_id, home_away, athlete_id, score_diff_final, pts_h_final, pts_a_final)

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
    score_diff_start = home_score - away_score,
    pts_h_start = home_score,
    pts_a_start = away_score
  ) |>
  dplyr::select(game_id, time_start, time_remaining, score_diff_start, pts_h_start, pts_a_start,
                athlete_id_1, athlete_id_2)

player_on <- starters |>
  dplyr::group_by(game_id, home_away) |>
  dplyr::mutate(index = 1:dplyr::n(), time_start = 0, score_diff_start = 0,
                pts_h_start = 0, pts_a_start = 0, time_remaining = NA) |>
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
      score_diff_start = substitutions$score_diff_start[i],
      pts_h_start = substitutions$pts_h_start[i],
      pts_a_start = substitutions$pts_a_start[i]
    )
  player_on <- dplyr::bind_rows(player_on, new_player_on)
}

player_on_collapsed <- player_on |>
  dplyr::group_by(
    game_id, time_start, time_remaining, score_diff_start, score_diff_final,
    pts_h_start, pts_a_start, pts_h_final, pts_a_final, home_away, index
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
    score_diff = dplyr::coalesce(dplyr::lead(score_diff_start, 1), score_diff_final) - score_diff_start,
    # points scored by each side during the stint (pts_h - pts_a == score_diff)
    pts_h = dplyr::coalesce(dplyr::lead(pts_h_start, 1), pts_h_final) - pts_h_start,
    pts_a = dplyr::coalesce(dplyr::lead(pts_a_start, 1), pts_a_final) - pts_a_start
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
    miss = (field_goals_attempted - field_goals_made) + (free_throws_attempted - free_throws_made),
    dreb = rebounds - offensive_rebounds
  ) |>
  dplyr::arrange(athlete_id, game_date)

stat_cols <- c("points", "rebounds", "assists", "steals", "blocks", "turnovers", "miss")
box_feature_names <- c("pts_per100", "reb_per100", "ast_per100",
                       "stl_per100", "blk_per100", "tov_per100", "miss_per100")

# Feature sets for the offense/defense split model (fit in its own section below).
# Offensive stats feed the offensive prior; defensive stats feed the defensive prior.
# Rebounds are split into offensive and defensive.
off_stat_cols <- c("points", "assists", "turnovers", "miss", "offensive_rebounds")
off_feature_names <- c("pts_per100", "ast_per100", "tov_per100", "miss_per100", "oreb_per100")
def_stat_cols <- c("dreb", "steals", "blocks")
def_feature_names <- c("dreb_per100", "stl_per100", "blk_per100")

roll_cols <- unique(c(stat_cols, off_stat_cols, def_stat_cols, "poss_played"))

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

# No standardization: the box features are the raw rolling per-100 rates.
# League-average rates (from windows with enough possessions) fill players with no
# games in the window, and are used to center the priors when reporting ratings.
ok <- roll[, "poss_played"] >= min_poss_scale
feat_mean <- colMeans(rate[ok, , drop = FALSE])
box_mat <- rate
for (j in seq_len(ncol(box_mat))) box_mat[is.na(box_mat[, j]), j] <- feat_mean[j]

z_pg <- data.frame(game_id = box$game_id, athlete_id = box$athlete_id, box_mat, check.names = FALSE)
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
fit_bpm <- function(Zm, ym, wm) lm.wfit(cbind(1, Zm), ym, wm)$coefficients
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
# Features are raw rates, so center them at the league average to form the prior.
# (The fit itself is unaffected: the constant cancels in home-minus-away.)
Zc_long <- sweep(as.matrix(Z_long[, box_feature_names]), 2, feat_mean, "-")
prior_by_player <- Z_long |>
  dplyr::mutate(
    prior_bapm = as.numeric(Zc_long %*% c_coef),
    prior_bpm  = as.numeric(Zc_long %*% c_bpm)
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
print(dplyr::arrange(dplyr::filter(athlete_ratings, team_abbreviation == "LV"), -bapm), digits = 3)


# ==============================================================================
# OFFENSIVE / DEFENSIVE BAPM
# ------------------------------------------------------------------------------
# Same idea as BAPM, but every player gets two coefficients:
#   o_p = offensive rating (points added per 100 possessions on offense)
#   d_p = defensive rating (points prevented per 100 possessions on defense;
#                           positive = good defense)
# Each stint contributes two rows, one per offense:
#   home on offense: y = 100 * home pts / poss,  + o_p (home players), - d_p (away players)
#   away on offense: y = 100 * away pts / poss,  + o_p (away players), - d_p (home players)
# so home-minus-away points per 100 = sum_home (o + d) - sum_away (o + d), and
# o_p + d_p is directly comparable to the overall BAPM / RAPM coefficient.
# Priors: o_p ~ c_off' x_off,p   (offensive box stats, raw per-100 rates)
#         d_p ~ c_def' x_def,p   (defensive box stats, raw per-100 rates)
# A (unpenalized) home-offense indicator plays the role the intercept plays in
# the original model (home-court advantage)
# ==============================================================================

od_feature_names <- c(off_feature_names, def_feature_names)
od_stat_cols <- c(off_stat_cols, def_stat_cols)

# Raw rolling per-100 rates (no standardization; same recipe and `ok` filter as above)
rate_od <- sapply(od_stat_cols, function(s) {
  ifelse(roll[, "poss_played"] > 0, 100 * roll[, s] / roll[, "poss_played"], NA_real_)
})
colnames(rate_od) <- od_feature_names

feat_mean_od <- colMeans(rate_od[ok, , drop = FALSE])
box_mat_od <- rate_od
for (j in seq_len(ncol(box_mat_od))) box_mat_od[is.na(box_mat_od[, j]), j] <- feat_mean_od[j]

z_pg_od <- data.frame(game_id = box$game_id, athlete_id = box$athlete_id, box_mat_od, check.names = FALSE)
z_pg_od <- z_pg_od[!duplicated(z_pg_od[, c("game_id", "athlete_id")]), ]

# stint level box matrices, kept separately for the home and away players
Z_long_od <- X_data |>
  dplyr::left_join(z_pg_od, by = c("game_id", "athlete_id")) |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(od_feature_names), ~ dplyr::coalesce(.x, 0)),
    side = ifelse(value == 1, "home", "away")
  )

Z_side <- Z_long_od |>
  dplyr::group_by(row, side) |>
  dplyr::summarize(dplyr::across(dplyr::all_of(od_feature_names), sum), .groups = "drop")

Z_home <- Z_side |> dplyr::filter(side == "home") |> dplyr::arrange(row)
Z_away <- Z_side |> dplyr::filter(side == "away") |> dplyr::arrange(row)

n <- nrow(x)
P <- ncol(x)
stopifnot(nrow(Z_home) == n, nrow(Z_away) == n)

# rows 1..n: home on offense; rows n+1..2n: away on offense
# defensive box columns enter with a minus sign, matching the -d_p in the player columns
Z_od <- rbind(
  cbind(as.matrix(Z_home[, off_feature_names]), -as.matrix(Z_away[, def_feature_names])),
  cbind(as.matrix(Z_away[, off_feature_names]), -as.matrix(Z_home[, def_feature_names]))
)
colnames(Z_od) <- od_feature_names

# player indicator matrices for home and away players
is_home <- X_data$value == 1
XH <- Matrix::sparseMatrix(i = X_data$row[is_home], j = X_data$column[is_home],
                           x = 1, dims = c(n, P))
XA <- Matrix::sparseMatrix(i = X_data$row[!is_home], j = X_data$column[!is_home],
                           x = 1, dims = c(n, P))

# columns: [offense players | defense players | home-offense indicator]
home_off <- c(rep(1, n), rep(0, n))
x_od <- Matrix::rbind2(
  Matrix::cbind2(XH, -XA),
  Matrix::cbind2(XA, -XH)
)
x_od <- Matrix::cbind2(x_od, Matrix::Matrix(home_off, ncol = 1, sparse = TRUE))

y_od <- c(100 * data$pts_h / data$poss, 100 * data$pts_a / data$poss)
w_od <- c(w, w)
gid_od <- c(data$game_id, data$game_id)

stopifnot(nrow(Z_od) == nrow(x_od), length(y_od) == nrow(x_od), !anyNA(y_od))
x_od_full <- Matrix::cbind2(x_od, Matrix::Matrix(Z_od, sparse = TRUE))
# ridge on the player columns and box coefficients, no penalty on the home indicator
pen_factor_od <- c(rep(1, 2 * P), 0, rep(c_penalty, ncol(Z_od)))

# 10-fold CV
oos_od <- rep(NA_real_, n)

for (k in seq_len(K_outer)) {
  te <- which(fold == k)
  tr <- which(fold != k)
  te_od <- c(te, te + n)
  tr_od <- c(tr, tr + n)
  
  f_od <- fit_ridge(x_od_full[tr_od, , drop = FALSE], y_od[tr_od], w_od[tr_od], gid_od[tr_od],
                    penalty_factor = pen_factor_od)
  p_od <- as.numeric(predict(f_od, newx = x_od_full[te_od, , drop = FALSE], s = "lambda.min"))
  oos_od[te] <- p_od[seq_along(te)] - p_od[length(te) + seq_along(te)]
  
  message("finished O/D fold ", k, " of ", K_outer)
}

fold_metrics_od <- do.call(rbind, lapply(seq_len(K_outer), function(k) {
  te <- which(fold == k)
  data.frame(fold = k, model = c("BAPM", "BAPM_OD"),
             rmse = c(wrmse(y[te], oos[te, "BAPM"], w[te]), wrmse(y[te], oos_od[te], w[te])),
             mae  = c(wmae(y[te], oos[te, "BAPM"], w[te]), wmae(y[te], oos_od[te], w[te])))
}))

cv_summary_od <- fold_metrics_od |>
  dplyr::group_by(model) |>
  dplyr::summarize(
    rmse_se = sd(rmse) / sqrt(dplyr::n()), rmse = mean(rmse),
    mae_se = sd(mae) / sqrt(dplyr::n()), mae = mean(mae),
    .groups = "drop"
  )

skill_od <- c(BAPM = skill[["BAPM"]],
              BAPM_OD = 1 - sum(w * (y - oos_od)^2) / sse_null)
cv_summary_od$oos_skill_vs_null <- skill_od[cv_summary_od$model]

cat("\nBAPM vs offense/defense BAPM (same folds, same scale)")
print(as.data.frame(cv_summary_od), digits = 4)

d_od <- fold_metrics_od$rmse[fold_metrics_od$model == "BAPM_OD"] -
  fold_metrics_od$rmse[fold_metrics_od$model == "BAPM"]
cat(sprintf("\nBAPM_OD - BAPM RMSE: mean %.4f, SE %.4f (negative favors BAPM_OD)\n",
            mean(d_od), sd(d_od) / sqrt(length(d_od))))

# --- Final fit on all data + offensive / defensive ratings per 100 possessions --
model_od <- fit_ridge(x_od_full, y_od, w_od, gid_od, penalty_factor = pen_factor_od)

joint_od <- coef(model_od, s = "lambda.min")[-1, 1]
delta_off <- joint_od[seq_len(P)]
delta_def <- joint_od[P + seq_len(P)]
home_adv <- joint_od[2 * P + 1]
c_od <- joint_od[(2 * P + 2):length(joint_od)]
names(c_od) <- od_feature_names
print(c_od)
cat(sprintf("Home-offense indicator (pts per 100): %.3f\n", home_adv))

c_off <- c_od[off_feature_names]
c_def <- c_od[def_feature_names]

# a player's prior is the possession-weighted average of c' z_{p,g} over their stints
# (raw rates, so centered at the league average; the constant is absorbed by the intercept)
Zc_off <- sweep(as.matrix(Z_long_od[, off_feature_names]), 2, feat_mean_od[off_feature_names], "-")
Zc_def <- sweep(as.matrix(Z_long_od[, def_feature_names]), 2, feat_mean_od[def_feature_names], "-")
prior_od_by_player <- Z_long_od |>
  dplyr::mutate(
    prior_off = as.numeric(Zc_off %*% c_off),
    prior_def = as.numeric(Zc_def %*% c_def)
  ) |>
  dplyr::group_by(athlete_id) |>
  dplyr::summarize(
    prior_off = weighted.mean(prior_off, poss),
    prior_def = weighted.mean(prior_def, poss),
    .groups = "drop"
  )

# overall BAPM from the original model, for comparison (obapm + dbapm should be close)
bapm_overall <- players |>
  dplyr::mutate(delta = delta) |>
  dplyr::left_join(prior_by_player, by = "athlete_id") |>
  dplyr::transmute(athlete_id, bapm = prior_bapm + delta)

athlete_ratings_od <- players |>
  dplyr::mutate(delta_off = delta_off, delta_def = delta_def) |>
  dplyr::left_join(prior_od_by_player, by = "athlete_id") |>
  dplyr::mutate(
    obapm = prior_off + delta_off,
    dbapm = prior_def + delta_def,
    bapm_od = obapm + dbapm
  ) |>
  dplyr::left_join(bapm_overall, by = "athlete_id") |>
  dplyr::left_join(athlete_summary, by = "athlete_id") |>
  dplyr::left_join(athlete, by = "athlete_id") |>
  dplyr::select(athlete_display_name, team_abbreviation, minutes, poss,
                prior_off, delta_off, obapm, prior_def, delta_def, dbapm, bapm_od, bapm)

print(head(dplyr::arrange(athlete_ratings_od, -obapm), 10), digits = 3)
print(head(dplyr::arrange(athlete_ratings_od, -dbapm), 10), digits = 3)
print(head(dplyr::arrange(athlete_ratings_od, -bapm_od), 10), digits = 3)

top10 <- function(df, col, min_poss = 500, n = 10) {
  df |>
    dplyr::filter(poss >= min_poss) |>
    dplyr::arrange(dplyr::desc(.data[[col]])) |>
    dplyr::slice_head(n = n) |>
    dplyr::select(athlete_display_name, team_abbreviation, poss, obapm, dbapm, bapm_od, bapm)
}

cat("\nTop 10 offensive BAPM\n");   print(top10(athlete_ratings_od, "obapm"),   digits = 3)
cat("\nTop 10 defensive BAPM\n");   print(top10(athlete_ratings_od, "dbapm"),   digits = 3)
cat("\nTop 10 overall BAPM (O + D)\n"); print(top10(athlete_ratings_od, "bapm_od"), digits = 3)

print(dplyr::arrange(dplyr::filter(athlete_ratings_od, team_abbreviation == "DAL"), -bapm_od), digits = 3)
print(dplyr::arrange(dplyr::filter(athlete_ratings_od, team_abbreviation == "LV"), -bapm_od), digits = 3)

cbind(c_bpm, c_coef)

# how correlated are the features?
round(cor(Z), 2)
# do extreme block rates come from thin windows?
plot(roll[, "poss_played"], box_mat[, "blk_per100"])
