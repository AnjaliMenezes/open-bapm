library(glmnet)
library(Matrix)

# BAPM: box-score-augmented plus-minus
#
# Minimizes
#   sum_i w_i (y_i - b0 - x_i' beta_adj - z_i' c)^2 + lambda * (||beta_adj||^2 + kappa * ||c||^2)
#
#   y_i  = 100 * score_diff_i / poss_i      (home margin per 100 possessions)
#   w_i  = poss_i
#   x_i  = +1 for each home player on court, -1 for each away player
#   z_i  = sum of home players' box metrics - sum of away players' box metrics
#   b0   = unpenalized intercept (home-court advantage)
#
# Arguments
#   stints      data.frame, one row per stint, with columns
#                 game_id, poss, score_diff, home_1..home_5, away_1..away_5
#               (score_diff is home minus away over the stint)
#   player_box  data.frame of player box metrics (already standardized), with
#                 athlete_id, [game_id], and one column per metric.
#               If game_id is present, metrics are matched per player-game
#               (e.g. rolling priors); otherwise one row per player.
#               Players with no matching row get z = 0.
#   kappa       relative ridge penalty on c (c_penalty in the original script)
#   nfolds      CV folds for lambda; folds are made by game
#   box_features  metric columns to use; defaults to every non-id column
#
# Returns
#   cv.glmnet object (alpha = 0) with an extra element $bapm holding the
#   player ids and feature names in column order of the design matrix.

BAPM <- function(stints, player_box, kappa = 1, nfolds = 5,
                 box_features = setdiff(names(player_box), c("game_id", "athlete_id"))) {
  stints <- stints[stints$poss > 0, , drop = FALSE]
  n <- nrow(stints)
  game_id <- as.character(stints$game_id)

  # Outcome and weights
  y <- 100 * stints$score_diff / stints$poss
  w <- stints$poss

  # Lineups to long form: one row per (stint, player slot)
  lineup_cols <- grep("^(home|away)_", names(stints), value = TRUE)
  long <- data.frame(
    row        = rep(seq_len(n), times = length(lineup_cols)),
    game_id    = rep(game_id, times = length(lineup_cols)),
    athlete_id = as.character(unlist(stints[lineup_cols], use.names = FALSE)),
    sign       = rep(ifelse(startsWith(lineup_cols, "home"), 1, -1), each = n)
  )

  # X: stint x player, +1 home / -1 away
  players <- sort(unique(long$athlete_id))
  X <- Matrix::sparseMatrix(
    i = long$row, j = match(long$athlete_id, players), x = long$sign,
    dims = c(n, length(players)), dimnames = list(NULL, players)
  )

  # Z: stint x metric, signed sum of on-court players' box metrics
  by_game <- "game_id" %in% names(player_box)
  box_key <- if (by_game) {
    paste(player_box$game_id, player_box$athlete_id)
  } else {
    as.character(player_box$athlete_id)
  }
  long_key <- if (by_game) paste(long$game_id, long$athlete_id) else long$athlete_id
  keep <- !duplicated(box_key)
  zp <- as.matrix(player_box[keep, box_features, drop = FALSE])[match(long_key, box_key[keep]), , drop = FALSE]
  zp[is.na(zp)] <- 0
  Z <- rowsum(zp * long$sign, long$row, reorder = TRUE)
  colnames(Z) <- box_features

  # Joint design [X | Z]; c gets penalty kappa relative to beta_adj
  x_full <- Matrix::cbind2(X, Matrix::Matrix(Z, sparse = TRUE))
  pen_factor <- c(rep(1, ncol(X)), rep(kappa, ncol(Z)))

  # Ridge, lambda by CV with folds by game
  gids <- unique(game_id)
  gf <- setNames(sample(rep(seq_len(nfolds), length.out = length(gids))), gids)
  fit <- glmnet::cv.glmnet(
    x = x_full, y = y, weights = w, alpha = 0, standardize = FALSE,
    foldid = unname(gf[game_id]), penalty.factor = pen_factor
  )

  fit$bapm <- list(players = players, box_features = box_features, kappa = kappa)
  fit
}

# Example
# set.seed(2025)
# fit    <- BAPM(stints, z_pg, kappa = 1)
# b      <- coef(fit, s = "lambda.min")[, 1]
# b0     <- b[1]
# beta   <- b[fit$bapm$players]          # player adjustments (delta)
# c_coef <- b[fit$bapm$box_features]     # box-score weights
