# Load Libraries

library(ggplot2)
library(ggrepel)
library(ggridges)

# Create destination folder if it doesn't already exist
dir.create("output/plots", recursive = TRUE, showWarnings = FALSE)

# Plot 1.Box Prior vs. Lineup Residual
plot_data <- athlete_ratings |> 
  dplyr::filter(poss >= 500)

# 2. Build Plot #1 and assign it to object 'p1'
p1 <- ggplot(plot_data, aes(x = prior_bapm, y = delta, size = poss, color = bapm)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.7) +
  geom_text_repel(
    data = dplyr::filter(plot_data, bapm > 4 | bapm < -2 | abs(delta) > 3),
    aes(label = athlete_display_name),
    size = 3.5,
    max.overlaps = 15
  ) +
  scale_color_viridis_c(option = "magma") +
  scale_size_continuous(range = c(2, 7)) +
  theme_minimal() +
  labs(
    title = "BAPM Component Breakdown",
    subtitle = "Separating Box Score Prior from Lineup Residual Adjustment (\u03b4)",
    x = "Box Score Prior (Expected Impact per 100 Possessions)",
    y = "Lineup Residual \u03b4 (Un-stat-tracked Impact per 100 Possessions)",
    color = "Total BAPM",
    size = "Possessions"
  )


# Export to PNG
ggsave(
  filename = "output/plots/bapm_component_breakdown.png",
  plot = p1,
  width = 10,        # Width in inches
  height = 7,        # Height in inches
  dpi = 300          # High-resolution (300 DPI) for crisp text and graphics
)

#2 Pure RAPM vs. BAPM Shrinkage

ggplot(plot_data, aes(x = rapm, y = bapm)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.6) +
  geom_point(aes(size = poss, color = team_abbreviation), alpha = 0.8) +
  geom_text_repel(
    data = dplyr::filter(plot_data, abs(rapm - bapm) > 2.5),
    aes(label = athlete_display_name),
    size = 3.5
  ) +
  theme_minimal() +
  labs(
    title = "RAPM vs. BAPM Impact Ratings",
    subtitle = "Dashed red line represents equality (RAPM = BAPM)",
    x = "Pure RAPM (pts / 100 poss)",
    y = "Box-Adjusted Plus-Minus (pts / 100 poss)",
    size = "Possessions",
    color = "Team"
  )

# Export to PNG
ggsave(
  filename = "output/plots/rapm_vs_bapm.png",
  plot = p4,
  width = 10,        # Width in inches
  height = 7,        # Height in inches
  dpi = 300          # High-resolution (300 DPI)
)

#3. Box Score Weights BPM vs BAPM
coef_df <- data.frame(
  feature = rep(box_feature_names, 2),
  model = c(rep("BPM (WLS)", length(box_feature_names)), rep("BAPM (Joint Ridge)", length(box_feature_names))),
  weight = c(c_bpm, c_coef)
)

ggplot(coef_df, aes(x = reorder(feature, weight), y = weight, fill = model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey50") +
  theme_minimal() +
  labs(
    title = "Box Score Feature Weights Across Models",
    subtitle = "Impact per 1 standard deviation change in standardized rate stats",
    x = "Standardized Feature (z-score)",
    y = "Model Coefficient (pts / 100 poss)",
    fill = "Model"
  )

#4 BAPM Density by WNBA Team

p2_team <- athlete_ratings |>
  dplyr::filter(poss >= 300) |>
  ggplot(aes(x = reorder(team_abbreviation, bapm, FUN = median), y = bapm, fill = team_abbreviation)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(aes(size = poss), width = 0.15, alpha = 0.7, color = "black", show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "WNBA Team Talent Profiles & Depth",
    subtitle = "BAPM Distribution by Team (Players with \u2265 300 Possessions)",
    x = "Team",
    y = "BAPM (Points per 100 Possessions)"
  )

p2_team

ggsave("output/plots/team_bapm_density.png", plot = p6, width = 10, height = 8, dpi = 300)

#5 Minutes Played vs BAPM

p7 <- athlete_ratings |>
  dplyr::filter(poss >= 200) |>
  ggplot(aes(x = minutes, y = bapm, size = poss, color = pm_per100)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.75) +
  ggrepel::geom_text_repel(
    data = dplyr::filter(athlete_ratings, bapm > 5.0 | (minutes > 1200 & bapm > 3.0)),
    aes(label = athlete_display_name),
    size = 3.5,
    max.overlaps = 12
  ) +
  scale_color_gradient2(low = "firebrick", mid = "grey80", high = "navy", midpoint = 0) +
  theme_minimal() +
  labs(
    title = "Workload vs. Overall Impact",
    subtitle = "Minutes Played vs. Box-Adjusted Plus-Minus (BAPM)",
    x = "Total Minutes Played",
    y = "BAPM (Points per 100 Possessions)",
    color = "Raw On-Court +/-",
    size = "Possessions"
  )

p7

ggsave("output/plots/workload_vs_bapm.png", plot = p7, width = 10, height = 7, dpi = 300)