# ============================================================
# Demo: Bayer Leverkusen pressures + opponent ELO tiers (2023/2024)
# ============================================================

library(StatsBombR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggsoccer)
library(readr)

# ------------------------------------------------------------
# 0) Helper to get ClubElo rating for a club on a given date
# ------------------------------------------------------------

get_elo_on_date <- function(team, date) {
  date <- as.Date(date)
  
  url <- paste0("http://api.clubelo.com/", team)
  hist <- suppressWarnings(read_csv(url, show_col_types = FALSE))
  
  # ClubElo history typically has From/To date ranges per rating row.
  hist <- hist %>%
    mutate(
      From = as.Date(From),
      To   = as.Date(To)
    ) %>%
    arrange(From)
  
  # Find a row whose [From, To] interval contains the date
  in_range <- hist %>%
    filter((!is.na(From) & date >= From) & (is.na(To) | date <= To))
  
  if (nrow(in_range) >= 1) return(as.numeric(in_range$Elo[1]))
  
  # Fallback: last Elo before the date
  before <- hist %>% filter(!is.na(From) & From <= date)
  if (nrow(before) >= 1) return(as.numeric(before$Elo[nrow(before)]))
  
  NA_real_
}

# ------------------------------------------------------------
# 1) Get ALL matches from the target season
# ------------------------------------------------------------
comps <- FreeCompetitions() %>%
  filter(competition_id == 9, season_name == "2023/2024")

matches <- FreeMatches(comps)

# Keep only matches where Bayer Leverkusen played
leverkusen_matches <- matches %>%
  filter(
    home_team.home_team_name == "Bayer Leverkusen" |
      away_team.away_team_name == "Bayer Leverkusen"
  )

# ------------------------------------------------------------
# 2) Download ALL events for those matches (this may take minutes)
# ------------------------------------------------------------
cat("Downloading full-season event data...\n")
events_raw <- free_allevents(MatchesDF = leverkusen_matches)
events <- allclean(events_raw)  # includes cleaning/splitting locations etc. [web:2]

# ------------------------------------------------------------
# 3) Filter defensive actions: Pressures
#    (In StatsBomb data, event type is stored in type.name) [web:20]
# ------------------------------------------------------------
pressure_events <- events %>%
  filter(
    team.name == "Bayer Leverkusen",
    type.name == "Pressure",
    !is.na(location.x),
    !is.na(location.y)
  )

# ------------------------------------------------------------
# 4) Build match-level table: opponent + match date + ClubElo name
# ------------------------------------------------------------
match_info <- leverkusen_matches %>%
  mutate(
    opponent = ifelse(
      home_team.home_team_name == "Bayer Leverkusen",
      away_team.away_team_name,
      home_team.home_team_name
    ),
    match_date = as.Date(match_date),
    
    # StatsBomb team names -> ClubElo team names
    opponent_clubelo = case_when(
      opponent == "Bayern Munich" ~ "Bayern",
      opponent == "Borussia Dortmund" ~ "Dortmund",
      opponent == "RB Leipzig" ~ "RBLeipzig",
      opponent == "VfB Stuttgart" ~ "Stuttgart",
      opponent == "Eintracht Frankfurt" ~ "Frankfurt",
      opponent == "Borussia Mönchengladbach" ~ "Gladbach",
      opponent == "VfL Wolfsburg" ~ "Wolfsburg",
      opponent == "SC Freiburg" ~ "Freiburg",
      opponent == "FC Augsburg" ~ "Augsburg",
      opponent == "TSG Hoffenheim" ~ "Hoffenheim",
      opponent == "Werder Bremen" ~ "Werder",
      opponent == "FC Heidenheim" ~ "Heidenheim",
      opponent == "Darmstadt 98" ~ "Darmstadt",
      opponent == "FC Köln" ~ "Koeln",
      opponent == "FSV Mainz 05" ~ "Mainz",
      opponent == "VfL Bochum 1848" ~ "Bochum",
      TRUE ~ gsub(" ", "", opponent)
    )
  ) %>%
  select(match_id, match_date, opponent, opponent_clubelo)

# ------------------------------------------------------------
# 5) Fetch opponent ELO on the match date
# ------------------------------------------------------------
cat("Fetching opponent ELO ratings...\n")
# Note: get_elo_on_date() must exist in your environment/package.
match_info <- match_info %>%
  rowwise() %>%
  mutate(opponent_elo = get_elo_on_date(opponent_clubelo, match_date)) %>%
  ungroup()

library(readr)
library(dplyr)
library(lubridate)

# ------------------------------------------------------------
# 5A) Compute fixed Bundesliga thresholds from an anchor date
# ------------------------------------------------------------
anchor_date <- as.Date("2023-08-18")  # pick the pre-MD1 anchor you want

# ClubElo "all teams by date" CSV API
# (returns Rank, Club, Country, Level, Elo, From, To)
clubelo_anchor <- readr::read_csv(
  paste0("http://api.clubelo.com/", format(anchor_date, "%Y-%m-%d")),
  show_col_types = FALSE
)

bundesliga_anchor <- clubelo_anchor %>%
  filter(
    Country == "GER",
    Level == 1,
    as.Date(From) <= anchor_date,
    as.Date(To) >= anchor_date
  )

stopifnot(nrow(bundesliga_anchor) == 18)

elo_cut <- quantile(
  bundesliga_anchor$Elo,
  probs = c(1/3, 2/3),
  type = 7,        # R default; keep explicit
  na.rm = TRUE
)

p33 <- unname(elo_cut[[1]])
p67 <- unname(elo_cut[[2]])

cat("Bundesliga fixed thresholds (anchor =", as.character(anchor_date), "):\n")
cat("P33 =", round(p33, 2), " P67 =", round(p67, 2), "\n")

# ------------------------------------------------------------
# 6) Join ELO onto each pressure event + create ELO tier
# ------------------------------------------------------------
pressure_events_final <- pressure_events %>%
  left_join(match_info, by = "match_id") %>%
  mutate(
    elo_tier = case_when(
      is.na(opponent_elo) ~ "Unknown",
      opponent_elo < p33  ~ "Weak opponent",
      opponent_elo < p67  ~ "Mid opponent",
      TRUE                ~ "Top opponent"
    )
  )

pressure_events_final <- pressure_events_final %>%
  mutate(
    elo_tier = factor(elo_tier, levels = c("Top opponent", "Mid opponent", "Weak opponent"))
  )

# ------------------------------------------------------------
# 7) Heatmap: pressure density by opponent tier
# ------------------------------------------------------------
ggplot(pressure_events_final) +
  stat_density_2d(
    aes(x = location.x, y = location.y, fill = after_stat(ndensity)),
    geom = "raster", contour = FALSE, n = 200, h = c(15, 15)
  ) +
  scale_fill_viridis_c(
    option = "magma", direction = -1, begin = 0.05,
    name = "Relative\nDensity"
  ) +
  # White mask so density does not appear outside the pitch boundaries
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = "white", color = NA) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 80, ymax = Inf, fill = "white", color = NA) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "white", color = NA) +
  annotate("rect", xmin = 120, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white", color = NA) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "grey30", fill = NA) +
  theme_pitch() +  # from ggsoccer [web:15]
  annotate(
    "segment",
    x = 30, xend = 90, y = -5, yend = -5,
    colour = "grey30", linewidth = 1,
    arrow = arrow(length = unit(0.3, "cm"), type = "closed")
  ) +
  annotate(
    "text",
    x = 60, y = -9, label = "Attacking direction",
    colour = "grey30", size = 4, fontface = "italic"
  ) +
  coord_fixed(xlim = c(-5, 125), ylim = c(-12, 85), expand = FALSE) +
  labs(
    title = "Bayer Leverkusen: Pressure Zones by Opponent Strength",
    subtitle = "Relative spatial density (normalized; faceted by opponent ELO tier)"
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "grey20"),
    legend.position = "right"
  ) +
  facet_wrap(~ elo_tier, ncol = 3)

# ------------------------------------------------------------
# 8) Match-level metrics + averages by opponent tier
# ------------------------------------------------------------
match_metrics <- pressure_events_final %>%
  group_by(match_id, elo_tier) %>%
  summarise(
    total_pressures = n(),
    dispersion_x = sd(location.x, na.rm = TRUE),
    dispersion_y = sd(location.y, na.rm = TRUE),
    .groups = "drop"
  )

dispersion_analysis <- match_metrics %>%
  group_by(elo_tier) %>%
  summarise(
    matches_played = n(),
    pressures_per_match = round(mean(total_pressures), 1),
    dispersion_length_x = round(mean(dispersion_x), 1),
    dispersion_width_y = round(mean(dispersion_y), 1),
    area_dispersion_index = dispersion_length_x * dispersion_width_y,
    .groups = "drop"
  ) %>%
  arrange(desc(pressures_per_match))

print(dispersion_analysis)

# Optional export:
# ggsave("Leverkusen_Pressure_Heatmap.png", width = 15, height = 5.5, dpi = 300)