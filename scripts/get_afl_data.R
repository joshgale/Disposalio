library(fitzRoy)
library(dplyr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(janitor)

# Define seasons to fetch
# We include 2025 to ensure we have a full history of 20 games for the start of 2026
seasons <- c(2025, 2026)
current_season <- 2026

# 1. Fetch player stats (Fryzigg source) for both seasons
message("Fetching player stats for 2025 and 2026...")
player_stats <- fetch_player_stats(season = seasons, source = "fryzigg")

# 2. Fetch fixture (Fryzigg source) for the current season
message("Fetching 2026 fixture...")
fixture <- fetch_fixture(season = current_season, source = "fryzigg") %>% 
  clean_names()

# Get the next upcoming game for each team
now <- now(tzone = "Australia/Melbourne")

# Ensure we have a proper datetime object for filtering
fixture <- fixture %>%
  mutate(match_date = as_datetime(date))

upcoming_fixture <- fixture %>%
  filter(match_date > now) %>%
  arrange(match_date)

# Create a mapping of Team -> Upcoming Opponent
home_opponents <- upcoming_fixture %>%
  select(team = home_team, opponent = away_team) %>%
  group_by(team) %>%
  slice(1)

away_opponents <- upcoming_fixture %>%
  select(team = away_team, opponent = home_team) %>%
  group_by(team) %>%
  slice(1)

# Combined mapping
team_opponents <- bind_rows(home_opponents, away_opponents) %>%
  group_by(team) %>%
  slice(1) %>%
  ungroup()

# 3. Process Player Data
message("Processing player data (combining 2025 and 2026 stats)...")
# We arrange by match_date DESC so that 'head' always gets the most recent games
processed_players <- player_stats %>%
  arrange(desc(match_date)) %>%
  group_by(player_id) %>%
  summarise(
    name = paste(first(player_first_name), first(player_last_name)),
    # Use the team from their most recent game
    team = first(player_team),
    # Get last 20 disposals across both seasons
    disposals = list(head(disposals, 20)),
    # Calculate Trend based on the last 3 vs last 10 available games
    avg_3 = mean(head(disposals, 3), na.rm = TRUE),
    avg_10 = mean(head(disposals, 10), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    trend = case_when(
      avg_3 > (avg_10 + 1) ~ 1,
      avg_3 < (avg_10 - 1) ~ -1,
      TRUE ~ 0
    )
  )

# Join with upcoming opponent from the 2026 fixture
final_players <- processed_players %>%
  left_join(team_opponents, by = c("team" = "team")) %>%
  mutate(opponent = ifelse(is.na(opponent), "TBC", opponent)) %>%
  select(name, team, opponent, disposals, trend)

# Ensure the directory exists
if (!dir.exists("public/data")) {
  dir.create("public/data", recursive = TRUE)
}

# 4. Export to JSON
message("Exporting to public/data/players.json...")
write_json(final_players, "public/data/players.json", pretty = TRUE)
message("Done!")
