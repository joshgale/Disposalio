library(fitzRoy)
library(dplyr)
library(jsonlite)
library(tidyr)

# Set the season to 2026 as requested
season <- 2026

# 1. Fetch player stats (Fryzigg source)
message("Fetching player stats...")
# Note: In a real scenario, we might need to handle cases where 2026 hasn't started.
# For this script, we assume fitzRoy supports the 2026 season data.
player_stats <- fetch_player_stats(season = season, source = "fryzigg")

# 2. Fetch fixture to identify upcoming opponents
message("Fetching fixture...")
fixture <- fetch_fixture(season = season)

# Get the next upcoming game for each team
# Filter for games that haven't happened yet (status "scheduled" or "future")
# Usually, status is NA or "Scheduled" for upcoming games.
# We'll look for the first game for each team where the start time is in the future.
now <- Sys.time()
upcoming_fixture <- fixture %>%
  filter(as.POSIXct(date) > now) %>%
  arrange(date)

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
# We need: Name, Team, Opponent, Disposals (last 20), and Trend
# Group by player and get their disposal history
processed_players <- player_stats %>%
  arrange(desc(match_date)) %>%
  group_by(player_id, player_first_name, player_last_name, player_team) %>%
  summarise(
    name = paste(first(player_first_name), first(player_last_name)),
    team = first(player_team),
    # Get last 20 disposals (ordered by most recent first)
    disposals = list(head(disposals, 20)),
    # Calculate Trend: Avg(Last 3) vs Avg(Last 10)
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

# Join with upcoming opponent
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
