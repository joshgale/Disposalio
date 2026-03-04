library(fitzRoy)
library(dplyr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(janitor)

# Define seasons
seasons <- c(2025, 2026)
current_season <- 2026

message("--- AFL DATA SYNC START ---")

# 1. Fetch player stats
message("Fetching player stats for seasons: ", paste(seasons, collapse = ", "))
# Use fryzigg for stats as it's very reliable for player-level data
player_stats <- fetch_player_stats(season = seasons, source = "fryzigg") %>% clean_names()
message("Player stats fetched. Rows: ", nrow(player_stats))

# 2. Fetch fixture
message("Fetching fixture for season: ", current_season)
# Use "AFL" as source because "fryzigg" does not support fetch_fixture
fixture <- fetch_fixture(season = current_season, source = "AFL") %>% clean_names()
message("Fixture fetched. Rows: ", nrow(fixture))

# 3. Standardize Date Columns
# Detect date column in stats
stats_date_col <- intersect(names(player_stats), c("match_date", "date", "utc_start_time", "start_time"))[1]
if (!is.na(stats_date_col)) {
  player_stats <- player_stats %>% mutate(match_date_parsed = as_datetime(.data[[stats_date_col]]))
}

# Detect date column in fixture
# AFL source usually uses 'start_time'
fix_date_col <- intersect(names(fixture), c("start_time", "utc_start_time", "match_date", "date"))[1]
if (!is.na(fix_date_col)) {
  fixture <- fixture %>% mutate(match_date_parsed = as_datetime(.data[[fix_date_col]]))
}

# 4. Standardize Disposals Column
if ("total_disposals" %in% names(player_stats) && !"disposals" %in% names(player_stats)) {
  player_stats <- player_stats %>% rename(disposals = total_disposals)
}

# 5. Identify Upcoming Opponents
now <- now(tzone = "Australia/Melbourne")
upcoming_fixture <- fixture %>%
  filter(match_date_parsed > now) %>%
  arrange(match_date_parsed)

# AFL source uses 'home_team_name' and 'away_team_name' usually
home_col <- intersect(names(fixture), c("home_team_name", "home_team"))[1]
away_col <- intersect(names(fixture), c("away_team_name", "away_team"))[1]

team_opponents <- bind_rows(
  upcoming_fixture %>% select(team = !!sym(home_col), opponent = !!sym(away_col), match_date_parsed),
  upcoming_fixture %>% select(team = !!sym(away_col), opponent = !!sym(home_col), match_date_parsed)
) %>%
  group_by(team) %>%
  arrange(match_date_parsed) %>%
  slice(1) %>%
  ungroup() %>%
  select(team, opponent)

# 6. Process Player Data
message("Processing player stats...")
processed_players <- player_stats %>%
  arrange(desc(match_date_parsed)) %>%
  group_by(player_id) %>%
  summarise(
    name = paste(first(player_first_name), first(player_last_name)),
    team = first(player_team),
    # Get last 20 disposals
    disposals = list(head(disposals, 20)),
    .groups = "drop"
  ) %>%
  # Calculate trends after summarising
  mutate(
    avg_3 = sapply(disposals, function(x) mean(head(unlist(x), 3), na.rm = TRUE)),
    avg_10 = sapply(disposals, function(x) mean(head(unlist(x), 10), na.rm = TRUE)),
    trend = case_when(
      avg_3 > (avg_10 + 1) ~ 1,
      avg_3 < (avg_10 - 1) ~ -1,
      TRUE ~ 0
    )
  )

# 7. Join and Finalize
# Note: Team names between Fryzigg and AFL source might differ slightly (e.g. "Adelaide Crows" vs "Adelaide")
# We'll do a basic cleaning to help the join
clean_team_name <- function(x) {
  x %>% 
    gsub(" Crows", "", .) %>%
    gsub(" Lions", "", .) %>%
    gsub(" Blues", "", .) %>%
    gsub(" Magpies", "", .) %>%
    gsub(" Bombers", "", .) %>%
    gsub(" Dockers", "", .) %>%
    gsub(" Cats", "", .) %>%
    gsub(" Suns", "", .) %>%
    gsub(" Giants", "", .) %>%
    gsub(" Hawks", "", .) %>%
    gsub(" Demons", "", .) %>%
    gsub(" Kangaroos", "", .) %>%
    gsub(" Power", "", .) %>%
    gsub(" Tigers", "", .) %>%
    gsub(" Saints", "", .) %>%
    gsub(" Swans", "", .) %>%
    gsub(" Eagles", "", .) %>%
    gsub(" Bulldogs", "", .) %>%
    trimws()
}

final_players <- processed_players %>%
  mutate(join_team = clean_team_name(team)) %>%
  left_join(
    team_opponents %>% mutate(join_team = clean_team_name(team)) %>% select(-team), 
    by = "join_team"
  ) %>%
  mutate(opponent = ifelse(is.na(opponent), "TBC", opponent)) %>%
  select(name, team, opponent, disposals, trend)

# 8. Export
if (!dir.exists("public/data")) {
  dir.create("public/data", recursive = TRUE)
}

message("Exporting to public/data/players.json...")
write_json(final_players, "public/data/players.json", pretty = TRUE)
message("--- AFL DATA SYNC COMPLETE ---")
