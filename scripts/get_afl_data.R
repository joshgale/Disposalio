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

# Standardized Team Names Mapping
standardize_team <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  x <- as.character(x)
  x %>% 
    gsub("Greater Western Sydney", "GWS Giants", ., ignore.case = TRUE) %>%
    gsub("GWS GIANTS", "GWS Giants", ., ignore.case = TRUE) %>%
    gsub("GWS", "GWS Giants", ., ignore.case = TRUE) %>%
    gsub("Gold Coast SUNS", "Gold Coast", ., ignore.case = TRUE) %>%
    gsub("Gold Coast Suns", "Gold Coast", ., ignore.case = TRUE) %>%
    gsub("Geelong Cats", "Geelong", ., ignore.case = TRUE) %>%
    gsub("Sydney Swans", "Sydney", ., ignore.case = TRUE) %>%
    gsub("Adelaide Crows", "Adelaide", ., ignore.case = TRUE) %>%
    gsub("Brisbane Lions", "Brisbane", ., ignore.case = TRUE) %>%
    gsub("Carlton Blues", "Carlton", ., ignore.case = TRUE) %>%
    gsub("Collingwood Magpies", "Collingwood", ., ignore.case = TRUE) %>%
    gsub("Essendon Bombers", "Essendon", ., ignore.case = TRUE) %>%
    gsub("Fremantle Dockers", "Fremantle", ., ignore.case = TRUE) %>%
    gsub("Hawthorn Hawks", "Hawthorn", ., ignore.case = TRUE) %>%
    gsub("Melbourne Demons", "Melbourne", ., ignore.case = TRUE) %>%
    gsub("North Melbourne Kangaroos", "North Melbourne", ., ignore.case = TRUE) %>%
    gsub("Port Adelaide Power", "Port Adelaide", ., ignore.case = TRUE) %>%
    gsub("Richmond Tigers", "Richmond", ., ignore.case = TRUE) %>%
    gsub("St Kilda Saints", "St Kilda", ., ignore.case = TRUE) %>%
    gsub("West Coast Eagles", "West Coast", ., ignore.case = TRUE) %>%
    gsub("Western Bulldogs", "Western Bulldogs", ., ignore.case = TRUE) %>%
    trimws()
}

# 1. Fetch player stats
message("Fetching player stats...")
player_stats <- fetch_player_stats(season = seasons, source = "fryzigg") %>% 
  clean_names() %>%
  mutate(player_team = standardize_team(player_team))

# 2. Fetch fixture
message("Fetching 2026 fixture...")
fixture <- fetch_fixture(season = current_season, source = "AFL") %>% 
  clean_names()

# 3. Fetch Lineups
message("Fetching current lineups...")
lineups <- tryCatch({
  raw_l <- fetch_lineup(season = current_season)
  if (!is.null(raw_l) && nrow(raw_l) > 0) {
    l_cleaned <- raw_l %>% clean_names()
    l_cleaned %>% mutate(team_name = standardize_team(team_name))
  } else {
    NULL
  }
}, error = function(e) {
  message("Warning: fetch_lineup failed: ", e$message)
  NULL
})

# 4. Standardize Date Columns
stats_date_col <- intersect(names(player_stats), c("match_date", "date", "utc_start_time", "start_time"))[1]
if (!is.na(stats_date_col)) {
  player_stats <- player_stats %>% mutate(match_date_parsed = as_datetime(.data[[stats_date_col]]))
}
fix_date_col <- intersect(names(fixture), c("start_time", "utc_start_time", "match_date", "date"))[1]
if (!is.na(fix_date_col)) {
  fixture <- fixture %>% mutate(match_date_parsed = as_datetime(.data[[fix_date_col]]))
}

# 5. Standardize Disposals Column
if ("total_disposals" %in% names(player_stats) && !"disposals" %in% names(player_stats)) {
  player_stats <- player_stats %>% rename(disposals = total_disposals)
}

# 6. Identify Upcoming Opponents
now <- now(tzone = "Australia/Melbourne")
upcoming_fixture <- fixture %>%
  filter(match_date_parsed > now) %>%
  arrange(match_date_parsed)

home_col <- intersect(names(fixture), c("home_team_name", "home_team"))[1]
away_col <- intersect(names(fixture), c("away_team_name", "away_team"))[1]

if (!is.na(home_col) && !is.na(away_col)) {
  team_opponents <- bind_rows(
    upcoming_fixture %>% select(team = !!sym(home_col), opponent = !!sym(away_col), match_date_parsed),
    upcoming_fixture %>% select(team = !!sym(away_col), opponent = !!sym(home_col), match_date_parsed)
  ) %>%
    mutate(
      team = standardize_team(team),
      opponent = standardize_team(opponent)
    ) %>%
    group_by(team) %>%
    arrange(match_date_parsed) %>%
    slice(1) %>%
    ungroup() %>%
    select(team, opponent)
} else {
  team_opponents <- tibble(team=character(), opponent=character())
}

# 7. Process Player Data
message("Processing player stats...")
processed_players <- player_stats %>%
  arrange(desc(match_date_parsed)) %>%
  group_by(player_id) %>%
  summarise(
    name = paste(first(player_first_name), first(player_last_name)),
    team = first(player_team),
    disposals = list(head(disposals, 20)),
    .groups = "drop"
  ) %>%
  mutate(
    avg_3 = sapply(disposals, function(x) mean(head(unlist(x), 3), na.rm = TRUE)),
    avg_10 = sapply(disposals, function(x) mean(head(unlist(x), 10), na.rm = TRUE)),
    trend = case_when(
      avg_3 > (avg_10 + 1) ~ 1,
      avg_3 < (avg_10 - 1) ~ -1,
      TRUE ~ 0
    )
  )

# 8. Determine 'Named' Status
if (!is.null(lineups) && nrow(lineups) > 0) {
  # Safe Player Name Mapping
  if ("player_player_name_given_name" %in% names(lineups) && "player_player_name_surname" %in% names(lineups)) {
    lineups$p_name_final <- paste(lineups$player_player_name_given_name, lineups$player_player_name_surname)
  } else if ("player_name" %in% names(lineups)) {
    lineups$p_name_final <- lineups$player_name
  } else if ("given_name" %in% names(lineups) && "surname" %in% names(lineups)) {
    lineups$p_name_final <- paste(lineups$given_name, lineups$surname)
  } else {
    lineups$p_name_final <- "Unknown"
  }
  
  lineups <- lineups %>%
    mutate(
      clean_pname = tolower(gsub("[^a-zA-Z]", "", p_name_final)),
      is_playing = ifelse(grepl("Selected|Interchange|Follower|Forward|Back|Midfield", status, ignore.case = TRUE) & 
                          !grepl("Emergency", status, ignore.case = TRUE), 1, -1)
    )
  
  # Group by player to handle multiple lineup entries if they exist
  lineups_status <- lineups %>%
    group_by(clean_pname) %>%
    summarise(lineup_status = max(is_playing), .groups = "drop")
  
  processed_players <- processed_players %>%
    mutate(clean_pname = tolower(gsub("[^a-zA-Z]", "", name))) %>%
    left_join(lineups_status, by = "clean_pname") %>%
    mutate(status = ifelse(is.na(lineup_status), 0, lineup_status)) %>%
    select(-clean_pname, -lineup_status)
} else {
  processed_players$status <- 0
}

# 9. Join Team Opponents
final_players <- processed_players %>%
  left_join(team_opponents, by = "team") %>%
  mutate(opponent = ifelse(is.na(opponent), "TBC", opponent)) %>%
  select(name, team, opponent, disposals, trend, status)

# 10. Export
if (!dir.exists("public/data")) {
  dir.create("public/data", recursive = TRUE)
}
message("Exporting to public/data/players.json...")
write_json(final_players, "public/data/players.json", pretty = TRUE)
message("--- AFL DATA SYNC COMPLETE ---")
