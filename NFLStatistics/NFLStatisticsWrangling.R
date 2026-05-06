library(tidyverse)
library(readr)
library(ggplot2)
library(gt)

#Loading in our datasets
##This includes Game outcomes data and Team statistics data
gameOutcomesRaw <- read.csv("nfl_mahomes_era_games.csv")
teamStatisticsRaw <- read.csv("nfl-team-statistics.csv")

#Filtertinr the Raw data to show only years 2018 to 2022
gameOutcomesClean <- gameOutcomesRaw |> 
  filter(season %in% c(2018, 2019, 2020, 2021, 2022))

#Filtertinr the Raw data to show only years 2018 to 2022 and selecting only offensive columns 
#as well as specific team and season column
teamStatisticsCleaned <- teamStatisticsRaw |> 
  filter(season %in% c(2018, 2019, 2020, 2021, 2022)) |> 
  select(season, team, starts_with("offense_ave"))

# Convert games into team-level outcomes
teamGameOutcomes <- gameOutcomesClean |> 
  select(season, home_team, away_team, home_score, away_score) |> 
  
  # Home team rows
  transmute(
    season,
    team = home_team,
    result = if_else(home_score > away_score, "Win", "Loss")
  ) |> 
  
  # Add away team rows
  bind_rows(
    gameOutcomesClean |> 
      select(season, home_team, away_team, home_score, away_score) |> 
      transmute(
        season,
        team = away_team,
        result = if_else(away_score > home_score, "Win", "Loss")
      )
  )

# Create team scoring table
teamScoring <- gameOutcomesClean |> 
  
  # Home team points
  transmute(
    season,
    team = home_team,
    points_scored = home_score
  ) |> 
  
  # Add away team points
  bind_rows(
    gameOutcomesClean |> 
      transmute(
        season,
        team = away_team,
        points_scored = away_score
      )
  ) |> 
  
  # Average by team-season
  group_by(season, team) |> 
  summarise(
    offense_ave_points = mean(points_scored, na.rm = TRUE)
  )

# Joining with offensive stats
teamPerformance <- teamGameOutcomes |> 
  left_join(teamStatisticsCleaned, by = c("season", "team")) |>
  left_join(teamScoring, by = c("season", "team"))


# Create summary table
summaryTable <- teamPerformance |> 
  group_by(result) |> 
  summarise(
    Avg_Points = mean(offense_ave_points, na.rm = TRUE),
    Avg_Pass_Yards = mean(offense_ave_yards_gained_pass, na.rm = TRUE),
    Avg_Run_Yards = mean(offense_ave_yards_gained_run, na.rm = TRUE),
    .groups = "drop"
  )


# Create plot
teamWinRates <- teamGameOutcomes |> 
  group_by(season, team) |> 
  summarise(
    wins = sum(result == "Win"),
    games = n(),
    win_pct = wins / games
  )

teamSeasonAnalysis <- teamWinRates |> 
  left_join(teamStatisticsCleaned, by = c("season", "team")) |>
  left_join(teamScoring, by = c("season", "team"))


ggplot(teamSeasonAnalysis, aes(x = offense_ave_points, y = win_pct)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Offensive Scoring and Win Percentage",
    x = "Average Offensive Points",
    y = "Win Percentage"
  ) +
  theme_minimal()

ggplot(teamSeasonAnalysis, aes(x = offense_ave_yards_gained_pass, y = win_pct)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Offensive Yards Gained (Pass) and Win Percentage",
    x = "Average Passing Yards",
    y = "Win Percentage"
  ) +
  theme_minimal()

ggplot(teamSeasonAnalysis, aes(x = offense_ave_yards_gained_run, y = win_pct)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Offensive Yards Gained (Run) and Win Percentage",
    x = "Average Running Yards",
    y = "Win Percentage"
  ) +
  theme_minimal()



