library(tidyverse)
library(readr)

gameOutcomesRaw <- read.csv("nfl_mahomes_era_games.csv")
teamStatisticsRaw <- read.csv("nfl-team-statistics.csv")

View(gameOutcomesRaw)
View(teamStatisticsRaw)

gameOutcomesClean <- gameOutcomesRaw |> 
  filter(season %in% c(2018, 2019, 2020, 2021, 2022))

teamStatisticsCleaned <- teamStatisticsRaw |> 
  filter(season %in% c(2018, 2019, 2020, 2021, 2022)) |> 
  select(season, team, starts_with("offense_ave"))
