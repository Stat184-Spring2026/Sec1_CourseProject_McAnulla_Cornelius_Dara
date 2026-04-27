library(tidyverse)
library(readr)

#Loading in our datasets
##This includes Game outcomes data and Team statistics data
gameOutcomesRaw <- read.csv("nfl_mahomes_era_games.csv")
teamStatisticsRaw <- read.csv("nfl-team-statistics.csv")

#Viewing the tables
View(gameOutcomesRaw)
View(teamStatisticsRaw)

#Filtertinr the Raw data to show only years 2018 to 2022
gameOutcomesClean <- gameOutcomesRaw |> 
  filter(season %in% c(2018, 2019, 2020, 2021, 2022))

#Filtertinr the Raw data to show only years 2018 to 2022 and selecting only offensive columns 
#as well as specific team and season column
teamStatisticsCleaned <- teamStatisticsRaw |> 
  filter(season %in% c(2018, 2019, 2020, 2021, 2022)) |> 
  select(season, team, starts_with("offense_ave"))
