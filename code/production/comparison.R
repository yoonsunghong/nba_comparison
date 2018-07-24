# ===================================================================
# Title: comparison.R
# Description:
#   This script takes in boxscore data, and puts out comparison
#   statistics report between the specific game and season average.
#   Season average excludes all games played between the two teams
#   that are involved in each designated game's report.
#   e.g. if the report considers one GSW vs. LAL game, it excludes
#   all GSW vs. LAL games for the season average calculation.
# Input: 2017-NBA-BOXSCORE.csv
# Output: game1.csv, game2.csv, and more for each team
# Author: Yoon Sung Hong
# Date: 12-27-2017
# ===================================================================

rm(list=ls())
library(dplyr)

rootPath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/compison/"
savePath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/comparison/report/"
#dir.create(savePath)

nbaPath <- "/Users/yoonsunghong/previous-projects/R-projects/NBA/data/" 

#===== Reading files from directory ======================================
#having two seperate dataframes: one for average statistics and another for each game. 
dat <- read.csv(paste0(nbaPath, "2017-NBA-BOXSCORE.csv"),stringsAsFactors = F)
game <- read.csv(paste0(nbaPath, "2017-NBA-BOXSCORE.csv"),stringsAsFactors = F)
teamdat <- read.csv(paste0(nbaPath, "2017-NBA-BOXSCORE.csv"),stringsAsFactors = F)
boundname <- which(colnames(game) == "Game.Number")
#finding the bounds of data we're interested in
bound1 <- which(colnames(dat) == "Game.Msg_boxscore.Team_stats.Team_id")
bound2 <- which(colnames(dat) == "Game.Msg_boxscore.Team_stats.SecondChancePointsAttempted")
dat <- dat[,bound1:bound2]
game <- cbind(game[,1], dat)
#assigning more appropriate names for columns
name <- c("team_id", 
           "team_city", 
           "team_abr", 
           "team_name", 
           "minutes", 
           "fg_made", 
           "fg_attempted", 
           "ft_made", 
           "ft_attempted", 
           "three_made", 
           "three_attempted", 
           "offensive_rebounds", 
           "defensive_rebounds", 
           "team_rebounds", 
           "assists", 
           "fouls", 
           "team_fouls", 
           "steals", 
           "turnovers", 
           "team_turnovers", 
           "blocks", 
           "points", 
           "flagrant_fouls", 
           "technical_fouls", 
           "ejections", 
           "blocks_agsinst", 
           "full_timeouts_remaining", 
           "short_timeouts_remaining", 
           "pointsoffturnovers", 
           "unanswered_points", 
           "longest_run", 
           "total_turnovers", 
           "fastbreak_points", 
           "fastbreak_points_made", 
           "fastbreak_points_attempted", 
           "points_in_the_paint", 
           "points_in_the_paint_made", 
           "points_in_the_paint_attempted", 
           "second_chance_points", 
           "second_chance_points_made", 
           "second_chance_points_attempted")
colnames(dat) <- name
#assigning more appropriate names for columns
colnames(game) <- c("gameno",
                    "team_id", 
                    "team_city", 
                    "team_abr", 
                    "team_name", 
                    "minutes", 
                    "fg_made", 
                    "fg_attempted", 
                    "ft_made", 
                    "ft_attempted", 
                    "three_made", 
                    "three_attempted", 
                    "offensive_rebounds", 
                    "defensive_rebounds", 
                    "team_rebounds", 
                    "assists", 
                    "fouls", 
                    "team_fouls", 
                    "steals", 
                    "turnovers", 
                    "team_turnovers", 
                    "blocks", 
                    "points", 
                    "flagrant_fouls", 
                    "technical_fouls", 
                    "ejections", 
                    "blocks_agsinst", 
                    "full_timeouts_remaining", 
                    "short_timeouts_remaining", 
                    "pointsoffturnovers", 
                    "unanswered_points", 
                    "longest_run", 
                    "total_turnovers", 
                    "fastbreak_points", 
                    "fastbreak_points_made", 
                    "fastbreak_points_attempted", 
                    "points_in_the_paint", 
                    "points_in_the_paint_made", 
                    "points_in_the_paint_attempted", 
                    "second_chance_points", 
                    "second_chance_points_made", 
                    "second_chance_points_attempted")
dat <- na.omit(dat)
game <- na.omit(game)
dat$pointsallowed <- rep(0,nrow(dat))
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$pointsallowed[i] <- dat$points[i+1]
  }
  else if(i %% 2 == 0) {
    dat$pointsallowed[i] <- dat$points[i-1]
  }
}
dat$rebounds <- dat$offensive_rebounds + dat$defensive_rebounds
dat$fgp <- (dat$fg_made/dat$fg_attempted)*100
dat$tpp <- (dat$three_made/dat$three_attempted)*100
dat$offensivereboundsopp <- rep(0,nrow(dat))
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$offensivereboundsopp[i] <- dat$offensive_rebounds[i+1]
  }
  else if(i %% 2 == 0) {
    dat$offensivereboundsopp[i] <- dat$offensive_rebounds[i-1]
  }
}
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$defensivereboundsopp[i] <- dat$defensive_rebounds[i+1]
  }
  else if(i %% 2 == 0) {
    dat$defensivereboundsopp[i] <- dat$defensive_rebounds[i-1]
  }
}
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$fgaopp[i] <- dat$fg_attempted[i+1]
  }
  else if(i %% 2 == 0) {
    dat$fgaopp[i] <- dat$fg_attempted[i-1]
  }
}
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$ftaopp[i] <- dat$ft_attempted[i+1]
  }
  else if(i %% 2 == 0) {
    dat$ftaopp[i] <- dat$ft_attempted[i-1]
  }
}
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$fgmopp[i] <- dat$fg_made[i+1]
  }
  else if(i %% 2 == 0) {
    dat$fgmopp[i] <- dat$fg_made[i-1]
  }
}
for(i in 1:nrow(dat)) {
  if(i %% 2 == 1) {
    dat$turnoveropp[i] <- dat$turnovers[i+1]
  }
  else if(i %% 2 == 0) {
    dat$turnoveropp[i] <- dat$turnovers[i-1]
  }
}
#game
game$pointsallowed <- rep(0,nrow(game))
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$pointsallowed[i] <- game$points[i+1]
  }
  else if(i %% 2 == 0) {
    game$pointsallowed[i] <- game$points[i-1]
  }
}
game$rebounds <- game$offensive_rebounds + game$defensive_rebounds
game$fgp <- round((game$fg_made/game$fg_attempted)*100,1)
game$tpp <- round((game$three_made/game$three_attempted)*100,1)
game$offensivereboundsopp <- rep(0,nrow(game))
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$offensivereboundsopp[i] <- game$offensive_rebounds[i+1]
  }
  else if(i %% 2 == 0) {
    game$offensivereboundsopp[i] <- game$offensive_rebounds[i-1]
  }
}
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$defensivereboundsopp[i] <- game$defensive_rebounds[i+1]
  }
  else if(i %% 2 == 0) {
    game$defensivereboundsopp[i] <- game$defensive_rebounds[i-1]
  }
}
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$fgaopp[i] <- game$fg_attempted[i+1]
  }
  else if(i %% 2 == 0) {
    game$fgaopp[i] <- game$fg_attempted[i-1]
  }
}
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$ftaopp[i] <- game$ft_attempted[i+1]
  }
  else if(i %% 2 == 0) {
    game$ftaopp[i] <- game$ft_attempted[i-1]
  }
}
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$fgmopp[i] <- game$fg_made[i+1]
  }
  else if(i %% 2 == 0) {
    game$fgmopp[i] <- game$fg_made[i-1]
  }
}
for(i in 1:nrow(game)) {
  if(i %% 2 == 1) {
    game$turnoveropp[i] <- game$turnovers[i+1]
  }
  else if(i %% 2 == 0) {
    game$turnoveropp[i] <- game$turnovers[i-1]
  }
}


#defining vector of teams for for loops later
tabrev <- c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
            "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
            "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

#average statistics have been done. Let's now work on per game stats
#selecting the needed variables
game <- game %>%
  select(gameno, team_abr, points, pointsallowed, assists, offensive_rebounds, rebounds,
         pointsoffturnovers, second_chance_points, fastbreak_points, points_in_the_paint,
         fgp, tpp, ft_attempted, turnovers)
#splitting the data by teams (this makes a list)
setup <- split(game, game$team_abr)

#splitting by game
for (i in 1:30) {
  dff <- setup[[i]]
  gameno <- dff$gameno
  n <- nrow(dff)
  #teamname for filename purposes
  teamname <- as.character(tabrev[i])
  for (j in 1:n) {
    #getting each game's data
    gamedat <- dff[j,-1:-2]
    #extracting game number
    gamenumber <- dff[j, 1]
    #using this to get the team names (strings)
    teamstring <- names(which(unlist(setup) == gamenumber))
    #substringing to first three to get abbreviations
    teamstring <- substr(teamstring, 1, 3)
    comp1 <- setup[[which(tabrev == teamstring[which(teamstring != teamname)])]]$gameno
    #removing non-fitting games
    dff2 <- dff[(!(gameno %in% comp1)),]
    avgstat <- c(round(mean(dff2$points),1),
                 round(mean(dff2$pointsallowed),1),
                 round(mean(dff2$assists),1),
                 round(mean(dff2$offensive_rebounds),1),
                 round(mean(dff2$rebounds),1),
                 round(mean(dff2$pointsoffturnovers),1),
                 round(mean(dff2$second_chance_points),1),
                 round(mean(dff2$fastbreak_points),1),
                 round(mean(dff2$points_in_the_paint),1),
                 round(mean(dff2$fgp),1),
                 round(mean(dff2$tpp),1),
                 round(mean(dff2$ft_attempted),1),
                 round(mean(dff2$turnovers),1)
                       )
    #now need to make each of these rows as columns (avgstat, gamedat, datdiff)
    #still haven't been done, couldn't quite figure yet
    table <- rbind(gamedat, avgstat)
    colnames(table) <- names(gamedat)
    #initializing datdiff (by making a duplicate dataframe of gamedat)
    datdiff <- gamedat
    for (k in 1:length(gamedat)) {
      #loop to calcuate difference
      datdiff[,k] <- gamedat[,k] - avgstat[k]
    }
    table <- rbind(table, datdiff)
    #savepath needs to be resettled for every team, thus needs a newly defined object
    write.csv(table, file = paste0(savePath, teamname, '/', "game", j, '.csv'), 
              row.names = c("Game", "Season", "Diff"))
  }
}
