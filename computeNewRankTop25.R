# top25 function for one rating period.
# takes a data frame containing all of the new game results and returns a data frame containing
# the new rankings (new average points per game for every team).
# assume the the all games data frame contains names of teams as characters NOT factors.

computeNewRank <- function(newGamesDF) {
  
  teams <- unique(c(newGamesDF$WinningTeam, newGamesDF$LosingTeam))
  pointTotals <- data.frame(TeamPoints = rep(0, length(teams)), GamesPlayed = rep(0, length(teams)) )
  rownames(pointTotals) <- teams
  pointTotals <- fillPointTotals(newGamesDF, pointTotals)
  
  resultList <- computeNewAvePoints(newGamesDF, pointTotals)
  newGamesDF <- resultList$a
  pointTotals <- resultList$b
  
}

fillPointTotals<- function(newGamesDF, pointTotals) {
  for (index in 1:length(rownames(newGamesDF))) {
    team <- newGamesDF[index, 'WinningTeam']
    gamesPlayed <- newGamesDF[index, 'WinningGamesPlayed']
    avePPG <- newGamesDF[index, 'WinningPointsInitial']
    pointTotals[team, 'GamesPlayed'] <- gamesPlayed
    pointTotals[team, 'TeamPoints'] <- avePPG
    team <- newGamesDF[index,'LosingTeam']
    gamesPlayed <- newGamesDF[index, 'LosingGamesPlayed']
    avePPG <- newGamesDF[index, 'LosingPointsInitial']
    pointTotals[team, 'GamesPlayed'] <- gamesPlayed
    pointTotals[team, 'TeamPoints'] <- avePPG
  }
  return(pointTotals)
} 


computeNewAvePoints <- function(newGamesDF, pointTotals) {
  
  originalPointTotals <- pointTotals
  
  # start with a data frame containing info for all games: winner, loser, winner score, 
  # loser score, current average points earned per game for winning team, current  average
  # points earned for losing team. 
  
  # compute the points earned by the winner and loser for each game.
  WinningPointsEarned <- newGamesDF$LosingPointsInitial + 400/(pmax(2/3,
                            2.5*(newGamesDF$LosingScore/newGamesDF$WinningScore)^2))
  LosingPointsEarned <- newGamesDF$WinningPointsInitial - 400/(pmax(2/3,
                            2.5*(newGamesDF$LosingScore/newGamesDF$WinningScore)^2))
  
  # put those new points into the newGames data frame as WinningPointsEarned and 
  # LosingPointsEarned
  newGamesDF <- data.frame(newGamesDF, WinningPointsEarned, LosingPointsEarned)
  
  # update total current points for each team from wins
  pointTotals <- addAllWinningGames(newGamesDF, pointTotals)
  
  # update total current points for each team from losses
  pointTotals <- addAllLosingGames(newGamesDF, pointTotals)
  
  # divide point totals by games played to get a current average points per game for
  # each team, and add it to the pointTotals data frame
  pointTotals <- data.frame(pointTotals, pointTotals$TeamPoints/pointTotals$GamesPlayed)
  
  # update winning points initial
  newGamesDF <- updateWinningPointsInitial(newGamesDF, pointTotals)
  
  # update losing points initial
  newGamesDF <- updateLosingPointsInitial(newGamesDF, pointTotals)
  
  # delete earned points columns
  newGamesDF$WinningPointsEarned <- NULL
  newGamesDF$LosingPointsEarned <- NULL
  
  # restore pointTotals to original
  pointTotals <- data.frame(originalPointTotals, pointTotals[length(pointTotals)])
  
  # we now have a data frame with columns for WinningTeam, LosingTeam, WinningScore, Losingscore,
  # WinningPointsInitial, LosingPointsInitial.
  # Winning and Losing points initial are now the current average points per game for the winning
  # and losing teams respectively.
  
  # pointTotals will hopefully always be whatever it started at for this round of ranking calculations,
  # plus a column with average points per game for every run of the ranking calculator 
  return(list(a=newGamesDF, b=pointTotals))
}

addAllWinningGames <- function(df, pointTotals) {
  for (index in 1:length(rownames(df))) {
    team <- df[index, 'WinningTeam']
    points <- df[index, "WinningPointsEarned"]
    pointTotals[team, "TeamPoints"] <- pointTotals[team, "TeamPoints"] + points
    pointTotals[team, "GamesPlayed"] <- pointTotals[team, "GamesPlayed"] + 1
  }
  return(pointTotals)
}

addAllLosingGames <- function(df, pointTotals) {
  for (index in 1:length(rownames(df))) {
    team <- df[index, 'LosingTeam']
    points <- df[index, "LosingPointsEarned"]
    pointTotals[team, "TeamPoints"] <- pointTotals[team, "TeamPoints"] + points
    pointTotals[team, "GamesPlayed"] <- pointTotals[team, "GamesPlayed"] + 1
  }
  return(pointTotals)
}

updateWinningPointsInitial <- function(df, pointTotals) {
  for (index in 1:length(rownames(df))) {
    team <- df[index, 'WinningTeam']
    winnerPoints <- pointTotals[team, length(pointTotals)]
    df[index, "WinningPointsInitial"] <- winnerPoints
  }
  return(df)
}

updateLosingPointsInitial <- function(df, pointTotals) {
  for (index in 1:length(rownames(df))) {
    team <- df[index, 'LosingTeam']
    winnerPoints <- pointTotals[team, length(pointTotals)]
    df[index, "LosingPointsInitial"] <- winnerPoints
  }
  return(df)
}
