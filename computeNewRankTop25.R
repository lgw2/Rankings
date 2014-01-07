# top25 function for one rating period.
# takes a data frame containing all of the new game results and returns a data frame containing
# the new rankings (new average points per game for every team).
# assume the the all games data frame contains names of teams as characters NOT factors.
# newGamesDF needs columns called "WinningTeam" "LosingTeam" "WinnerScore" "LoserScore" "WinningPointsInitial"
# "LosingPointsInitial" "WinningGamesPlayed" "LosingGamesPlayed".


computeNewRank <- function(newGamesDF) {
  
  teams <- unique(c(newGamesDF$WinningTeam, newGamesDF$LosingTeam))
  pointTotals <- data.frame(TeamPoints = rep(0, length(teams)), GamesPlayed = rep(0, length(teams)) )
  rownames(pointTotals) <- teams
  #pointTotals <- fillPointTotals(newGamesDF, pointTotals)
  
  
  for (x in 500) {
    resultList <- computeNewAvePoints(newGamesDF, pointTotals)
    newGamesDF <- resultList$a
    pointTotals <- resultList$b
  }
  
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
                            2.5*(newGamesDF$LoserScore/newGamesDF$WinnerScore)^2))
  LosingPointsEarned <- newGamesDF$WinningPointsInitial - 400/(pmax(2/3,
                            2.5*(newGamesDF$LoserScore/newGamesDF$WinnerScore)^2))
  
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
    
  # we now have a data frame with columns for WinningTeam, LosingTeam, WinnerScore, LoserScore,
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


# function to get rid of W, F, L from WinnerScore and LoserScore
deleteAllLetterScores <- function(df) {
  toDelete <- vector(mode="numeric",length=0)
  for (index in 1:length(rownames(df))) {
    if (is.na(df[index, "WinnerScore"]) | is.na(df[index, "LoserScore"])) {
      toDelete <- c(toDelete, index)
    } 
  }
  if (length(toDelete) > 0) {
    toDelete <- toDelete*-1
    return(df[toDelete,])
  } else {
    return(df)
  }
}

DFCleanup <- function(df) {
  # give message if missing WinningTeam, LosingTeam, WinnerScore, or LoserScore
  # delete games with letter scores
  # add columns of zeros for missing WinningPointsInitial, LosingPointsInitial, WinningGamesPlayed,
  # LosingGamesPlayed
  
  columns <- c("WinningTeam", "LosingTeam", "WinnerScore", "LoserScore")
  for (index in 1:4) {
    column <- columns[index]
    if (!(column %in% names(df))) {
      print(paste(column, "is missing from the data frame."))
    } else {
      if (column == "WinningTeam" || column == "LosingTeam") {
        df[,column] <- as.character(df[,column])
      } else {
        df[,column] <- as.numeric(as.character(df[,column]))
      }
    } 
  }  
    
  df <- deleteAllLetterScores(df)
  
  columns <- c("WinningPointsInitial", "LosingPointsInitial", "WinningGamesPlayed",
               "LosingGamesPlayed")
  for (index in 1:4) {
    column <- columns[index]
    if (!(column %in% names(df))) {
      if (column == "WinningPointsInitial" || column == "LosingPointsInitial") {
        df <- data.frame(df, rep(1000,length(df[,1])))
      } else {
        df <- data.frame(df, rep(0,length(df[,1])))
      }
      names(df)[length(names(df))] <- column
    }
  }
    
  return(df)
  
}
