# top25 function for one rating period.
# takes a data frame containing all of the new game results and the current point
# value for both the winning team and the losing team

computeNewRankTop25 <- function(newGamesDF) {
  
  WinningPointsFinal <- newGamesDF$LosingPointsInitial + 400/(pmax(2/3,
                            2.5*(newGamesDF$LosingScore/newGamesDF$WinningScore)^2))
  LosingPointsFinal <- newGamesDF$WinningPointsInitial - 400/(pmax(2/3,
                            2.5*(newGamesDF$LosingScore/newGamesDF$WinningScore)^2))
  
  newGamesDF <- data.frame(newGamesDF, WinningPointsFinal, LosingPointsFinal)

  
}

example <- function(x) {
  return(x+2)
}