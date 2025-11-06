# how many NFL games are one-possession games in the fourth quarter? 
# game summary url example: https://www.espn.com/nfl/game/_/gameId/401772940/
onePossessionPts = 8
buildScoringPlays = function(scoring.drives){
  # need period number, awayScore, homeScore, clock
  # scoring.plays = data.frame("awayScore" = 0, "homeScore" = 0, "period" = 1, 
  #                            "clockDisplayValue" = "15:00")
  # for(i in 1:nrow(scoring.drives)){
  #   score.result = scoring.drives[i, "result"]
  #   score.team.index = match(scoring.drives$team$displayName[i], game.teams)
  #   new.row = scoring.plays[i, ]
  #   new.row$period = scoring.drives$end$period$number[i]
  #   new.row$clockDisplayValue = scoring.drives$end$clock$displayValue[i]
  #   if(score.result == "FG"){
  #     score.pts = 3
  #   } else if(score.result == "TD"){
  #     drive.plays = scoring.drives$plays[[i]]
  #   } else if(score.result == "SFTY"){
  #     
  #   } else {
  #     stop(paste("Error in decoding score result.\n", 
  #                "Game ID:", game.id, "\n", 
  #                "Score result:", score.result), "\n", e)
  #   }
  #   new.row[, score.team.index] = new.row[, score.team.index] + score.pts
  #   scoring.plays = scoring.plays %>% rbind(new.row)
  # }
  scoring.plays = data.frame()
  for(i in 1:nrow(scoring.drives)){
    tmp = scoring.drives$plays[[i]] %>% 
      filter(scoringPlay) %>% 
      `rownames<-`(NULL)
    scoring.plays = scoring.plays %>% 
      rbind(tmp)
  }
  return(scoring.plays)
}
analyzeEspnGameRecap = function(game.id){
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  espn.url = paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event=", game.id)
  espn.json = fromJSON(content(GET(espn.url), "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
  df = data.frame()
  
  game.teams = espn.json$boxscore$teams %>%
    arrange(homeAway) %>%
    mutate(team_name = team$displayName) %>%
    select(team_name) %>% 
    unlist()
  if("NFC" %in% game.teams){
    # pro bowl
    return(df)
  }
  df[1 , c("Away", "Home")] = game.teams
  
  date =tryCatch({
    espn.json$drives[[1]]$plays[[1]]$modified[1] %>% 
      as.POSIXct(tz = "UTC") %>% 
      format(tz = "America/New_York", usetz = TRUE) %>% 
      as.Date()
  }, error = function(e){
    print(paste("Error when getting date for game", game.id, "\n", e))
    return("")
  })
  if(length(date) == 0){
    print(paste("Date of length 0 for game", game.id, "| Possibly postponed?"))
    return(data.frame())
  }
  df$Date = date
  
  
  scoring.plays = espn.json$scoringPlays
  if(is.null(scoring.plays)){
    scoring.plays = espn.json$drives$previous %>% 
      filter(isScore) %>% 
      buildScoringPlays()
    # function call 
  }
  if(nrow(scoring.plays) == 0){
    stop("There is a 0-0 tie!", 
         paste("game ID", game.id))
  } else {
    final.score = scoring.plays %>%
      tail(1)
  }
  df$Away.Pts = final.score$awayScore
  df$Home.Pts = final.score$homeScore
  
  df$OnePoss4qtr = isOnePossessionInFourthQuarter(scoring.plays)
  return(df)
}
isOnePossessionInFourthQuarter = function(scoring.plays){
  require(dplyr)
  
  fourth.quarter.plays = scoring.plays %>% 
    filter(period$number >= 4)
  last.score.before.fourth.quarter = scoring.plays %>% 
    filter(period$number < 4) %>% 
    tail(1)
  tmp = bind_rows(last.score.before.fourth.quarter, fourth.quarter.plays)
  if(nrow(tmp) == 0){
    return(TRUE) # 0-0 tie
  }
  for(i in 1:nrow(tmp)){
    if(scoreDiff(tmp[i, ]) < onePossessionPts){
      return(TRUE)
    }
  }
  return(FALSE)
}
scoreDiff = function(play.row){
  abs(play.row$awayScore - play.row$homeScore)
}
getSeasonGameData = function(season){
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(stringr)
  
  df = data.frame()
  for(season.type in 2:3){
    dfx = data.frame()
    espn.url = paste0('https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/seasons/', 
                      season, '/types/', season.type, '/events?limit=1000')
    espn.json = fromJSON(content(GET(espn.url), "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
    game.ids = espn.json$items$`$ref` %>% 
      str_extract("\\d{9}")
    # pb = txtProgressBar(max = length(game.ids), style = 3)
    for(id in game.ids){
      x = tryCatch({
        analyzeEspnGameRecap(id)
      }, error = function(e){
        stop(paste("Error when analyzing game", id, "\n"), e)
      })
      
      dfx = dfx %>% 
        rbind(x)
      # setTxtProgressBar(pb, match(id, game.ids))
    }
    dfx$Playoffs = season.type == 3
    dfx$Game.id = game.ids
    df = df %>% rbind(dfx)
  }
  return(df)
}
getNflGameData = function(seasons){
  require(dplyr)
  print(paste("Getting data for seasons", seasons[1], "-", tail(seasons, 1)))
  df = data.frame()
  pb = txtProgressBar(max = length(seasons), style = 3)
  for(s in seasons){
    df = df %>% 
      rbind(getSeasonGameData(s))
    setTxtProgressBar(pb, match(s, seasons))
  }
  close(pb)
  assign("nfl.game.data", df)
  View(nfl.game.data)
  beepr::beep()
}
getNflGameData(2010:2024)
beepr::beep()


# investigate game 400554366






