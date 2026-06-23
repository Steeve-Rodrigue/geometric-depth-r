nba_player_db <- function(
    Player,
    seasons,
    season_types = "Regular Season",
    export_csv   = FALSE,
    filepath     = NULL
){
  
  db <- lapply(seasons, function(season){
    
    cat("Downloading", Player, "-", season, "\n")
    
    dat <- nba_data(
      Player       = Player,
      season       = season,
      season_types = season_types
    )
    
    result <- data.frame(
      PLAYER_ID   = attr(dat, "PLAYER_ID"),
      PLAYER_NAME = attr(dat, "namePlayer"),
      
      TEAM_ID     = attr(dat, "TEAM_ID"),
      TEAM_NAME   = attr(dat, "team"),
      
      SEASON      = season,
      
      GAME_ID     = dat$idGame,
      GAME_DATE   = dat$dateGame,
      
      LOC_X       = dat$LOC_X,
      LOC_Y       = dat$LOC_Y + 41.75 + 5.25,
      
      SHOT_MADE   = dat$SHOT_MADE,
      
      stringsAsFactors = FALSE
    )
    
    Sys.sleep(2)  # pause between seasons to avoid rate limiting
    result
  })
  
  db <- dplyr::bind_rows(db)
  
  if (export_csv) {
    if (is.null(filepath)) {
      player_clean <- gsub(" ", "_", Player)
      filepath <- paste0(player_clean, "_", min(seasons), "_", max(seasons), ".csv")
    }
    write.csv(db, filepath, row.names = FALSE)
    cat("Exported to", filepath, "\n")
  }
  
  db
}



nba_data_csv <- function(filepath, seasons = NULL) {
  
  team <- read.csv(filepath)
  
  if (!is.null(seasons)) {
    team <- team[team$SEASON %in% seasons, ]
  }
  
  yy <- data.frame(
    idGame    = as.factor(team$GAME_ID),
    dateGame  = team$GAME_DATE,
    LOC_X     = team$LOC_X,
    LOC_Y     = team$LOC_Y - 41.75 - 5.25,
    SHOT_MADE = team$SHOT_MADE,
    stringsAsFactors = FALSE
  )
  
  attributes(yy)$namePlayer <- unique(team$PLAYER_NAME)
  attributes(yy)$PLAYER_ID  <- unique(team$PLAYER_ID)
  attributes(yy)$team        <- unique(team$TEAM_NAME)
  attributes(yy)$TEAM_ID     <- unique(team$TEAM_ID)
  attributes(yy)$season      <- unique(team$SEASON)
  
  return(yy)
}