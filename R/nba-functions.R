#' Get NBA player ID from the stats API
#'
#' @param player_name Character. Full or partial player name (case-insensitive).
#' @return Integer PERSON_ID of the first matching player.
#' @examples
#' \dontrun{
#' get_player_id("LeBron James")  # 2544
#' }
get_player_id <- function(player_name) {
  resp <- httr::GET(
    "https://stats.nba.com/stats/commonallplayers",
    query = list(LeagueID = "00", Season = "2024-25", IsOnlyCurrentSeason = 0),
    httr::add_headers(
      `User-Agent`         = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      `Referer`            = "https://www.nba.com/",
      `Origin`             = "https://www.nba.com",
      `x-nba-stats-origin` = "stats",
      `x-nba-stats-token`  = "true",
      `Accept`             = "application/json, text/plain, */*",
      `Accept-Language`    = "en-US,en;q=0.9",
      `Accept-Encoding`    = "gzip, deflate, br",
      `Connection`         = "keep-alive"
    ),
    httr::timeout(30)
  )
  if (httr::http_error(resp)) stop(paste("HTTP error:", httr::status_code(resp)))
  parsed  <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  hdrs    <- parsed$resultSets$headers[[1]]
  rows    <- parsed$resultSets$rowSet[[1]]
  df      <- as.data.frame(rows, stringsAsFactors = FALSE)
  colnames(df) <- hdrs
  match_row <- df[grepl(player_name, df$DISPLAY_FIRST_LAST, ignore.case = TRUE), ]
  if (nrow(match_row) == 0) stop(paste("Player not found:", player_name))
  as.integer(match_row$PERSON_ID[1])
}
#===========================================NBA_data==========================================================================

#' Fetch shot chart data for an NBA player
#'
#' @param Player Character. Player name passed to \code{get_player_id}.
#' @param season Integer. End year of the season (e.g. \code{2024} for 2023-24).
#' @param season_types Character. One of \code{"Regular Season"}, \code{"Playoffs"}, etc.
#' @return A data frame with columns \code{player}, \code{idGame}, \code{dateGame},
#'   \code{x}, \code{y}, \code{SHOT_MADE}. Player metadata stored as attributes.
#' @examples
#' \dontrun{
#' shots <- nba_data("LeBron James", 2024)
#' }
nba_data <- function(Player, season, season_types = "Regular Season") {
  # season = end year, e.g. 2019 -> "2018-19"
  season_str <- paste0(season - 1, "-", substr(as.character(season), 3, 4))
  player_id  <- get_player_id(Player)

  resp <- httr::GET(
    "https://stats.nba.com/stats/shotchartdetail",
    query = list(
      PlayerID       = player_id,
      Season         = season_str,
      SeasonType     = season_types,
      TeamID         = 0, GameID        = "",
      ContextMeasure = "FGA",
      DateFrom       = "", DateTo        = "",
      GameSegment    = "", LastNGames    = 0,
      LeagueID       = "00", Location   = "",
      Month          = 0, OpponentTeamID = 0,
      Outcome        = "", Period        = 0,
      Position       = "", RookieYear    = "",
      SeasonSegment  = "", VsConference  = "",
      VsDivision     = ""
    ),
    httr::add_headers(
      `User-Agent`         = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      `Referer`            = "https://www.nba.com/",
      `Origin`             = "https://www.nba.com",
      `x-nba-stats-origin` = "stats",
      `x-nba-stats-token`  = "true",
      `Accept`             = "application/json, text/plain, */*",
      `Accept-Language`    = "en-US,en;q=0.9",
      `Accept-Encoding`    = "gzip, deflate, br",
      `Connection`         = "keep-alive"
    ),
    httr::timeout(30)
  )
  if (httr::http_error(resp)) stop(paste("HTTP error:", httr::status_code(resp)))
  parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
  hdrs   <- parsed$resultSets$headers[[1]]
  rows   <- parsed$resultSets$rowSet[[1]]
  raw    <- as.data.frame(rows, stringsAsFactors = FALSE)
  colnames(raw) <- hdrs

  # API returns coordinates in tenths of a foot
  player <- raw %>%
    mutate(
      x          = as.numeric(LOC_X) / 10 * -1,
      y          = as.numeric(LOC_Y) / 10 - 41.75,
      SHOT_MADE = SHOT_MADE_FLAG == "1",
      idGame     = as.factor(GAME_ID)
    )

  yy <- data.frame(player=raw$PLAYER_NAME ,idGame=player$idGame, dateGame=player$GAME_DATE, x=player$x, y=player$y, SHOT_MADE=player$SHOT_MADE)
  attributes(yy)$namePlayer <- Player
  attributes(yy)$PLAYER_ID  <- player_id
  attributes(yy)$team        <- unique(raw$TEAM_NAME)[1]
  attributes(yy)$TEAM_ID     <- unique(raw$TEAM_ID)[1]
  attributes(yy)$season      <- season
  return(yy)
}
#=================================================NBA_data_depth====================================================================

#' Reshape shot data into a per-game list for depth analysis
#'
#' Splits the flat shot data frame from \code{nba_data()} by game and converts
#' each game into a list containing the shot coordinates as a matrix, the made/missed
#' flags, and the game date. The result is the input format expected by depth functions.
#'
#' @param yy_df Data frame returned by \code{nba_data()}.
#' @return A named list (one element per game) where each element contains:
#'   \code{date}, \code{SHOT_MADE} (logical vector), \code{coords} (n x 2 matrix of x/y).
#'   Player metadata attributes are preserved.
#' @examples
#' \dontrun{
#' shots <- nba_data("LeBron James", 2024)
#' lb <- nba_data_depth(shots)
#' }
nba_data_depth <- function(yy_df){
  yy <- lapply(split(yy_df,yy_df$idGame), function(x){
    list(date = unique(x$dateGame),SHOT_MADE=x$SHOT_MADE,coords=cbind(x$x,x$y))
  })
  attributes(yy)$namePlayer<-attr(yy_df,"namePlayer")
  attributes(yy)$PLAYER_ID<-attr(yy_df,"PLAYER_ID")
  attributes(yy)$team<- attr(yy_df,"team")
  attributes(yy)$TEAM_ID<-attr(yy_df,"TEAM_ID")
  attributes(yy)$season<-attr(yy_df,"season")
  yy
}
#=================================================NBA_data_depth_made====================================================================

#' Filter per-game shot data to made shots only
#'
#' From the list returned by \code{nba_data_depth()}, keeps only the coordinates
#' of made shots for each game. Games with no made shots are dropped.
#'
#' @param yy Named list returned by \code{nba_data_depth()}.
#' @return A filtered list with the same structure as \code{yy}, where each
#'   element contains only \code{coords} of made shots. Player metadata attributes
#'   are preserved.
#' @examples
#' \dontrun{
#' lb <- nba_data_depth(shots)
#' lb_made <- nba_data_depth_made(lb)
#' }
nba_data_depth_made <- function(yy){
  temp <- lapply(yy, function(x){
    coords=x$coords[x$SHOT_MADE,]
    if(is.null(nrow(coords))){
      coords <- matrix(coords,ncol=2,nrow=1)
    }
    if(nrow(coords)==0){
      x <- NULL
    }
    list(date=x$date,coords=coords)})
  #temp[sapply(temp,function(x) is.null(x$coords))] <- NULL
  attributes(temp)$namePlayer <- attr(yy,"namePlayer")
  temp
}

#=================================================NBA_data_depth_missed====================================================================
#' Filter per-game shot data to missed shots only
#'
#' From the list returned by \code{nba_data_depth()}, keeps only the coordinates
#' of missed shots for each game. Games with no missed shots are dropped.
#'
#' @param yy Named list returned by \code{nba_data_depth()}.
#' @return A filtered list with the same structure as \code{yy}, where each
#'   element contains only \code{coords} of missed shots. Player metadata attributes
#'   are preserved.
#' @examples
#' \dontrun{
#' lb <- nba_data_depth(shots)
#' lb_missed <- nba_data_depth_missed(lb)
#' }

nba_data_depth_missed <- function(yy){
  temp <- lapply(yy, function(x){
    coords=x$coords[!x$SHOT_MADE,]
    if(is.null(nrow(coords))){
      coords <- matrix(coords,ncol=2,nrow=1)
    }
    if(nrow(coords)==0){
      x <- NULL
    }
    list(date = x$date,coords=coords)})
  #temp[sapply(temp,function(x) nrow(x$coords)==0)] <- NULL
  attributes(temp)$namePlayer <- attr(yy,"namePlayer")
  attributes(temp)$PLAYER_ID  <- attr(yy,"PLAYER_ID")
  attributes(temp)$team        <- attr(yy,"team")
  attributes(temp)$TEAM_ID     <- attr(yy,"TEAM_ID")
  attributes(temp)$season      <- attr(yy,"season")
  temp
}

#=================================================NBA_depth_plot====================================================================

#' Depth-vs-Depth plot comparing two groups of shots
#'
#' Computes the Tukey (halfspace) depth of every shot relative to both groups
#' (e.g. missed \code{xx} vs made \code{yy}) and plots one against the other.
#' Points near the diagonal indicate the two spatial distributions are similar;
#' points off it indicate they differ.
#'
#' @param xx Per-game shot lists (from \code{nba_data_depth_missed})
#' @param yy Per-game shot lists (from \code{nba_data_depth_made}) 
#' @param Ndirs Integer. Number of random directions for the depth approximation.
#' @param col Length-2 colour vector for the \code{xx} and \code{yy} points.
#' @param parConst1,parConst2 Tuning constants passed to \code{depths.Tukey}.
#' @param package Logical. \code{TRUE} uses \code{curveDepth::depths.Tukey};
#'   \code{FALSE} uses the internal \code{ppdepth}.
#' @return A list of the four depth vectors: \code{XvsX}, \code{YvsY},
#'   \code{XvsY}, \code{YvsX}. Also draws the DD-plot as a side effect.
#' @examples
#' \dontrun{
#' out <- ddplot_nba(lb_missed, lb_made, Ndirs = 250, parConst1 = -2, parConst2 = 5)
#' }
ddplot_nba <- function(xx,yy,Ndirs,col=c("red","green4"),
                       parConst1 = -1, parConst2 = 5, package=TRUE){
  if(package){
    depthsXvsX <- depths.Tukey(xx, xx, nDirs = curNdirs, subs = FALSE, exactEst = FALSE, parConst1 = parConst1, parConst2 = parConst2)
    depthsXvsY <- depths.Tukey(xx, yy, nDirs = curNdirs, subs = FALSE, exactEst = FALSE, parConst1 = parConst1, parConst2 = parConst2)
    depthsYvsX <- depths.Tukey(yy, xx, nDirs = curNdirs, subs = FALSE, exactEst = FALSE, parConst1 = parConst1, parConst2 = parConst2)
    depthsYvsY <- depths.Tukey(yy, yy, nDirs = curNdirs, subs = FALSE, exactEst = FALSE, parConst1 = parConst1, parConst2 = parConst2)  
  }else{
    xx <- lapply(xx, function(x){list(coords=t(x$coords))})
    yy <- lapply(yy, function(x){list(coords=t(x$coords))})
    
    depthsXvsX <- ppdepth(xx,xx,Ndirs)
    depthsXvsY <- ppdepth(xx,yy,Ndirs)
    depthsYvsX <- ppdepth(yy,xx,Ndirs)
    depthsYvsY <- ppdepth(yy,yy,Ndirs)
  }
  depthsVsX <- c(depthsXvsX, depthsYvsX)
  depthsVsY <- c(depthsXvsY, depthsYvsY)
  plot(cbind(depthsVsX, depthsVsY), col = c(rep(col[1], length(depthsXvsX)), 
                                            rep(col[2], length(depthsYvsY))),
       xlab = "Depth in missed", ylab = "Depth in made", pch = 19, 
       xlim = c(0, 1), ylim = c(0, 1), 
       main = " " )
  grid()
  list(XvsX=depthsXvsX,YvsY=depthsYvsY,XvsY=depthsXvsY,YvsX=depthsYvsX)
}



















































plot_match <- function(data,idMatch){
  temp<- data[idMatch][[1]]
  don <- data.frame(SHOT_MADE=temp$typeEvent,x=temp$coord[1,],y=temp$coord[2,])
  p1 <- plot_court(court_themes$ppt, use_short_three = F) + 
    geom_point(data = don, aes(x = x, y = y, color = SHOT_MADE, fill = SHOT_MADE), size =2, shape = 21, stroke = .5) + 
    scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) + 
    scale_fill_manual(values = c("green2","gray20"), aesthetics = "fill", breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) + 
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(0, 45)) + theme(legend.position="none") #+ 
  #theme(legend.text = element_text(size=5), #legend.title = element_text(size=7),
  #      legend.position.inside = c(.5, .85), legend.direction = "horizontal", 
  #      legend.title = element_blank(),
  #      plot.background = element_rect(fill="gray20", color = NA)) 
  #ggdraw(p1) + theme(plot.background = element_rect(fill="gray20", color = NA))
  p1
}

plot_match2 <- function(data,idMatch,col='lightblue'){
  temp<- data[idMatch][[1]]
  don <- data.frame(x=temp$coord[,1],y=temp$coord[,2])
  
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p <- p + 
    geom_point(data = don, aes(x = x, y = y), 
               size =2, shape = 19, stroke = .5, col=col) +
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  p
}

plot_season <- function(data,x,y,pt.col=pt.col){
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p <- p + 
    geom_point(data = data, aes(x = x, y = y, color = SHOT_MADE, fill = SHOT_MADE), 
               size =1, shape = 21, stroke = .5) +
    draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", attr(data,"PLAYER_ID"), ".png"), 
               x = -29, y = 1, width = 9, height = 9) +
    # ---- plot team logo (remove these 2 lines if you're plotting multiple teams!)
    draw_image(paste0("https://cdn.nba.com/logos/nba/", attr(data,"TEAM_ID"), "/primary/L/logo.svg"), 
           x = 22, y = 1, width = 6, height = 6) +
    # ---- fill the points with color
    scale_color_manual(values = c("green4","red3"), aesthetics = "color", 
                       breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) + 
    scale_fill_manual(values = c("green2","gray61"), aesthetics = "fill", 
                      breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(legend.position="none",axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  p  
}

ppOnedepth <- function(obj,data,Dir){
  m <- ncol(obj$coords)
  out <- numeric(m)
  for(i in 1:m){
    y <- obj$coords[,i] 
    # calcul de la mesure empirique du 1/2 espace pour chaque individu
    Qn <- rowMeans(sapply(data, function(x,y,Dir){
      colMeans((x$coords - y)[1,] %o% Dir[1,] + (x$coords - y)[2,] %o% Dir[2,]>=0)
    },y=y,Dir=Dir))
    mu <- 1/length(Dir)+colMeans((obj$coords - y)[1,-i] %o% Dir[1,] + (obj$coords - y)[2,-i] %o% Dir[2,]>=0)
    out[i] <- min(Qn/mu)
  }
  mean(out)
}

ppdepth <- function(objs,data,nDir){
  # taille des objets
  n <- length(data)  # taille données
  m <- length(objs)   # taille objets
  # direction des 1/2 espaces
  angle <- runif(nDir)*2*pi
  Dir <- rbind(cos(angle),sin(angle))
  # calcul des profondeurs
  out <- numeric(m)
  for(i in 1:m){
    out[i] <- ppOnedepth(objs[[i]],data,Dir)
  }
  out
}


deepest_match <- function(xx,depth,no,col="red"){
  n <- length(depth)
  deepest <- order(depth) ; 
  plot_match2(xx,names(xx)[deepest[n-no+1]],col=col)
}

