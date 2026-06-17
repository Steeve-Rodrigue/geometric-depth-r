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
      LOC_X          = as.numeric(LOC_X) / 10 * -1,
      LOC_Y          = as.numeric(LOC_Y) / 10 - 41.75,
      SHOT_MADE = SHOT_MADE_FLAG == "1",
      idGame     = as.factor(GAME_ID)
    )

  yy <- data.frame(player=raw$PLAYER_NAME ,idGame=player$idGame, dateGame=player$GAME_DATE, LOC_X=player$LOC_X, LOC_Y=player$LOC_Y, SHOT_MADE=player$SHOT_MADE)
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
    list(date = unique(x$dateGame),SHOT_MADE=x$SHOT_MADE,coords=cbind(x$LOC_X,x$LOC_Y))
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
       main = paste0("DD-plot — ", attr(xx, "namePlayer"), " (", attr(xx, "season"), ")")

       )
  grid()
  list(XvsX=depthsXvsX,YvsY=depthsYvsY,XvsY=depthsXvsY,YvsX=depthsYvsX)
}
#==============================================deepest_match====================================================================

#' Return the n-th deepest game of one shot process (made OR missed)
#'
#' Ranks the games of a single shot process by their depth and returns the game
#' at rank \code{no} from the top: \code{no = 1} is the deepest (most typical)
#' game, \code{no = 2} the second, and so on. The depth is relative to whichever
#' process is supplied, so \code{xx} and \code{depth} must come from the SAME
#' process (both made, or both missed).
#'
#' @param xx Per-game shot list for one process: made (\code{nba_data_depth_made})
#'   or missed (\code{nba_data_depth_missed}).
#' @param depth Numeric depth vector for that same process, one value per game,
#'   e.g. \code{ddplot_nba(...)$YvsY} for made or \code{$XvsX} for missed.
#' @param no Integer rank from the deepest game (1 = most central/typical).
#' @return The selected game as a list (\code{date}, \code{coords}), with the
#'   game id (\code{idmatch}) and player name (\code{player}) attached.
#' @examples
#' \dontrun{
#' out <- ddplot_nba(lb_missed, lb_made)
#' deepest_match(lb_made,   out$YvsY, no = 1)  # most typical MADE-shot game
#' deepest_match(lb_missed, out$XvsX, no = 1)  # most typical MISSED-shot game
#' }
deepest_match <- function(xx,depth,no){
  n <- length(depth)
  deepest <- order(depth)
  idMatch <- names(xx)[deepest[n-no+1]]
  temp <- xx[idMatch][[1]]
  temp$idmatch <- idMatch
  temp$player  <- attr(xx,"namePlayer")
  temp
}
#==============================================plot_deepest_match====================================================================
#' Plot the shot chart of the n-th deepest game
#'
#' Convenience wrapper that selects the n-th deepest game with
#' \code{deepest_match()} and draws its shot chart via \code{plot_match2()}.
#' The same \code{xx}/\code{depth} pairing rule applies (both made, or both missed).
#'
#' @inheritParams deepest_match
#' @param col Point colour for the shot chart.
#' @return A ggplot object of the selected game's shot chart.
#' @examples
#' \dontrun{
#' out <- ddplot_nba(lb_missed, lb_made)
#' plot_deepest_match(lb_made, out$YvsY, no = 1, col = "green4")
#' }
plot_deepest_match <- function(xx,depth,no,col="blue"){
  game <- deepest_match(xx, depth, no)
  plot_match2(xx, game$idmatch, col=col)
}

#==============================================plot_match====================================================================

#' Plot a single game's shot chart, coloured by made/missed
#'
#' Draws all shots of one game on a half-court, colouring made shots green and
#' missed shots red. Expects a game from \code{nba_data_depth()} (unfiltered, so
#' the \code{SHOT_MADE} flag is still present).
#'
#' @param data Per-game shot list from \code{nba_data_depth()}.
#' @param idMatch Name (game id) of the game to plot.
#' @return A ggplot object of the game's shot chart.
#' @examples
#' \dontrun{
#' lb <- nba_data_depth(shots)
#' plot_match(lb, names(lb)[1])
#' }
plot_match <- function(data, idMatch){
  temp <- data[[idMatch]]
  don  <- data.frame(SHOT_MADE = temp$SHOT_MADE,
                     x = temp$coords[, 1],
                     y = temp$coords[, 2])
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p +
    geom_point(data = don, aes(x = x, y = y, color = SHOT_MADE, fill = SHOT_MADE),
               size = 2, shape = 21, stroke = .5) +
    scale_color_manual(values = c("green4", "red3"), aesthetics = "color",
                       breaks = c("TRUE", "FALSE"), labels = c("Made", "Missed")) +
    scale_fill_manual(values = c("green2", "red"), aesthetics = "fill",
                      breaks = c("TRUE", "FALSE"), labels = c("Made", "Missed")) +
    scale_x_continuous(limits = c(-27.5, 27.5)) +
    scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(
      legend.position = "none",
      
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background  = element_rect(fill = "white", colour = "white"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )}
#==============================================plot_match2====================================================================
#' Plot a single game's shot chart in one colour
#'
#' Draws all shots of one game on a half-court as same-coloured dots. Unlike
#' \code{plot_match()}, it does not distinguish made/missed — use it on data
#' already filtered to a single process (made or missed).
#'
#' @param data Per-game shot list (e.g. from \code{nba_data_depth_made}/\code{_missed}).
#' @param idMatch Name (game id) of the game to plot.
#' @param col Point colour.
#' @return A ggplot object of the game's shot chart.
#' @examples
#' \dontrun{
#' lb_made <- nba_data_depth_made(nba_data_depth(shots))
#' plot_match2(lb_made, names(lb_made)[1], col = "green4")
#' }
plot_match2 <- function(data,idMatch,col='blue'){
  temp<- data[idMatch][[1]]
  don <- data.frame(x=temp$coord[,1],y=temp$coord[,2])
  
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p <- p + 
    geom_point(data = don, aes(x = x, y = y), 
               size =2.5, shape = 19, stroke = .5, col=col) +
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(
      legend.position = "none",
      
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background  = element_rect(fill = "white", colour = "white"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  p
}
#==============================================plot_season====================================================================
#' Plot a player's full-season shot chart with headshot and team logo
#'
#' Draws every shot of a season on a half-court, coloured by made/missed, and
#' overlays the player's headshot and team logo (fetched from NBA CDN URLs built
#' from the \code{PLAYER_ID}/\code{TEAM_ID} attributes).
#'
#' @param data Shot data frame from \code{nba_data()}, with \code{x}, \code{y},
#'   \code{SHOT_MADE} columns and \code{PLAYER_ID}/\code{TEAM_ID} attributes.
#' @param x,y,pt.col Unused legacy arguments (coordinates and colour are taken
#'   from \code{data} directly).
#' @return A ggplot object of the season shot chart.
#' @examples
#' \dontrun{
#' shots <- nba_data("LeBron James", 2024)
#' plot_season(shots)
#' }
plot_season <- function(data,x,y,pt.col=pt.col){
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p <- p + 
    geom_point(data = data, aes(x   = LOC_X, y = LOC_Y,  color = SHOT_MADE, fill = SHOT_MADE), 
               size =2, shape = 21, stroke = .5) +
    draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", attr(data,"PLAYER_ID"), ".png"), 
               x = -29, y = 1, width = 9, height = 9) +
    # ---- plot team logo (remove these 2 lines if you're plotting multiple teams!)
    draw_image(paste0("https://cdn.nba.com/logos/nba/", attr(data,"TEAM_ID"), "/primary/L/logo.svg"), 
           x = 22, y = 1, width = 6, height = 6) +
    # ---- fill the points with color
    scale_color_manual(values = c("green4","red4"), aesthetics = "color", 
                       breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) + 
    scale_fill_manual(values = c("green2","red2"), aesthetics = "fill", 
                      breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(
      legend.position = "none",
      
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background  = element_rect(fill = "white", colour = "white"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  p  
}

#==============================================plot_season====================================================================
#' Plot a player's full-season shot chart with headshot and team logo
#'
#' Draws every shot of a season on a half-court, coloured by made/missed, and
#' overlays the player's headshot and team logo (fetched from NBA CDN URLs built
#' from the \code{PLAYER_ID}/\code{TEAM_ID} attributes).
#'
#' @param data Shot data frame from \code{nba_data()}, with \code{x}, \code{y},
#'   \code{SHOT_MADE} columns and \code{PLAYER_ID}/\code{TEAM_ID} attributes.
#' @param x,y,pt.col Unused legacy arguments (coordinates and colour are taken
#'   from \code{data} directly).
#' @return A ggplot object of the season shot chart.
#' @examples
#' \dontrun{
#' shots <- nba_data("LeBron James", 2024)
#' plot_season(shots)
#' }
plot_season2 <- function(data,x,y,pt.col=pt.col){
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "gray")
  p <- p + 
    geom_point(data = data, aes(x   = LOC_X, y = LOC_Y,  color = SHOT_MADE, fill = SHOT_MADE), 
               size =1.5, shape = 21, stroke = .5) +
    scale_color_manual(values = c("black","black"), aesthetics = "color", 
                       breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) + 
    scale_fill_manual(values = c("blue","blue"), aesthetics = "fill", 
                      breaks=c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(
      legend.position = "none",
      
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background  = element_rect(fill = "white", colour = "white"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  p  
}
#NON-EXPORTED
#==============================================ppdepth====================================================================
#
#' Halfspace depth of one point pattern (non-package version)
#'
#' Internal building block of the from-scratch (non-\code{curveDepth}) depth used
#' by \code{ddplot_nba(..., package = FALSE)}. Computes the Tukey/halfspace depth
#' of a single object's point pattern relative to a reference set, as the mean
#' over its points of the minimum over directions of \code{Qn/mu}.
#'
#' @param obj One object with a \code{coords} matrix (2 x n: rows x/y).
#' @param data Reference list of objects, each with a \code{coords} matrix.
#' @param Dir 2 x nDir matrix of unit direction vectors (rows cos/sin).
#' @return A single numeric depth value for \code{obj}.
#' @keywords internal
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

#' Halfspace depth of several point patterns (non-package version)
#'
#' From-scratch (non-\code{curveDepth}) point-process depth, used by
#' \code{ddplot_nba(..., package = FALSE)} as the alternative to
#' \code{depths.Tukey}. Draws \code{nDir} random directions on the unit circle
#' and returns the halfspace depth of each object in \code{objs} relative to
#' \code{data}, via \code{ppOnedepth}.
#'
#' @param objs List of objects whose depth is wanted (each with a \code{coords} matrix).
#' @param data Reference list of objects forming the distribution.
#' @param nDir Integer. Number of random directions for the approximation.
#' @return Numeric vector of depths, one per object in \code{objs}.
#' @keywords internal
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


depth_all_matches <- function(matches, Ndirs,
                               parConst1 = -1, parConst2 = 5,
                               package = TRUE) {
  if (!package) {
    matches <- lapply(matches, function(x) list(coords = t(x$coords)))
  }

  depths <- sapply(seq_along(matches), function(i) {
    if (package) {
      depths.Tukey(list(matches[[i]]), matches, nDirs = Ndirs,
                   subs = FALSE, exactEst = FALSE,
                   parConst1 = parConst1, parConst2 = parConst2)
    } else {
      ppdepth(list(matches[[i]]), matches, Ndirs)
    }
  })

  names(depths) <- names(matches)
  depths
}