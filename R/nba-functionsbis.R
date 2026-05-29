##
## read data
##
nba_data <-function(Player, season){
  team <- read.csv(paste0("data/NBA_",season,"_Shots.csv"))
  hoop_center_y <- 5.25
  player <- team %>% filter(PLAYER_NAME==Player) %>% mutate(x = LOC_X, 
                                                            y = LOC_Y-41.75-5.25)
  # Horizontally flip the data
  player$GAME_ID <- as.factor(player$GAME_ID)
  # data.frame
  yy <- data.frame(idGame=as.factor(player$GAME_ID),dateGame=player$GAME_DATE,LOC_X=player$x,LOC_Y=player$y,SHOT_MADE=player$SHOT_MADE)
  attributes(yy)$namePlayer<-Player
  attributes(yy)$PLAYER_ID<-unique(player$PLAYER_ID)
  attributes(yy)$team<- unique(player$TEAM_NAME)
  attributes(yy)$TEAM_ID<-unique(player$TEAM_ID)
  attributes(yy)$season<-season
  return(yy)
}



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

nba_data_depth_missed <- function(yy){
  temp <- lapply(yy, function(x){
    coords=x$coords[!x$SHOT_MADE,]
    if(is.null(nrow(coords))){
      coords <- matrix(coords,ncol=2,nrow=1)
    }
    if(nrow(coords)==0){
      x <- NULL
    }
    list(date=x$date,coords=coords)})
  #temp[sapply(temp,function(x) nrow(x$coords)==0)] <- NULL
  attributes(temp)$namePlayer <- attr(yy,"namePlayer")
  temp
}


##
## data depth
##

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
       xlab = "Depth vs. red", ylab = "Depth vs. blue", pch = 19, 
       xlim = c(0, 1), ylim = c(0, 1), 
       main = " " )
  grid()
  list(XvsX=depthsXvsX,YvsY=depthsYvsY,XvsY=depthsXvsY,YvsX=depthsYvsX)
}

plot_deepest_match <- function(xx,depth,no,col="red"){
  n <- length(depth)
  deepest <- order(depth) ; 
  plot_match2(xx,names(xx)[deepest[n-no+1]],col=col)
}

deepest_match <- function(xx,depth,no){
  n <- length(depth)
  deepest <- order(depth) ; 
  idMatch <- names(xx)[deepest[n-no+1]]
  temp <- xx[idMatch][[1]]
  temp$idmatch <- idMatch 
  temp$player <- attr(xx,"namePlayer")
  temp
}


select_deepest_match <- function(Players, season){
  N <- length(Players)
  out <- vector(mode="list",length=N)
  team <- read.csv(paste0("data/NBA_",season,"_Shots.csv"))
  for(i in 1:N){
    #print(i)
    player <- team %>% filter(PLAYER_NAME==Players[i]) %>% mutate(x = LOC_X, 
                                                              y = LOC_Y-41.75-5.25)
    # Horizontally flip the data
    player$GAME_ID <- as.factor(player$GAME_ID)
    # data.frame
    yy <- data.frame(idGame=as.factor(player$GAME_ID),dateGame=player$GAME_DATE,LOC_X=player$x,LOC_Y=player$y,SHOT_MADE=player$SHOT_MADE)
    attributes(yy)$namePlayer<-Players[i]
    attributes(yy)$PLAYER_ID<-unique(player$PLAYER_ID)
    attributes(yy)$team<- unique(player$TEAM_NAME)
    attributes(yy)$TEAM_ID<-unique(player$TEAM_ID)
    attributes(yy)$season<-season
    
    shots <- nba_data_depth_made(nba_data_depth(yy))
    depths <- depths.Tukey(shots, shots, nDirs = 250, subs = FALSE, exactEst = FALSE, parConst1 = -2, parConst2 = 5)
    out[[i]] <- deepest_match(shots,depths,1)
  }
  out
}


##
## plot data
##


plot_season <- function(data){
  p <- ggplot(data = data.frame(x = 0, y = 0), aes(x, y))
  p <- drawNBAcourt(p, full = FALSE, size = 0.75, col = "black")
  p <- p + 
    geom_point(data = data, aes(x = LOC_X, y = LOC_Y, color = SHOT_MADE, fill = SHOT_MADE), size =1, shape = 21, stroke = .5) +
    draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", attr(data,"PLAYER_ID"), ".png"), 
               x = -29, y = 1, width = 9, height = 9) +
    # ---- plot team logo (remove these 2 lines if you're plotting multiple teams!)
    draw_image(paste0("https://cdn.nba.com/logos/nba/", attr(data,"TEAM_ID"), "/primary/L/logo.svg"), 
               x = 22, y = 1, width = 6, height = 6) +
    # ---- fill the points with color
    scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks = c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
    scale_fill_manual(values = c("green2","gray61"), aesthetics = "fill", breaks = c("TRUE", "FALSE"), labels=c("Made", "Missed"))  +
    scale_x_continuous(limits = c(-27.5, 27.5)) + scale_y_continuous(limits = c(-47.5, 7.5)) +
    theme(legend.position="none",axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  p  
}




# mise en forme data depth


plot_match <- function(data,idMatch){
  temp<- data[idMatch][[1]]
  don <- data.frame(isShotMade=temp$typeEvent,x=temp$coord[1,],y=temp$coord[2,])
  p1 <- plot_court(court_themes$ppt, use_short_three = F) + 
    geom_point(data = don, aes(x = x, y = y, color = isShotMade, fill = SHOT_MADE), size =2, shape = 21, stroke = .5) + 
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
          axis.ticks.y=element_blank()) + 
    labs(title = temp$date)
  p
}





