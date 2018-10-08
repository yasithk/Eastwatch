library(rjson)
# shot data for Stephen Curry
playerID <- 201939
shotURL <- 'https://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2017-18&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=1627732&PlayerPosition=&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0'
# import from JSON
shotData <- fromJSON(file = shotURL, method="C")

# unlist shot data, save into a data frame
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

# shot data headers
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]

# covert x and y coordinates into numeric
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))
# shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))
shotDataf$GAME_DATE <- as.numeric(as.character(shotDataf$GAME_DATE))

# have a look at the data
View(shotDataf)

#filter for Date range

shotDataf <- shotDataf %>% filter(GAME_DATE > 20180305)

# simple plot using EVENT_TYPE to colour the dots
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))

library(grid)
library(jpeg)
library(RCurl)

# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(-250, 250) +
  ylim(-50, 420)



# plot using ggplot and NBA court background image
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = SHOT_ZONE_BASIC, shape = EVENT_TYPE)) +
  xlim(250, -250) +
  ylim(-50, 420) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))


library(grid)
library(gridExtra)
library(png)
library(RCurl)

# scrape player photo and save as a raster object
playerImg.URL <- paste("http://stats.nba.com/media/players/132x132/",playerID,".png", sep="")
playerImg <- rasterGrob(readPNG(getURLContent(playerImg.URL)), 
                        width=unit(0.15, "npc"), height=unit(0.15, "npc"))

# plot using ggplot and NBA court background
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = EVENT_TYPE, alpha = 0.8), size = 3) +
  scale_color_manual(values = c("#008000", "#FF6347")) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  geom_rug(alpha = 0.2) +
  coord_fixed() +
  # ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), sep = "")) +
  ggtitle('Ben Simmons March& April Hot Streak')
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))

# add player photo and footnote to the plot
pushViewport(viewport(x = unit(0.9, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "thedatagame.com.au", just = "centre", vjust = 50)
