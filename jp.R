#function to check if a package is installed
is_installed <- function(mykg) is.element(mypkg, installed.packages()[,1])

library(dplyr)
library(magrittr)
library(ggplot2)
if(!is_installed("reshape")){install.packages("reshape")}
library(reshape)

raw <- read.csv("pbp-2018.csv")
#  dplyr::select(GameId, Yards, OffenseTeam, IsPass) %>%
#  dplyr::filter(IsPass==1)


#filter raw data by date
raw$GameDate <- as.Date(raw$GameDate, format= "%Y-%m-%d")
raw.datefiltered <- subset(raw, GameDate> "2018-09-01" & GameDate < "2018-10-29")
#raw.datefiltered <- raw
#dplyr::glimpse(raw.datefiltered)

#passing yards
cols <- c("GameId", "OffenseTeam","Yards","IsPass")
py <- raw.datefiltered[,cols]
py <- py[-which(py$OffenseTeam == ""),]
py <- py[which(py$IsPass == 1),]
#dplyr::glimpse(summary(py))

#Number of games played
cols <- c("GameId", "OffenseTeam")
gp <- raw.datefiltered[, cols]
gp <- gp[-which(gp$OffenseTeam == ""),]
gp <- aggregate(GameId ~ OffenseTeam, gp, function(x) length(unique(x)))

py2 <- aggregate(py["Yards"], list(py$OffenseTeam), sum)

#rushing yards
cols <- c("GameId", "OffenseTeam","Yards","IsRush")
ry <- raw.datefiltered[,cols]
ry <- ry[-which(ry$OffenseTeam == ""),]
ry <- ry[which(ry$IsRush == 1),]

ry2 <- aggregate(ry["Yards"], list(ry$OffenseTeam), sum)

#passing yards summed by game
py.bygame <- cast(py, OffenseTeam+GameId~., sum, value = "Yards")
colnames(py.bygame)[colnames(py.bygame)=="(all)"] <- "PassingYards"

#rushing yards summed by game
ry.bygame <- cast(ry, OffenseTeam+GameId~., sum, value = "Yards")
colnames(ry.bygame)[colnames(ry.bygame)=="(all)"] <- "RushingYards"

#checkpoint

#average passing yards per game
avg.pypg <- cast(py.bygame, OffenseTeam~., mean, value = "PassingYards")
colnames(avg.pypg)[colnames(avg.pypg)=="(all)"] <- "AvgPassingYardsPerGame"

#average rushing yards per game
avg.rypg <- cast(ry.bygame, OffenseTeam~., mean, value = "RushingYards")
colnames(avg.rypg)[colnames(avg.rypg)=="(all)"] <- "AvgRushingYardsPerGame"

#passing yards allowed
cols <- c("GameId", "DefenseTeam","Yards","IsPass")
pya <- raw.datefiltered[,cols]
pya <- pya[which(pya$IsPass == 1),]

#rushing yards allowed
cols <- c("GameId", "DefenseTeam","Yards","IsRush")
rya <- raw.datefiltered[,cols]
rya <- rya[which(rya$IsRush == 1),]

#passing yards allowed summed by game
pya.bygame <- cast(pya, DefenseTeam+GameId~., sum, value = "Yards")
colnames(pya.bygame)[colnames(pya.bygame)=="(all)"] <- "PassingYardsAllowed"

#rushing yards allowed summed by game
rya.bygame <- cast(rya, DefenseTeam+GameId~., sum, value = "Yards")
colnames(rya.bygame)[colnames(rya.bygame)=="(all)"] <- "RushingYardsAllowed"

#checkpoint

#average passing yards allowed per game
avg.pyapg <- cast(pya.bygame, DefenseTeam~., mean, value = "PassingYardsAllowed")
colnames(avg.pyapg)[colnames(avg.pyapg)=="(all)"] <- "AvgPassingYardsAllowedPerGame"

#average rushing yards allowed per game
avg.ryapg <- cast(rya.bygame, DefenseTeam~., mean, value = "RushingYardsAllowed")
colnames(avg.ryapg)[colnames(avg.ryapg)=="(all)"] <- "AvgRushingYardsAllowedPerGame"

#checkpoint

#create a training set of games, passing and rushing yards earn
raw.train <- subset(raw, GameDate> "2018-09-01" & GameDate < "2018-09-14")

#match ups and passing yards
cols <- c("GameId", "OffenseTeam", "DefenseTeam", "Yards","IsPass")
mu.py <- raw.train[,cols]
mu.py <- mu.py[-which(mu.py$OffenseTeam == ""),]
mu.py <- mu.py[which(mu.py$IsPass == 1),]

#match ups and rushing yards
cols <- c("GameId", "OffenseTeam", "DefenseTeam", "Yards","IsRush")
mu.ry <- raw.train[,cols]
mu.ry <- mu.ry[-which(mu.ry$OffenseTeam == ""),]
mu.ry <- mu.ry[which(mu.ry$IsRush == 1),]

#the matchup passing yards summed by game
mu.py.bygame <- cast(mu.py, GameId+OffenseTeam+DefenseTeam~., sum, value = "Yards")
colnames(mu.py.bygame)[colnames(mu.py.bygame)=="(all)"] <- "PassingYards"

#the matchup rushing yards summed by game
mu.ry.bygame <- cast(mu.ry, GameId+OffenseTeam+DefenseTeam~., sum, value = "Yards")
colnames(mu.ry.bygame)[colnames(mu.ry.bygame)=="(all)"] <- "RushingYards"

#create a matchup with rushing and passing yards
mu.train <- merge(mu.py.bygame, mu.ry.bygame, by.x=c("GameId", "OffenseTeam","DefenseTeam"), by.y=c("GameId", "OffenseTeam","DefenseTeam"))

#add average passing yards per game to the matchup
mu.train <- merge(mu.train, avg.pypg, by="OffenseTeam")
mu.train <- merge(mu.train, avg.pyapg, by="DefenseTeam")

#add average rushing yards per game to the matchup
mu.train <- merge(mu.train, avg.rypg, by="OffenseTeam")
mu.train <- merge(mu.train, avg.ryapg, by="DefenseTeam")

#get rid of the game id
mu.train$GameId <- NULL
ggplot(mu.train, aes(x=AvgPassingYardsPerGame)) + geom_bar()

write.csv(mu.train, file = "nfltrain.csv")
