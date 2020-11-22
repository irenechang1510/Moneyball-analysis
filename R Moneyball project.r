library(tidyverse)

batting <- read.csv('Batting copy.csv')

head(batting)

str(batting)

head(batting$AB)

head(batting$X2B)

# Bating average
batting$BA <- batting$H / batting$AB

tail(batting$BA,5)

# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting
$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting
$X3B) + (4 * batting$HR) ) / batting$AB

str(batting)

sal <- read.csv('Salaries.csv')

summary(batting)

summary(sal)

batting <- subset(batting, yearID >= 1985)

summary(batting)

combo <- merge(batting, sal, by=c('playerID', 'yearID'))

summary(combo)

lost_players <- subset(combo, playerID %in% c('damonjo01', 'giambja01','saenzol01'))

lost_players

lost_players <- subset(lost_players, yearID == 2001)

lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

lost_players

avail_players <- filter(combo, yearID == 2001)

ggplot(avail_players, aes(x=OBP, y=salary)) + geom_point()

avail_players <- filter(avail_players, salary <8000000, OBP>0)

# calculate the sum of AB of the lost players
sum(lost_players$AB)

avail_players <- filter(avail_players, AB >= 490)

#sort by OBP
possible <- head(arrange(avail_players, desc(OBP)), 10)

possible <- possible[, c('playerID','OBP','AB','salary')]

possible

possible[2:4,]
