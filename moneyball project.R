#MONEYBALL PROJECT

#Reading the batting file
batting <- read.csv('Batting.csv')
head(batting)
str(batting)

#Calculating:- Batting Average(BA)
#              On Base Percentage(OBP)
#              Slugging Percentage(SLG)

batting$BA = batting$H/batting$AB
batting$OBP = (batting$H + batting$BB + batting$HBP)/
  (batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B = batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG = (batting$X1B + (2 * batting$X2B) + (3 * batting$X3B) + 4* batting$HR)/
   batting$AB
str(batting)
# Reading the salaries file
sal <- read.csv('Salaries.csv')
summary(batting)
summary(sal)

# Our batting data goes back to 1871!
# Our salary data starts at 1985,
# meaning we need to remove the batting data that occured before 1985.
batting <- subset(batting, batting$yearID >= 1985)
summary(batting)

# Now it is time to merge the batting data with the salary data!
# Since we have players playing multiple years,
# we'll have repetitions of playerIDs for multiple years,
# meaning we want to merge on both players and years

merged_dfs <- merge(batting,sal,by = c('playerID','yearID'))
summary(merged_dfs)

# The players lost were:
# first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees,
# outfielder Johnny Damon (damonjo01) to the Boston Red Sox and
# infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').

lost_players = subset(merged_dfs, playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))

# Since all these players were lost in after 2001 in the offseason,
# let's only concern ourselves with the data from 2001

lost_players = subset(lost_players, lost_players$yearID == '2001')
lost_players
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]

# Now, we have all the infomartion. The constraints are :-
# The total combined salary of the three players can not exceed 15 million dollars.
# Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# Their mean OBP had to equal to or greater than the mean OBP of the lost players

install.packages('dplyr')
library(dplyr)
avail.players <- filter(merged_dfs, yearID == 2001)
avail.players
install.packages('ggplot2')
library(ggplot2)

# Then I made a quick plot to see where I should cut-off for salary in respect to OBP

a <- ggplot(avail.players, aes(x=avail.players$OBP,y=avail.players$salary)) + geom_point()
print(a)

# according to graph, there is no point in investing more than 8 million dollars on one player
# and there are many players with OBP = 0.

avail.players <- filter(avail.players, salary<800000, OBP>0)

#The total AB of the lost players is 1469. This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB.

avail.players <- filter(avail.players,AB>=500)

possible <- head(arrange(avail.players,desc(OBP)),10)
# Grab columns I'm interested in
possible <- possible[,c('playerID','OBP','AB','salary')]
possible

#Can't choose giambja again, but the other ones look good (2-4). I choose them.

possible[2:4,]

# Great, looks like I just saved the 2001 Oakland A's a lot of money! If only I had a time machine and R,
# I could have made a lot of money in 2001 picking players!