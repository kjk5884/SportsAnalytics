library(rvest)
library(dplyr)

updateEfficiency <- function(){
  ratingLink = "https://www.basketball-reference.com/leagues/NBA_2022_ratings.html"
  ratingPage = read_html(ratingLink)
  # pulls the text, then removes the links
  team = ratingPage %>% html_nodes("#ratings a") %>% html_text()
  adjoRating = as.numeric(ratingPage %>% html_nodes("td:nth-child(13)") %>% html_text())
  adjdRating = as.numeric(ratingPage %>% html_nodes("td:nth-child(14)") %>% html_text())
  teamData = data.frame(team, adjoRating, adjdRating, stringsAsFactors = FALSE)
}

updateSchedule <- function(scheduleLink){
  schedulePage = read_html(scheduleLink)
  date = schedulePage %>% html_nodes("th.left") %>% html_text()
  visitor = schedulePage %>% html_nodes(".left:nth-child(3)") %>% html_text()
  visitor = visitor[0:(length(visitor)-1)]
  home = schedulePage %>% html_nodes(".left:nth-child(5)") %>% html_text()
  schedule = data.frame(date,home,visitor, stringsAsFactors = FALSE)
}

dailyProjections <- function(schedule, date1, teamData){
  todayGames <- filter(schedule, date==date1)
  teamStats.int <- merge(todayGames,teamData,by.x=(c("home")),by.y=(c("team")))
  teamStats <- merge(teamStats.int,teamData,by.x=(c("visitor")),by.y=(c("team")))
  colnames(teamStats) <- c("visitor", "home", "date", "hmAdjO", "hmAdjD", "visAdjO", "visAdjD")
  teamStats <- teamStats[,c(3,1,6,7,2,4,5)]
  
  projScore <- data.frame(teamStats$visitor, (teamStats$visAdjO+teamStats$hmAdjD)/2,
                          teamStats$home, (teamStats$hmAdjO+teamStats$visAdjD)/2)
  colnames(projScore) <- c("visitor","visScore","home","hmScore")
  projScore$homeSpread <- projScore$visScore-projScore$hmScore-3
  # incorporates bonus for home games
  return(data.frame(projScore))
}
novScheduleLink = "https://www.basketball-reference.com/leagues/NBA_2022_games-november.html"
decScheduleLink = "https://www.basketball-reference.com/leagues/NBA_2022_games-december.html"


# RUN THIS TO FIND PROJECTIONS
efficiency <- updateEfficiency()
monthlySchedule <- updateSchedule(novScheduleLink)
todayDate <- "Tue, Nov 23, 2021"
nov23results <- dailyProjections(monthlySchedule,todayDate,efficiency)
View(nov23results)
