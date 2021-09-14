install.packages("nflfastR")
install.packages("tidyverse", type = "binary")

library(nflfastR)
library(tidyverse)
library(ggplot2)

pbp2020 <- load_pbp(2020)

# "Success" - if play results in a positive EPA - Expected Points Added

# Filters By Running Plays, Groups by Rusher, Averages Success Rate and Yards
# Gained, Sums Carries, arranges by success rate
allNFL <- pbp2020 %>%
  filter(rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    success_rate = mean(success), ypc = mean(yards_gained), plays = n()
  ) %>%
  arrange(-success_rate) %>%
  filter(plays > 20)
view(allNFL)

# Creates plots of YPC vs Success Rate and YPC vs Plays

ggplot(allNFL, aes(success_rate,ypc))+geom_point()
ggplot(allNFL, aes(plays,ypc))+geom_point()

# Filters for exclusively Derrick Henry carries, removes 2pt conversions
# Plots carries by point in game and by down

Henry <- pbp2020 %>% filter(rusher=="D.Henry", down !="NA")
Henry$game_seconds_remaining = Henry$game_seconds_remaining*-1
hist(Henry$game_seconds_remaining,main="Derrick Henry Carries Through Game - 2020",
     xlab = "Seconds Remaining", ylab = "Carries",col="#4B92DB")
abline(v=-3000,col="red",lwd=4)
abline(v=-2000,col="red",lwd=4)
abline(v=-1000,col="red",lwd=4)

hist(Henry$down,main="Derrick Henry Carries by Down - 2020", 
     xlab = "Down", ylab = "Carries",col="#4B92DB",breaks = c(0,1,2,3,4))
ggplot(Henry,aes(down))+
       geom_histogram(binwidth=1, fill="#4B92DB",color="#e9ecef") + 
      ggtitle("Derrick Henry Carries By Down - 2020") +
      xlab("Down") + ylab("Carries")+ 
      theme(plot.title = element_text(size = 20, hjust="0.5", face = "bold"))

      