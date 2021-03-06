##Cleaning Leftover Data
rm(list=ls())

my.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.dir)

###Loads packages
library(tidyverse)
library(lubridate)

#Loads Dataset
SoccerResults <- read_csv('results.csv')

#Creates a DataFrame with the stats for Home teams
Home <- SoccerResults %>%
         mutate(team = home_team, 
                opponent = away_team, 
                goals_for = home_score,
                goals_against = away_score,
                year = year(date),
                venue = ifelse(neutral==T, 'Neutral','Home')) %>%
         select(date, 
                team,
                opponent,
                goals_for,
                goals_against,
                tournament,
                city,
                country,
                year,
                venue)

##Creates results vector
hresults <- c()

for (i in 1:nrow(Home)) {
  if (Home$goals_for[i]>Home$goals_against[i]) {
    hresults[i] <- 'Win'
  } else if (Home$goals_for[i]==Home$goals_against[i]) {
    hresults[i] <- 'Draw'
  } else { hresults[i] <- 'Lose'}
}

##Creates results columns
Home$results <- hresults

##Creates Dataframe with the stats for Away Teams
Away <- SoccerResults %>%
  mutate(team = away_team, 
         opponent = home_team, 
         goals_for = away_score,
         goals_against = home_score,
         year = year(date),
         venue = ifelse(neutral==T, 'Neutral','Away')) %>%
  select(date, 
         team,
         opponent,
         goals_for,
         goals_against,
         tournament,
         city,
         country,
         year,
         venue)

#Creates new results vector (I could have simply made the opposite of hresults, but it doesn't matter now)
aresults <- c()

for (i in 1:nrow(Away)) {
  if (Away$goals_for[i]>Away$goals_against[i]) {
    aresults[i] <- 'Win'
  } else if (Away$goals_for[i]==Away$goals_against[i]) {
    aresults[i] <- 'Draw'
  } else { aresults[i] <- 'Lose'}
}

#Creates results column
Away$results <- aresults

#Binds Dataframe
Teams <- bind_rows(Home, Away)

#Counts number of matches for top teams (Brazil 977)
teamFilter <- count(Teams, team) %>% arrange(desc(n)) %>% head(10)

#Checks if the number is equal (it is)
SoccerResults %>% filter(home_team == 'Brazil' | away_team == 'Brazil')

#Cleans old dataframes
rm(list = c('Away', 'Home')) 

# 
# 
# fig <-  Teams %>%
#   filter(date >= '1930-01-01',
#          date <= '2019-01-01') %>%
#   group_by(team, venue) %>%
#   summarise(Matches = n(),
#             Goals_For = sum(goals_for),
#             Goals_Against = sum(goals_against),
#             Goals_Diff = sum(goals_for)-sum(goals_against),
#             Wins = sum(ifelse(results == 'Win',1,0)),
#             Draws = sum(ifelse(results == 'Draw',1,0)),
#             Losses = sum(ifelse(results == 'Lose',1,0))
#   ) %>%
#   mutate(pct_wins = Wins/Matches,
#          pct_draws = Draws/Matches,
#          pct_losses = Losses/Matches) 
# 
# pcts <- c('pct_wins', 'pct_draws', 'pct_draws')
# teste <- 'pct_wins'
# 
# 
# 
# teste2 <- if(teste %in% pcts) {
#   function(x, y) sum((x * y)/sum(y))
# } else {
#   function(x, y) sum(x)
# }
# 
# newvar <- 'bottom'
# 
# 
# teamFilter <- fig %>% 
#   summarise(temp = teste2(.data[[teste]], Matches)) %>% 
#   arrange(-temp) %>% 
#   head(10)
# 
# label <- ifelse(teste %in% pcts, paste(round(temp,2)*100,'%'))
# 
# plot <- fig %>%
#   filter(team %in% teamFilter$team) %>%
#   mutate(temp = teste2(.data[[teste]], Matches)) %>%
#   ggplot(aes(x = fct_reorder(team, 
#                              -temp, 
#                              mean), 
#              fill = venue, 
#              text = paste0('NT: ',
#                            team,
#                            '<br>',str_to_title(str_replace(teste, '_', ' ')),
#                            ': ',
#                            if(teste %in% pcts) paste(round(.data[[teste]],2)*100,'%') else .data[[teste]] 
#              )))  +
#   geom_bar(position = 'dodge', 
#            stat = 'identity', 
#            aes_string(y = 'pct_wins')) +
#   geom_text(aes(team, 
#                 temp, 
#                 label = if(teste %in% pcts) paste(round(temp,2)*100,'%') else temp, 
#                 group = team
#                 ),
#             size = 5,
#             vjust = .5)
# 
# abc <- ggplotly(plot, tooltip = c('text')) %>%
#   style(hoverinfo = "none", traces = c(4,5,6))
# abc
# 
# warning <- fig %>%
#   filter(team %in% teamFilter$team) %>%
#   summarise(order = teste2(x = pct_wins, y = Matches))
  

