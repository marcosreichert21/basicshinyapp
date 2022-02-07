##Server Side
server <- function(input, output, session) {
  rval_tdates <- reactive({
    Teams %>%
      filter(date >= input$dates[1],
             date <= input$dates[2],
             if(!'All' %in% input$tourney) tournament %in% input$tourney else T,
             if(input$yncountry == 'Yes') country %in% input$countrydesc  else T)
  })
  output$teste <- renderPlotly({
    teamFilter <- count(rval_tdates(), team) %>% arrange(desc(n)) %>% head(input$teams)
    p <- rval_tdates() %>%
      filter(team %in% teamFilter$team) %>%
      group_by(team, venue) %>%
      summarise(Matches = n(),
                Goals_For = sum(goals_for),
                Goals_Against = sum(goals_against),
                Goals_Diff = sum(goals_for)-sum(goals_against),
                Wins = sum(ifelse(results == 'Win',1,0)),
                Draws = sum(ifelse(results == 'Draw',1,0)),
                Losses = sum(ifelse(results == 'Lose',1,0))
      ) %>%
      mutate(pct_wins = Wins/Matches,
             pct_draws = Draws/Matches,
             pct_losses = Losses/Matches) %>%
      ggplot(aes(x = fct_reorder(team, desc(.[[input$var]])), 
                 fill = venue)) +
     geom_bar(position = 'dodge', 
              stat = 'identity', 
              aes_string(y = input$var)) +
     theme_classic() +
     xlab('National Team') +
     ylab(input$var) +
     labs(fill = 'Venue')
    ggplotly(p)
  })
}
