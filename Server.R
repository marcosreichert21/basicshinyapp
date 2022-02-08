##Server Side
server <- function(input, output, session) {
  ##Reactive for the filter
  rval_tdates <- reactive({
    Teams %>%
      filter(date >= input$dates[1],
             date <= input$dates[2],
             if(!'All' %in% input$tourney) tournament %in% input$tourney else T,
             if(input$yncountry == 'Yes') country %in% input$countrydesc  else T)
  })
  
  ##Graphic for tab statistics
  output$stats <- renderPlotly({
    teamFilter <- count(rval_tdates(), team) %>% arrange(desc(n)) %>% head(input$teams)
    rval_tdates() %>%
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
      ggplot(aes(x = fct_reorder(team, desc(.[[input$var]]), sum), 
                 fill = venue)) +
     geom_bar(position = 'dodge', 
              stat = 'identity', 
              aes_string(y = input$var)) +
     theme_classic() +
     xlab('National Team') +
     ylab(str_to_title(str_replace(input$var, '_', ' '))) +
     labs(fill = 'Venue')
    #ggplotly(p)
    #p
  })
  
  ##Reactive function for H2H tab
  rval_h2h <- reactive({rval_tdates() %>%
                          filter(team %in% c(input$team, input$opponent),
                                 opponent %in% c(input$team, input$opponent))
  })
  ##Graphic for H2H tab
  output$h2h <- renderPlotly({
      rval_h2h() %>%
      ggplot(aes(x = team, fill = results)) +
      geom_bar(aes_string(y = input$varh2h), stat = 'identity') +
      theme_classic() +
      xlab('National Team') +
      ylab(str_to_title(str_replace(input$varh2h, '_', ' '))) +
      labs(fill = 'Results')
  })
  ##KPI Output
  output$kpi <- renderText({
    nrow(rval_h2h())/2
  })
  ##H2H DT
  output$H2Hdt <- renderDT({
    datatable(rval_tdates() %>%
      filter(team == input$team,
             opponent == input$opponent) %>%
      rename_all(funs(
        str_replace_all(., '_', ' ') %>%
          str_to_title(.)
      )),
      caption = 'Match details')
  })
  ##Details DT
  output$details <- renderDT({
    SoccerResults %>% 
      filter(date >= input$dates[1],
                             date <= input$dates[2],
                             if(!'All' %in% input$tourney) tournament %in% input$tourney else T,
                             if(input$yncountry == 'Yes') country %in% input$countrydesc  else T) %>%
      rename_all(funs(
        str_replace_all(., '_', ' ') %>%
          str_to_title(.)))
      
  })
}
