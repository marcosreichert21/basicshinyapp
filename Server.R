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
  ##Reactive functions for graphic
  rfun_graph <- reactive({
    if(input$var %in% c('pct_wins', 'pct_draws', 'pct_losses')) {
      function(x, y) sum((x * y)/sum(y))
    } else {
      function(x, y) sum(x)
    }
  })
  
  ##Graphic for tab statistics
  output$stats <- renderPlotly({
    p <- rval_tdates() %>%
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
             pct_losses = Losses/Matches)
    if(input$desc == 'desc'){
      teamFilter <- p %>% 
        summarise(order = rfun_graph()(.data[[input$var]], Matches)) %>% 
        arrange(-order) %>% 
        head(input$teams)
      
    p <-   p %>%
        filter(team %in% teamFilter$team) %>%
        mutate(temp = rfun_graph()(.data[[input$var]], Matches)) %>%
        ggplot(aes(x = fct_reorder(team, 
                                   -temp, 
                                   mean), 
                   fill = venue,
                   text = paste0('NT: ',
                                 team,
                                 '<br>',str_to_title(str_replace(input$var, '_', ' ')),
                                 ': ',
                                 if(input$var %in% c('pct_wins', 'pct_draws', 'pct_losses')) paste(round(.data[[input$var]],2)*100,'%') else .data[[input$var]] 
                   ))) +
        geom_bar(position = 'dodge', 
                 stat = 'identity', 
                 aes_string(y = input$var)) +
        theme_classic() +
        xlab('National Team') +
        ylab(str_to_title(str_replace(input$var, '_', ' '))) +
        labs(fill = 'Venue') +
        geom_text(aes(team, 
                      temp, 
                      label = if(input$var %in% c('pct_wins', 'pct_draws', 'pct_losses')) paste(round(temp,2)*100,'%') else temp, 
                      group = team),
                  size = 5,
                  vjust = .5)
    } else {
      teamFilter <- p %>% 
        summarise(order = rfun_graph()(.data[[input$var]], Matches)) %>% 
        arrange(order) %>% 
        head(input$teams)
      
     p <-  p %>%
        filter(team %in% teamFilter$team) %>%
        mutate(temp = rfun_graph()(.data[[input$var]], Matches)) %>%
        ggplot(aes(x = fct_reorder(team, 
                                   temp, 
                                   mean), 
                   fill = venue,
                   text = paste0('NT: ',
                                 team,
                                 '<br>',str_to_title(str_replace(input$var, '_', ' ')),
                                 ': ',
                                 if(input$var %in% c('pct_wins', 'pct_draws', 'pct_losses')) paste(round(.data[[input$var]],2)*100,'%') else .data[[input$var]] 
                   ))) +
        geom_bar(position = 'dodge', 
                 stat = 'identity', 
                 aes_string(y = input$var)) +
        theme_classic() +
        xlab('National Team') +
        ylab(str_to_title(str_replace(input$var, '_', ' '))) +
        labs(fill = 'Venue') +
        geom_text(aes(team, 
                      temp, 
                      label = if(input$var %in% c('pct_wins', 'pct_draws', 'pct_losses')) paste(round(temp,2)*100,'%') else temp, 
                      group = team),
                  size = 5,
                  vjust = .5)
    }
    
    ggplotly(p, tooltip = c('text')) %>%
      style(hoverinfo = "none", traces = c(4,5,6))
    
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
