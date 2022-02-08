H2H <- tabPanel(fluidRow(br(),
                         column(3,
                                selectizeInput('team',
                                            'Choose Team',
                                            choices = unique(Teams$team))
                                ),
                         column(3, 
                                h2('Number of Matches:', textOutput('kpi')),
                                align = 'center'
                         ),
                          column(width = 3, 
                                 #offset = 3,
                                 selectizeInput('opponent',
                                             'Choose Adversaries',
                                             choices = unique(Teams$opponent))
                                 )
                         ),
                 fluidRow(column(9,
                                 plotlyOutput('h2h')
                                 )
                          ),
                br(),
                 fluidRow(column(3, offset = 6,
                                selectInput('varh2h',
                                            'Choose variable',
                                            choices =c(`Goals For` = 'goals_for', 
                                            `Goals Against` = 'goals_against')
                                            )
                                )
                          ),
                fluidRow(column(9, 
                                hr()
                                )
                         ),
                fluidRow(column(9,
                                DTOutput('H2Hdt')
                                )
                         ),
                 title = 'Head-to-Head')
