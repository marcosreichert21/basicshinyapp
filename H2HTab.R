H2H <- tabPanel(fluidRow(br(),
                         column(3,
                                selectizeInput('team',
                                            'Choose Team',
                                            choices = unique(Teams$team))
                                ),
                          column(width = 3, 
                                 offset = 3,
                                 selectizeInput('opponent',
                                             'Choose Adversaries',
                                             multiple = T,
                                             choices = unique(Teams$opponent))
                                 )
                         ),
                 fluidRow(column(9,
                                 plotlyOutput('h2h')
                                 )
                          ),
                 title = 'Head-to-Head')
