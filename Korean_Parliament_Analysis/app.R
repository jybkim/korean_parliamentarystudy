library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)

# load the PR and SMD data



# Define UI for application 
ui <- navbarPage(theme = shinytheme("cerulean"),
    
    # the title
    
    title = "Korean Parliamentary Study",
    
    # About tab panel page
    
                tabPanel("About",
                         
                         # develop About page
                         fluidPage(
                             mainPanel(includeHTML('about.html')
                                       )
                         )
                ),
    
    # tab panel of Proportional Representation
    
                 tabPanel("Proportional Representation",
                          
                          # develop PR page
                          fluidPage(
                              titlePanel("Looking at Trends in Proportional Representation"),
                              
                              # put plotly of PR Representation over time
                              mainPanel(
                                  h4(strong("Proportional Representation from 1988 to 2016")),
                                  tags$ul(
                                    tags$li(h5("System of PR changed in 1996, 
                                             but the crux of voting by parties the same")),
                                    tags$li(h5("Independents are not considered since voting for parties
                                             only allowed in PR voting"))),
                                  plotlyOutput("PRinteractive", height = 650)
                              )
                              
                          )),

                 tabPanel("Single Member District",
                          tabsetPanel(
                              tabPanel("Subtab 2.1"),
                              tabPanel("Subtab 2.2")
                          )),
                 tabPanel("Statistical Analysis",
                          tabsetPanel(
                              tabPanel("Subtab 3.1"),
                              tabPanel("Subtab 3.2")
                          )),
                 )



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$PRinteractive <- renderPlotly({
        
        #display plot
        
        ggplotly(PRinteractive)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
