library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(gganimate)
library(gifski)

# load the PR and SMD data

source("PR.R")
source("SMD_region.R")
source("SMD_statanalysis.R")

# make a list

newlist <- as.list(SMD_partyshare$region) %>%
    unique()


# Define UI for application 
ui <- navbarPage(theme = shinytheme("cerulean"),
    
    # the title
    
    title = "Korean Parliamentary Study", 
    
    # first tab panel of Proportional Representation
    
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
                                             only allowed in PR voting")),
                                    tags$li(h5("Animation shows the changes in PR voting behavior."))),
                                  imageOutput("graphanimate_actual")
                              )
                              
                          )),

                 tabPanel("Single Member District",
                          fluidPage(
                              
                              # add title to the Single Member District
                              titlePanel("Regional Vote Trends"), 
                              
                              # add sidebar Panel with selector to show descriptions
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      h5(strong("Selecting a Region")),
                                      selectInput(
                                          inputId = "region",
                                          label = "Options of City/Province",
                                          choices = newlist
                                      ),
                                      p("Some cities were separated from existing provinces/cities after 1988."),
                                      p("This may explain shortage in datapoints for certain areas.")
                                  ), 
                                  
                                  # add the mainPanel page
                                  mainPanel(
                                      h4(strong("Voting by Region from 1988 to 2016")),
                                      tags$ul(
                                          tags$li(h5("Elections happen every 4 years.")),
                                          tags$li(h5("Elections in between those 4-year period are by-elections.")),
                                          tags$li(h5("Hover over the graph and zoom in to see exact details!"))),
                                      plotlyOutput("graphregion", height = 650)
                              )
                          )
                          )),
                 tabPanel("Statistical Analysis",
                          fluidPage(
                              
                              # add title to the Single Member District
                              titlePanel("Linear Regression"), 
                              
                              # add sidebar Panel with selector to show descriptions
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      h5(strong("Statistical Analysis")),
                                      p("We often see in the United States that there's a strong correlation
                                        between older voters and conservative vote shares. In the
                                        2018 midterm elections for United States, the only age group
                                        that broke for Republicans was those 65 and older."),
                                      p("I wanted to see if such a correlation existed with 
                                        Korean conservatives. After all, those in the same age group
                                        in Korea remember the horrors of the Korean War and 
                                        living through the tough times in the immediate aftermath 
                                        of Korean War. They firsthand contributed to the rapid economic
                                        growth that the country had experienced up until the mid 1990s, 
                                        and the conservative movement is often seen as the heir to 
                                        the government that reigned during the economic miracle."), 
                                      p("The regression done here was between shares of older population in various
                                      cities/provinces throughout different election years 
                                      and overall vote share for different ideologies. Each point in the
                                      scatter plot represents a specific city/province in various election
                                      years, with multiple points for the same x-value to depict the 
                                      difference in vote shares for different ideologies."),
                                      p("Reason for not choosing a specific province or specific election year
                                        was to demonstrate the consistency of 65+ voter behavior throughout
                                        different elections."), 
                                      p("For more certainty, additional variables could have been 
                                        provided to prove some relationship between population of seniors and
                                        the average vote share in any given district and election year. Specifically,
                                        this model does not account for the potential variables that may change based
                                        on the various election years this regression takes into factor. 
                                        Wealth is also not taken into consideration, which may affect voting behavior
                                        regardless of the age of voter."),
                                      p("NOTE: Interaction regression table may not show liberal/progressives
                                        because it is codified as 0 in a ternary variable, with 1 as conservatives
                                        and 2 as independents.")
                                  ), 
                                  
                                  # add the mainPanel page
                                  mainPanel(
                                      h4(strong("Regression Graph")),
                                      plotOutput("regressionplot", height = 650),
                                      h4(strong("Regression with Interaction of Old Population
                                                and Party/Ideology")),
                                      gt_output("interaction_gt")
                                  )
                              )
                          )),
    
    # developing About page
    
    tabPanel("About",
             
             # develop About page
             fluidPage(theme = shinytheme("cerulean"),
                       mainPanel(
                           htmlOutput("page")
                       )
             ))
    
    )



# Define server logic required to draw the graph
server <- function(input, output) {
    
    # define function to include the about page
    
    getPage <- function() {
        return(includeHTML("about.html"))
    }
    
    # include the output page for the about tab
    
    output$page <- renderUI({getPage()})
    
    # output for inputselector 
    
    # but first, make a function that returns differnt things
    
    plottest <- reactive({
        if ("Busan" %in% input$region) return(Busan)
        if ("Chungbuk" %in% input$region) return(Chungbuk)
        if ("Chungnam" %in% input$region) return(Chungnam)
        if ("Daegu" %in% input$region) return(Daegu)
        if ("Gangwon" %in% input$region) return(Gangwon)
        if ("Gwangju" %in% input$region) return(Gwangju)
        if ("Gyeongbuk" %in% input$region) return(Gyeongbuk)
        if ("Gyeonggi" %in% input$region) return(Gyeonggi)
        if ("Gyeongnam" %in% input$region) return(Gyeongnam)
        if ("Incheon" %in% input$region) return(Incheon)
        if ("Jeju" %in% input$region) return(Jeju) 
        if ("Jeonbuk" %in% input$region) return(Jeonbuk)
        if ("Jeonnam" %in% input$region) return(Jeonnam)
        if ("Seoul" %in% input$region) return(Seoul)
        if ("Daejeon" %in% input$region) return(Daejeon)
        if ("Ulsan" %in% input$region) return(Ulsan)
        if ("Sejong" %in% input$region) return(Sejong)
    })
    
    output$graphregion <- renderPlotly({
        
        # load in the rdata for smd_partyshare
        SMD_partyshare <- read_rds("data_files/SMD_partyshare.rds")
        
        # call the plottest function to work and print
        plots = plottest()
        print(plots)

    })
    
    # output for regressionplot
    
    output$regressionplot <- renderPlot({
        
        regressionplot
        
    })
    
    # output for the interaction_gt
    
    output$interaction_gt <- render_gt({
        
        interaction_gt
    })
    
    # output for the PR

    output$graphanimate_actual <- renderImage({
        filename <- file.path('data_files/graphanimate_actual.gif')
        
        # Return a list containing the filename and alt text
        list(src = filename,
             contentType = 'image/gif',
             alt = 'Animation of PR vote in Korea')
        
    }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
