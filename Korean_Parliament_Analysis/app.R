library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(gganimate)

# load the PR and SMD data

source("PR.R")
source("SMD_region.R")

# make a list

newlist <- as.list(SMD_partyshare$region) %>%
    unique()


# Define UI for application 
ui <- navbarPage(theme = shinytheme("cerulean"),
    
    # the title
    
    title = "Korean Parliamentary Study", 
    
    # first tab panel of Proportional Representation
    
                tabPanel("About",

                         # develop About page
                         fluidPage(theme = shinytheme("cerulean"),
                             mainPanel(
                                 htmlOutput("page")
                             )
                         )),
    
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
                                          tags$li(h5("Elections in between those 4-year period are by-elections."))),
                                      plotOutput("graphregion", height = 650)
                              )
                          )
                          )),
                 tabPanel("Statistical Analysis",
                          tabsetPanel(
                              tabPanel("Subtab 3.1"),
                              tabPanel("Subtab 3.2")
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
    
    output$graphregion <- renderPlot({
        
        # load in the rdata for smd_partyshare
        SMD_partyshare <- read_rds("data_files/SMD_partyshare.rds")

        # first making the ggplot

        SMD_partyshare %>%
            filter(region == input$region) %>%
            ggplot(aes(x = year, y = averagevoteshare, color = cons_or_not)) +
                       # text = paste("Year: ", year,
                       #              "<br>Vote Share: ", round(averagevoteshare, digits = 2), "%",
                       #              "<br>Party/Ideology: ", cons_or_not))) +
            geom_line() +
            scale_colour_manual(values = c("blue", "red", "yellow3")) +

            # scale the years
            scale_x_continuous(breaks = seq(1988, 2016, by = 4),
                               labels = c("1988", "'92", "'96", "2000",
                                          "'04", "'08", "'12", "'16")) +

            # scale the percentage
            scale_y_continuous(breaks = seq(10, 100, by = 10),
                                limits = c(0, 100)) +

            # add title to graph
            labs(title = "Conservative vs. Liberal vs. Independent Vote Share \n for Regional Districts",
                 subtitle = "Data in Between Breaks are By-Elections",
                 caption  = "Data from Kuniaki Nemoto, Musashi University",
                 y = "Percent", x = "Year", color = "Legend") +

            theme_classic()

        
        # SMD_partyshare %>%
        #     filter(region == input$region) %>%
        #     ggplot(aes(x = year, y = averagevoteshare, group = cons_or_not)) + 
        #     geom_line() +
        #     geom_segment(aes(xend = 2016, yend = averagevoteshare), linetype = 2, colour = 'grey') + 
        #     geom_point(size = 2) + 
        #     geom_text(aes(x = 2016.2, label = cons_or_not), hjust = 0) + 
        #     transition_reveal(year) + 
        #     theme_minimal() + 
        #     labs(title = "Vote Shares for Different Parties/Ideology Groups", 
        #          y = "Percent", x = "Year")
        
    
        # ggplotly(x)
            #      , tooltip = "text") %>%
            # layout(dragmode = "pan") %>%
            # config(displayModeBar = "static", displaylogo = FALSE, 
            #        modeBarButtonsToRemove = list("sendDataToCloud", "toImage"))

        # # choose plot based on input$region
        # 
        # ifelse(input$region == "busan",
        #        graphregion <- ggplotly(busan),
        #        graphregion <- ggplotly(ulsan)) %>%
        # 
        #     ifelse(input$region == "chungbuk",
        #            graphregion <- ggplotly(chungbuk), .) %>%
        # 
        #     ifelse(input$region == "chungnam",
        #            graphregion <- ggplotly(chungnam), .) %>%
        # 
        #     ifelse(input$region == "daegu",
        #            graphregion <- ggplotly(daegu), .) %>%
        # 
        #     ifelse(input$region == "daejeon",
        #            graphregion <- ggplotly(daejeon), .) %>%
        # 
        #     ifelse(input$region == "gangwon",
        #            graphregion <- ggplotly(gangwon), .) %>%
        # 
        #     ifelse(input$region == "gwangju",
        #            graphregion <- ggplotly(gwangju), .) %>%
        # 
        #     ifelse(input$region == "gyeongbuk",
        #            graphregion <- ggplotly(gyeongbuk), .) %>%
        # 
        #     ifelse(input$region == "gyeonggi",
        #            graphregion <- ggplotly(gyeonggi), .) %>%
        # 
        #     ifelse(input$region == "gyeongnam",
        #            graphregion <- ggplotly(gyeongnam), .) %>%
        # 
        #     ifelse(input$region == "incheon",
        #            graphregion <- ggplotly(incheon), .) %>%
        # 
        #     ifelse(input$region == "jeju",
        #            graphregion <- ggplotly(jeju), .) %>%
        # 
        #     ifelse(input$region == "jeonbuk",
        #            graphregion <- ggplotly(jeonbuk), .) %>%
        # 
        #     ifelse(input$region == "jeonnam",
        #            graphregion <- ggplotly(jeonnam), .) %>%
        # 
        #     ifelse(input$region == "seoul",
        #            graphregion <- ggplotly(seoul), .) %>%
        # 
        #     ifelse(input$region == "sejong",
        #            graphregion <- ggplotly(sejong), .) %>%
        # 
        #     ifelse(input$region == "ulsan",
        #            graphregion <- ggplotly(ulsan), .)

    })
    
    # output for the PR

    output$PRinteractive <- renderPlotly({
        
        # display plot
        
        ggplotly(PRinteractive)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
