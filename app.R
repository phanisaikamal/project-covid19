#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(coronavirus)
library(DT)
library(maps)
library(mapproj)

df <- as.data.frame(coronavirus)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(h1("COVID-19 Monitor Northeastern University")),
    
    # primary layout 
    sidebarLayout(
        sidebarPanel(
            verticalLayout(
                h3(textOutput("totalConfirmed")), 
                dataTableOutput("confirmed")
            ), 
            width = 3
        ),
        mainPanel(
            sidebarLayout(
                sidebarPanel(
                    verticalLayout(
                        verticalLayout(
                            h3(textOutput("totalDeaths")), 
                            dataTableOutput("deaths"), 
                            fluid = TRUE
                        ), 
                        verticalLayout(
                            h3(textOutput("totalRecovered")), 
                            dataTableOutput("recovered"), 
                            fluid = TRUE
                        ),
                        fluid = TRUE
                    ), 
                    width = 4
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Global Spread Map", 
                            tabsetPanel(
                                tabPanel(
                                    "Cumulative Confirmed Cases", 
                                    plotOutput("cumulativeConfirmed")
                                ),
                                tabPanel(
                                    "Active Cases", 
                                    plotOutput("active")
                                )
                            )
                        ),
                        tabPanel(
                            "Growth Charts", 
                            tabsetPanel(
                                tabPanel(
                                    "Actual Growth", 
                                    plotOutput("actualGrowth")
                                ),
                                tabPanel(
                                    "Forecasted Growth", 
                                    plotOutput("forecastGrowth")
                                )
                            )
                        )
                    ), 
                    width = 8
                ),
                position = c("right")
            ), 
            width = 9
        ),
        position = c("left", "right")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$totalConfirmed <- renderText(
        paste("Total Confimed: ", 
                  df %>% 
                      filter(type == "confirmed") %>% 
                      summarise(sum(cases))
        )
    ) 
    
    output$confirmed <- DT::renderDataTable(
        df %>% 
            filter(type == "confirmed") %>% 
            group_by(Country = Country.Region) %>% 
            summarise(Total = sum(cases)) %>% 
            arrange(-Total), 
        rownames = FALSE, 
        extensions = c("Scroller"), 
        options = list(info = FALSE, scroller = TRUE, scrollY = 500, searching = FALSE)
    ) 
    
    output$totalDeaths <- renderText(
        paste("Total Deaths: ", 
              df %>% 
                  filter(type == "death") %>% 
                  summarise(sum(cases))
        )
    )
    
    output$deaths <- DT::renderDataTable(
        df %>% 
            filter(type == "death") %>% 
            group_by(Country = Country.Region) %>% 
            summarise(Total = sum(cases)) %>% 
            arrange(-Total), 
        rownames = FALSE, 
        extensions = c("Scroller"), 
        options = list(info = FALSE, scroller = TRUE, scrollY = 200, searching = FALSE)
    )
    
    output$totalRecovered <- renderText(
        paste("Total Recovered: ", 
              df %>% 
                  filter(type == "recovered") %>% 
                  summarise(sum(cases))
        )
    )
    
    output$recovered <- DT::renderDataTable(
        df %>% 
            filter(type == "recovered") %>% 
            group_by(Country = Country.Region) %>% 
            summarise(Total = sum(cases)) %>% 
            arrange(-Total), 
        rownames = FALSE, 
        extensions = c("Scroller"), 
        options = list(info = FALSE, scroller = TRUE, scrollY = 200, searching = FALSE)
    )
    
    output$cumulativeConfirmed <- renderPlot(
        ggplot() + 
            geom_polygon(data = map_data("world"), 
                         aes(x = long, y = lat, group = group), 
                         fill = "grey", alpha = 0.5) + 
            geom_point(data = df %>% filter(type == "confirmed"), 
                       aes(x = Long, y = Lat, size = cases), color = "red", alpha = 0.3) + 
            theme_void() + 
            coord_map()
    )
    
    output$active <- renderPlot(
        ggplot() + 
            geom_polygon(data = map_data("world"), 
                         aes(x = long, y = lat, group = group), 
                         fill = "grey", alpha = 0.5) +  
            theme_void() + 
            coord_map()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
