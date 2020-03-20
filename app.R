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
library(DT)
library(maps)
library(mapproj)

get_data <- function(){
    confirmed.cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
    confirmed.cases <- confirmed.cases[, c(1:4, ncol(confirmed.cases))]
    recovered.cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
    recovered.cases <- recovered.cases[, c(1:4, ncol(recovered.cases))]
    deaths.cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
    deaths.cases <- deaths.cases[, c(1:4, ncol(deaths.cases))]
    all.cases <- confirmed.cases %>% 
        inner_join(recovered.cases, by = names(confirmed.cases[, 1:4])) %>%
        inner_join(deaths.cases, by = names(confirmed.cases[, 1:4]))
    names(all.cases) <- c("State", "Country", "Lat", "Long", "Confirmed", "Recovered", "Deaths")
    all.cases <- mutate(all.cases, Active = Confirmed - Recovered - Deaths)
    return(all.cases)
}

df <- get_data()

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("COVID-19 Monitor Northeastern University"),
    
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
        paste("Total Confirmed: ", sum(df$Confirmed))
    ) 
    
    output$confirmed <- DT::renderDataTable(
        df %>% 
            group_by(Country) %>% 
            summarise(Total = sum(Confirmed)) %>% 
            arrange(-Total), 
        rownames = FALSE, 
        extensions = c("Scroller"), 
        options = list(info = FALSE, scroller = TRUE, scrollY = 500, searching = FALSE)
    ) 
    
    output$totalDeaths <- renderText(
        paste("Total Deaths: ", sum(df$Deaths))
    )
    
    output$deaths <- DT::renderDataTable(
        df %>%  
            group_by(Country) %>% 
            summarise(Total = sum(Deaths)) %>% 
            arrange(-Total), 
        rownames = FALSE, 
        extensions = c("Scroller"), 
        options = list(info = FALSE, scroller = TRUE, scrollY = 200, searching = FALSE)
    )
    
    output$totalRecovered <- renderText(
        paste("Total Recovered: ", sum(df$Recovered))
    )
    
    output$recovered <- DT::renderDataTable(
        df %>% 
            group_by(Country) %>% 
            summarise(Total = sum(Recovered)) %>% 
            arrange(-Total), 
        rownames = FALSE, 
        extensions = c("Scroller"), 
        options = list(info = FALSE, scroller = TRUE, scrollY = 200, searching = FALSE)
    )
    
    output$cumulativeConfirmed <- renderPlot(
        ggplot() + 
            geom_polygon(data = map_data("world"), 
                         aes(x = long, y = lat, group = group), 
                         fill = "grey", alpha = 0.8) + 
            geom_point(data = df, 
                       aes(x = Long, y = Lat, size = Confirmed), color = "red", alpha = 0.3) + 
            theme_void() + 
            theme(legend.position = "bottom") + 
            coord_cartesian(ylim = c(-60, 80)) 
    )
    
    output$active <- renderPlot(
        ggplot() + 
            geom_polygon(data = map_data("world"), 
                         aes(x = long, y = lat, group = group), 
                         fill = "grey", alpha = 0.8) + 
            geom_point(data = df, 
                       aes(x = Long, y = Lat, size = Active), color = "orange", alpha = 0.3) + 
            theme_void() +  
            scale_size_binned(breaks = c(10, 100, 1000, 5000, 10000, 25000, 50000))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
