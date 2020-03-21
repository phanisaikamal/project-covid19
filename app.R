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
library(leaflet)
library(shinydashboard)

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

df <- get_data() %>% 
    filter(Confirmed > 0)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "COVID-19 Monitor"), 
    
    dashboardSidebar(disable = TRUE), 
    
    # primary layout 
    dashboardBody(
        fluidRow(
            valueBoxOutput(width = 3, "confirmedCount"), 
            valueBoxOutput(width = 3, "recoveredCount"), 
            valueBoxOutput(width = 3, "deathsCount"), 
            valueBoxOutput(width = 3, "activeCount")
        ),
        
        fluidRow(
            column(
                width = 2, 
                box(
                    width = NULL, 
                    solidHeader = TRUE, background = "red", 
                    dataTableOutput("confirmedTable")
                )
            ), 
            
            column(
                width = 8, 
                tabBox(
                    side = "right", width = "100%", 
                    selected = "Confirmed Cases", 
                    tabPanel("Active Cases", leafletOutput("activeMap")), 
                    tabPanel("Death Cases", leafletOutput("deathsMap")), 
                    tabPanel("Recovered Cases", leafletOutput("recoveredMap")), 
                    tabPanel("Confirmed Cases", leafletOutput("confirmedMap"))
                ) 
            ), 
            
            column(
                width = 2, 
                box(
                    width = NULL, 
                    solidHeader = TRUE, background = "green", 
                    dataTableOutput("recoveredTable")
                ), 
                box(
                    width = NULL, 
                    solidHeader = TRUE, background = "purple", 
                    dataTableOutput("deathsTable")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$confirmedCount <- renderValueBox(
        valueBox(
            paste(sum(df$Confirmed, na.rm = TRUE)), "Confirmed Cases", 
            color = "red", icon = icon("user-check")
        )
    )
    
    output$recoveredCount <- renderValueBox(
        valueBox(
            paste(sum(df$Recovered, na.rm = TRUE)), "Recovered Cases", 
            color = "green", icon = icon("user-shield")
        )
    )
    
    output$deathsCount <- renderValueBox(
        valueBox(
            paste(sum(df$Deaths, na.rm = TRUE)), "Death Cases", 
            color = "purple", icon = icon("user-slash")
        )
    )
    
    output$activeCount <- renderValueBox(
        valueBox(
            paste(sum(df$Active, na.rm = TRUE)), "Active Cases", 
            color = "yellow", icon = icon("user-clock")
        )
    )
    
    output$confirmedTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Confirmed, na.rm = TRUE)) %>% 
                arrange(-Total) %>% 
                head(10), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
    
    output$activeMap <- renderLeaflet(
        leaflet(df) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Active*20, popup = ~State, 
                       fillOpacity = 0.5, color = "orange") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) 
    )
    
    output$deathsMap <- renderLeaflet(
        leaflet(df) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Deaths*20, popup = ~State, 
                       fillOpacity = 0.5, color = "purple") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) 
    )
    
    output$recoveredMap <- renderLeaflet(
        leaflet(df) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Recovered*20, popup = ~State, 
                       fillOpacity = 0.5, color = "green") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) 
    )
    
    output$confirmedMap <- renderLeaflet(
        leaflet(df) %>%  
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Confirmed*50, label = ~as.character(paste(if(is_empty(df$State) != TRUE){df$State}, 
                                                                           df$Country, "-", 
                                                                           "Confirmed: ", Confirmed)), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.5, color = "red") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) 
    )
    
    output$recoveredTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Recovered, na.rm = TRUE)) %>% 
                arrange(-Total) %>% 
                head(5), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
    
    output$deathsTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Deaths, na.rm = TRUE)) %>% 
                arrange(-Total) %>% 
                head(5), 
            rownames = FALSE, 
            options = list(searching = FALSE, 
                           paging = FALSE, 
                           info = FALSE)
        ) %>% 
            formatStyle(
                c('Country', 'Total'), 
                color = 'black', backgroundColor = NULL, fontWeight = 'bold'
            )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)