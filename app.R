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

df <- get_data()

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "COVID-19 Monitor Northeastern University"), 
    
    dashboardSidebar(disable = TRUE), 
    
    # primary layout 
    dashboardBody(
        fluidRow(
            valueBoxOutput(width = 3, "confirmedCount"), 
            valueBoxOutput(width = 3, "recoveredCount"), 
            valueBoxOutput(width = 3, "deathsCount"), 
            valueBoxOutput(width = 3, "activeCount")
        ),
        
        fluidPage(
            leafletOutput("map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$confirmedCount <- renderValueBox(
        valueBox(
            paste(sum(df$Confirmed)), "Confirmed Cases", 
            color = "red", icon = icon("user-check")
        )
    )
    
    output$recoveredCount <- renderValueBox(
        valueBox(
            paste(sum(df$Recovered)), "Recovered Cases", 
            color = "green", icon = icon("user-shield")
        )
    )
    
    output$deathsCount <- renderValueBox(
        valueBox(
            paste(sum(df$Deaths)), "Death Cases", 
            color = "purple", icon = icon("user-slash")
        )
    )
    
    output$activeCount <- renderValueBox(
        valueBox(
            paste(sum(df$Active)), "Active Cases", 
            color = "yellow", icon = icon("user-clock")
        )
    )
    
    output$map <- renderLeaflet(
        leaflet(df) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Confirmed*15, popup = ~State, 
                       fillOpacity = 0.3, color = "red")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
