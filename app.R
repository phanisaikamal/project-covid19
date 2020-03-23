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

hits <- function(){
    if(!file.exists("pageHitsCount.Rdata"))
        counter <- 0
    else
        load(file = "pageHitsCount.Rdata")
    counter <- counter + 1 
    save(counter, file = "pageHitsCount.Rdata")
    return(counter)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "COVID-19 Monitor"), 
    
    dashboardSidebar(disable = TRUE), 
    
    # primary layout 
    dashboardBody(
        fluidRow(
            valueBoxOutput(width = 3, "confirmedCount"), 
            valueBoxOutput(width = 2, "recoveredCount"), 
            valueBoxOutput(width = 2, "deathsCount"), 
            valueBoxOutput(width = 3, "activeCount"), 
            valueBoxOutput(width = 2, "hitsCount")
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
    
    df <- get_data() %>% 
        filter(Confirmed > 0)
    
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
    
    output$hitsCount <- renderValueBox(
        valueBox(
            paste(hits()), "Page Hits", 
            color = "teal", icon = icon("chart-line")
        )
    )
    
    output$confirmedTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Confirmed)) %>% 
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
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Active*20, label = ~as.character(gsub("NA ,", "", 
                                                                       paste(State, ",", 
                                                                             Country, "-", 
                                                                             "Active: ", Active))), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.5, color = "orange") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$deathsMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Deaths*20, label = ~as.character(gsub("NA ,", "", 
                                                                       paste(State, ",", 
                                                                             Country, "-", 
                                                                             "Deaths: ", Deaths))), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.5, color = "purple") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$recoveredMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5)) %>% 
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Recovered*20, label = ~as.character(gsub("NA ,", "", 
                                                                          paste(State, ",", 
                                                                                Country, "-", 
                                                                                "Recovered: ", Recovered))), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.5, color = "green") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$confirmedMap <- renderLeaflet(
        leaflet(df, options = leafletOptions(minZoom = 1, maxZoom = 5)) %>%  
            addTiles() %>% 
            addCircles(lng = ~Long, lat = ~Lat, weight = 1, 
                       radius = ~Confirmed*20, label = ~as.character(gsub("NA ,", "", 
                                                                          paste(State, ",", 
                                                                                Country, "-", 
                                                                                "Confirmed: ", Confirmed))), 
                       labelOptions = labelOptions(noHide = FALSE), 
                       fillOpacity = 0.5, color = "red") %>% 
            setView(lng = 0, lat = 0, zoom = 1.5) %>% 
            setMaxBounds(-180, 90, 180, -90) 
    )
    
    output$recoveredTable <- DT::renderDataTable(
        datatable(
            df %>% 
                group_by(Country) %>% 
                summarise(Total = sum(Recovered)) %>% 
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
                summarise(Total = sum(Deaths)) %>% 
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