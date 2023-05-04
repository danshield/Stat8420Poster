library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(plotly)
library(usmap)

# To create and save omaha map data from goole
# omaps <-  get_map(location = 'Omaha', maptype = 'roadmap', zoom = 11, color='bw')
# save(omaps, file = "omaps.RData") # get_map(location = 'Omaha', source = 'stamen', maptype = 'toner')
# Once saved, we don't need to connect google, we can just load
# I have done this step already. Just get the omaps.RData from canvas

netflixData <- read.csv('netflix_titles.csv')
mapData <- read.csv('choropleth.csv')
mapData$Total[mapData$Total <= 10] <- 10
mapData$Total[mapData$Total >= 1000] <- 1000

minYear <- 2008
maxYear <- 2020

ui <- dashboardPage(
  dashboardHeader(title = "My Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("My control Center", tabName = "Netflix", 
               icon = icon("dashboard"))
    )    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Netflix",
              fluidRow(
                width = 12,
                box(sliderInput("slider", "Countries with Netflix available by year",
                                sep = '',
                                min = minYear, 
                                max = maxYear,
                                value = minYear), 
                    width='90%')),
              fluidRow(
                width = 12,
                plotlyOutput("netflix"),
                  height = '100%',
                  width = '90%'
              )
      )
    )
  )
)


server <- function(input, output) {
   #light grey boundaries
   l <- list(color = toRGB("grey"), width = 0.5)
  
   # specify map projection/options
   g <- list(
    showframe = TRUE,
    showcoastlines = FALSE,
    projection = list(type = 'mercator')
  )
   
  output$netflix <- renderPlotly({
#    data <- mapData %>%
#            filter(Introduced <= input$slider)
    
    chart <- data %>%
      mutate(available = ifelse(Introduced <= input$slider, 1, 0))
    
    fig <- plot_geo(chart, width = 800, height = 800) %>%
      add_trace(
        z = ~available, color = ~available, colors = 'Blues',
        text = ~Country, locations = ~code, marker = list(line = l),
        showscale = FALSE
      ) %>%
      layout(
        geo = g,
        showlegend = FALSE,
        modebar = FALSE
#        width = 600, 
#        height = 600
      ) %>%
      config(displayModeBar = FALSE)
  })
  
}

shinyApp(ui, server)
