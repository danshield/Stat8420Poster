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
              box(plotlyOutput("netflix"),
                height = '80%',
                width = '100%'
              )
              #fluidRow(
              #  box(plotlyOutput("netflix")),
              #      width = '100%',
              #      height = '80%'
              #)
      )
    )
  )
)


server <- function(input, output) {
   #light grey boundaries
   l <- list(color = toRGB("grey"), width = 0.5)
  
   # specify map projection/options
   g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'mercator')
  )
   
  output$netflix <- renderPlotly({
    fig <- plot_geo(mapData) %>%
      add_trace(
        z = ~Total, color = ~Total, colors = 'Blues',
        text = ~Country, locations = ~code, marker = list(line = l)
      ) %>%
      colorbar(title = 'Titles', orientation="v", tickangle="0", len = ".8",
                            tickvals = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
                            ticktext = c('Less than 100','200', '300', '400', '500', '600', '700', '800', '900','More than 1000')) %>%
      layout(
        geo = g
      )
  })
}

shinyApp(ui, server)
