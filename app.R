library(dplyr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(plotly)
library(usmap)
library(DT)
library(shinyWidgets)

netflixData <- read.csv('updated.csv')
netflixData$listed_in <- sapply(strsplit(netflixData$listed_in, ','), as.list)
netflixData$listed_in <- sapply(netflixData$listed_in, function(x) {trimws(x)})

mapData <- read.csv('choropleth.csv')
mapData$Total[mapData$Total <= 10] <- 10
mapData$Total[mapData$Total >= 1000] <- 1000


          
minYear <- 2008
maxYear <- 2020

filterGenres <- function(column) {
  genres <- unique(unlist(column))
  genres <- lapply(trimws(genres), as.character)
  unique(sort(unlist(genres)))
}

getGenres <- function(typ) {
  column <- netflixData %>%
              filter(type == typ) %>%
              select(listed_in)
  filterGenres(column$listed_in)
}

tvGenres <- getGenres('TV Show')
movieGenres <- getGenres('Movie')

movieRatings <- netflixData %>%
                  filter(type == 'Movie') %>%
                  select(rating) %>%
                  distinct(rating)


ui <- dashboardPage(
  dashboardHeader(title = "Stats 8426"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Domination", tabName = "Netflix", 
               icon = icon("dashboard")),
      menuItem("Television", tabName = "TV", 
               icon = icon("dashboard")),
      menuItem("Movies", tabName = "Movies", 
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
                                value = minYear,
                                ticks = TRUE,
                                animate = TRUE), 
                    width='90%')),
              fluidRow(
                width = 12,
                plotlyOutput("netflix"),
                  height = '100%',
                  width = '90%'
              )
      ),
      tabItem(tabName="TV",
              fluidRow(box(
                selectInput('tvGenre', "Genre", choices = tvGenres)
              )),
              fluidRow(width=12, DT::dataTableOutput('tv'))
              ),
      tabItem(tabName="Movies",
              fluidRow(box(
                selectInput("movieRating", "Rating", choices = movieRatings)
                )
              ),
              fluidRow(width=12, DT::dataTableOutput('movie'))
              )
      )
    )
)


server <- function(input, output, session) {
   #light grey boundaries
   l <- list(color = toRGB("grey"), width = 0.5)
  
   # specify map projection/options
   g <- list(
     showframe = TRUE,
     showcoastlines = FALSE,
     projection = list(type = 'mercator')
   )
   
   movieRatings <- netflixData %>%
     filter(type == 'Movie')
   
  output$netflix <- renderPlotly({
    val <- input$slider
    
    chart <- mapData %>%
      mutate(available = ifelse(Introduced <= val, 1, 0))
    
    fig <- plot_geo(chart, width = 800, height = 700, hoverinfo = 'text') %>%
      add_trace(
        z = ~available, color = ~Introduced, colors = 'Blues',
        text = ~Country, locations = ~code, marker = list(line = l),
        showscale = FALSE
      ) %>%
      layout(
        geo = g,
        showlegend = FALSE,
        modebar = FALSE,
        title = list("foo")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$tv <- DT::renderDataTable({
    netflixData %>%
      filter(type == 'TV Show') %>%
      filter(sapply(listed_in, function(x) { any(input$tvGenre %in% x)})) %>%
      select(c(title, rating, release_year, description)) %>%
      arrange(title)
  })
  
  output$movie <- DT::renderDataTable({
    netflixData %>%
      filter(type == 'Movie') %>%
      filter(rating == input$movieRating) %>%
      select(c(title, rating, release_year, description)) %>%
      arrange(title)
  })
  
}

shinyApp(ui, server)
