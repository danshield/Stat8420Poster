library(plotly)

data <- read.csv('./choropleth.csv')

data$Total[data$Total <= 10] <- 10
data$Total[data$Total >= 1000] <- 1000

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  colorbar = 'bottom',
  projection = list(type = 'mercator')
)

fig <- plot_geo(data)
fig <- fig %>% add_trace(
  z = ~Total, color = ~Total, colors = 'Blues',
  text = ~Country, locations = ~code, marker = list(line = l)
)

fig <- fig %>% colorbar(title = 'Titles', x = 0.5, y=0.33, orientation="h", tickangle="-45", len = ".9",
                        tickvals = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
                        ticktext = c('Less than 100','200', '300', '400', '500', '600', '700', '800', '900','More than 1000'))
fig <- fig %>% layout(
  title = 'Number of Titles Available on Netflix by Country',
  geo = g
)

fig


