library(plotly)

data <- read.csv('./choropleth.csv')

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'mercator')
)

fig <- plot_geo(data)
fig <- fig %>% add_trace(
  z = ~Total, color = ~Total, colors = 'Blues',
  text = ~Country, locations = ~code, marker = list(line = l)
)
fig <- fig %>% colorbar(title = 'Year Launched')
fig <- fig %>% layout(
  title = 'Number of Titles Availalbe on Netflix',
  geo = g
)

fig

