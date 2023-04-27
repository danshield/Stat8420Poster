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
  projection = list(type = 'mercator')
)

fig <- plot_geo(data)
fig <- fig %>% add_trace(
  z = ~Total, color = ~Total, colors = 'Blues',
  text = ~Country, locations = ~code, marker = list(line = l)
)

fig <- fig %>% colorbar(title = 'Titles', orientation="v", tickangle="0", len = ".8",
                        tickvals = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
                        ticktext = c('Less than 100','200', '300', '400', '500', '600', '700', '800', '900','More than 1000'))
fig <- fig %>% layout(
  geo = g
)

fig



