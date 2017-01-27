# helpers.R

# Draw station map
stationMap <- function(mapType) {
  if (mapType == 'trips') {
    popupText = paste(
      sep = '<br />',
      paste('<b>', stations$name, '</b>'),
      #paste('ID:', stations$station_id),
      paste('Docks:', stations$dockcount)
    )
  } else if(mapType == 'stations') {
    popupText = paste(
      sep = '<br />',
      paste('<b>', stations$name, '</b>'),
      paste('ID:', stations$station_id),
      paste('Docks:', stations$dockcount),
      paste('Installation date: ', stations$installation)
    )
  }

  leaflet(stations) %>% addProviderTiles('Stamen.Terrain') %>%
    addMarkers(popup = popupText)
}

# Add routes to station map
addRoutes <- function(map, routes, cutoff) {
  routes = filter(routes, n > cutoff)
  for (i in 1:nrow(routes)) {
    lonSt = as.numeric(routes[i, 'longStart'])
    lonEn = as.numeric(routes[i, 'longEnd'])
    latSt = as.numeric(routes[i, 'latStart'])
    latEn = as.numeric(routes[i, 'latEnd'])

    namSt = as.character(unlist(routes[i, 'nameStart']))
    namEn = as.character(unlist(routes[i, 'nameEnd']))

    n = routes[i, 'n']

    wt = as.integer((routes[i, 'n'] / sum(routes[, 'n'])) * 200 + 1)
    opac = (routes[i, 'n'] / sum(routes[, 'n'])) * 10

    #cat(paste(i, lonSt, lonEn, latSt, latEn, namSt, namEn, n, wt, opac))

    map %>% addPolylines(
      lng = c(lonSt, lonEn),
      lat = c(latSt, latEn),
      weight = wt,
      color = '#FF0000',
      opacity = opac,
      popup =
        paste(sep = '<br/>',
              paste0('<b>', namSt, ' - ', namEn, '</b>'),
              paste('Trips:', n)
        )
    )
  }
}

# bikeHistoOld = function(bikes, metric) {
#   # Question: How can I pass the content of metric directly into the function, i.e.
#   # aes(x=ifelse(metric == 'n', n))
#   # aes(x=eval(parse(text = 'n'))))
#   g = switch(
#     metric,
#     'n' = ggplot(bikes, aes(x=n)) + geom_histogram(binwidth=10^2),
#     'dur' = ggplot(bikes, aes(x=dur)) + geom_histogram(binwidth=10^5),
#     'medDur' = ggplot(bikes, aes(x=medDur)) + geom_histogram(binwidth=10^2)
#   )
#
#   g + theme_bw() +
#     labs(x = 'Bike usage', y = metric) +
#     ggtitle('Total bike usage per bike') +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       plot.subtitle = element_text(hjust = .5)
#     )
# }

# Bike histogram for various metrics
bikeHisto = function(bikes, metric) {
  bucketSize = switch(
    metric,
    'n' = 10^2,
    'dur' = 10^5 * 2,
    'medDur' = 10^2
  )

  cat(bucketSize)

  g  = gvisHistogram(
    select(
      bikes,
      # switch(
      #   metric,
      #   'Count' = n,
      #   'Total Duration' = dur,
      #   'Median Duration' = medDur
      # )
      eval(parse(text = paste(metric)))
    ),
    options=list(
      height=500,
      title='Bike usage',
      vAxis=
        paste(
          "{",
            "minorGridlines: { count: 1 }",
          "}"
        ),
      histogram=
        paste(
          "{",
            "bucketSize: ", bucketSize,
          "}"
        ),
      hAxis=
        paste(
          "{",
            "slantedText: true",
          "}"
        )
    )
  )
}

# getBikeMetric = function(choice) {
#   switch(
#     choice,
#     'Count' = 'n',
#     'Total Duration' = 'dur',
#     'Median Duration' = 'medDur'
#   )
# }

# Order bikes df by metric, desc or asc
orderBikes = function(df, metric, order='asc') {
  if (order == 'desc') {
    if (metric == 'n') {
      arrange(df, desc(n))
    } else if (metric == 'dur') {
      arrange(df, desc(dur))
    } else if (metric == 'medDur') {
      arrange(df, desc(medDur))
    }
  } else {
    if (metric == 'n') {
      arrange(df, n)
    } else if (metric == 'dur') {
      arrange(df, dur)
    } else if (metric == 'medDur') {
      arrange(df, medDur)
    }
  }
}

# stationBar = function(chart) {
#   y = ifelse(chart == 'staPerCity', 'Stations', 'Docks')
#
#   g = switch(
#     chart,
#     'staPerCity' = ggplot(data=stations, aes(x=landmark)) +
#       geom_bar(),
#
#     'dockPerCity' = ggplot(data=stations, aes(x=landmark, y=dockcount)) +
#       geom_bar(stat='identity') +
#       geom_text(aes(label=dockcount), position=position_dodge(width=0.9), vjust=-0.25)
#   )
#
#   g +
#     labs(x = 'City', y = y) +
#     ggtitle(paste(y, 'per city')) +
#     theme_bw() +
#     theme(
#       plot.title = element_text(hjust = 0.5),
#       plot.subtitle = element_text(hjust = .5)
#     ) +
#     scale_fill_brewer(palette = 'Blues')
# }

# Formatted string about trips
getTrip = function(routes, maxLen = 15) {
  rts = arrange(routes, desc(n))[1, ]

  start = as.character(rts$nameStart)
  start =
    paste0(
      substr(start, 1, maxLen),
      ifelse(nchar(start) > maxLen, '...', '')
    )

  end = as.character(rts$nameEnd)
  end =
    paste0(
      substr(end, 1, maxLen),
      ifelse(nchar(end) > maxLen, '...', '')
    )

  trip = paste(
    as.character(start),
    '-',
    as.character(end),
    paste0(
      '(',
      as.integer(rts$n),
      ')'
    )
  )

  return(trip)
}

# Rose chart, i.e. 24h clock, about station usage
stationRoseChart = function(staFreqs, staId) {
  staName = as.character((filter(stations, station_id == staId))$name)

  if ('Start.Terminal' %in% colnames(staFreqs)) {
    g = ggplot(filter(staFreqs, Start.Terminal == staId), aes(x=Start.Date.Hour, y=n, fill=n))
    staEnd = 'start'

  } else if ('End.Terminal' %in% colnames(staFreqs)) {
    g = ggplot(filter(staFreqs, End.Terminal == staId), aes(x=End.Date.Hour, y=n, fill=n))
    staEnd = 'end'
  }
  g +
    geom_bar(stat='identity') +
    coord_polar() +
    theme_bw() +
    labs(x = 'Hour of day', y = '', fill = 'No. of trips') +
    theme(
      plot.title = element_text(hjust = 0.5, size = 22),
      plot.subtitle = element_text(hjust = .5, size = 16),
      # axis.text.x = element_blank(),
      # axis.ticks.x = element_blank()
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    guides(fill = "legend") +
    ggtitle(paste('Station usage as trip', paste0(staEnd, 'ing'), 'point'), subtitle=staName)
}

# Calendar chart about station usage
stationCalendar = function(staTripData, staData, staId) {
  gvisCalendar(
    filter(staTripData, Start.Terminal == staId),
    datevar='Start.Date.Date',
    numvar='n',
    options = list(
      width='100%',
      title=paste('Trips date x station for', as.character((filter(staData, station_id == staId))$name)[1])
    )
  )
}

# sankeyTripsD3 = function(routes) {
#   g <- graph.tree(nrow(routes) + 1, children = 4)
#
#   E(g)$weight = 1
#   edges <- get.data.frame(g)
#
#   edges$from <- as.character(routes$nameStart)
#   edges$to <- as.character(routes$nameEnd)
#   edges$value <- routes$n
#
#   sankeyPlot <- rCharts$new()
#   sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey')
#   sankeyPlot$setTemplate(script = 'http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey/layouts/chart.html')
#
#   sankeyPlot$set(
#     data = edges,
#     nodeWidth = 15,
#     nodePadding = 10,
#     layout = 32,
#     width = 960,
#     height = 500
#   )
#
#   return(sankeyPlot)
# }

# Sankey chart about trips
sankeyTrips = function(routesData, cutoff) {
  gvisSankey(
    filter(routesData, n > cutoff),
    from='nameStart',
    to='nameEnd',
    weight='n',
    options=list(
      width='100%',
      height=500
    )
  )
}

# Column chart about stations, docks in cities
stationColumn = function(chart) {
  if (chart == 'staPerCity') {
    titleText = 'Stations'
    yVar = c('Stations')
  } else {
    titleText = 'Docks'
    yVar = c('Docks')
  }

  # titleText = 'Stations, Docks'
  # yVar = c('Stations', 'Docks')

  gvisColumnChart(
    docks,
    xvar='landmark',
    yvar=yVar,
    options = list(
      height=500,
      hAxis="{title: 'City'}",
      vAxis=
        paste(
          "{",
            "minorGridlines: { count: 1 },",
            "title: \'", titleText, "\'",
          "}"
        ),
      series=
        paste(
          "{",
            "0: { axis: 'stations' }, ",
            "1: { axis: 'docks' }",
          "}"
        ),
      axes=
        paste(
          "{",
            "y:",
            "{",
              "stations: { label: 'Stations' }, ",
              "docks: { label: 'Docks' }",
            "}",
          "}"
        ),
      title=paste(titleText, 'x City')
      #series="[{labelInLegend: 'Stations'}]"
    )
  )
}

# Timeline about bikes in operation
bikeTimeLine = function(daysFromTo) {
  cat(daysFromTo[1], daysFromTo[2])

  fromCol = '#89b5ff'
  toCol = '#29497e'
  bikeCol = paste0("'", colorRampPalette(c(fromCol, toCol))(nrow(bikes)), "'", collapse=',')


  gvisTimeline(
    data=filter(bikes, daysInUse >= daysFromTo[1] & daysInUse <= daysFromTo[2]),
    rowlabel="Bike.No",
    barlabel="Bike.No",
    start="minDate",
    end="maxDate",
    options=list(
      timeline="{groupByRowLabel:false}",
      height=500,
      width='99%',
      colors=paste0("[", bikeCol, "]")
    )
  )
}
