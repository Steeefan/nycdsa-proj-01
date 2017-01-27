# server.R

shinyServer(function(input, output, session) {

  # finalMap <- reactive({
  #   if (input$tripsWhich == 2) {
  #     addRoutes(stationMap('trips'), routesABe)
  #   } else if (input$tripsWhich == 3) {
  #     stationMap('trips')
  #   }
  # })

  output$tripsMap = renderLeaflet(
    stationMap('trips')
  )

  observeEvent({
      input$tripsWhich
      input$tripsCutoff
    },
    {
      proxy = leafletProxy('tripsMap')
      clearShapes(proxy)

      if (input$tripsWhich > 0) {
        # if (input$tripsWhich == 1) {
        #   routesForMap = routesABne
        # } else if (input$tripsWhich == 2) {
        #   routesForMap = routesABe
        # }

        routesForMap = eval(parse(text = paste(input$tripsWhich)))

        proxy %>% addRoutes(routesForMap, input$tripsCutoff)
      }
  })

  output$maxTripABe = renderInfoBox({
    trip = getTrip(routesABe)

    infoBox(h4('Trip most taken (A:B=B:A)'), h6(trip), icon=icon('arrow-up'))
  })

  output$maxTripABne = renderInfoBox({
    trip = getTrip(routesABne)

    infoBox(h4('Trip most taken (A:B!=B:A)'), h6(trip), icon=icon('arrow-up'))
  })

  output$tripCount = renderInfoBox({
    infoBox(h4('Trips'), format(nrow(trips), scientific=F, decimal.mark=".", big.mark=","), icon=icon('exchange'))
  })

  output$bikeCount = renderInfoBox({
    infoBox(
      h4('Bikes in use'),
      n_distinct(bikes),
      icon = icon('bicycle')
    )
  })

  output$maxBike = renderInfoBox({
    metric = input$bikesMetric
    bks = orderBikes(bikes, metric, 'desc')

    maxVal = as.integer(bks[1, metric])
    maxBike = as.integer(bks[1, 'Bike.No'])

    infoBox(
      h4(paste('Bike', maxBike)),
      paste(format(round(maxVal / ifelse(metric != 'n', 60, 1), 1), scientific=F, decimal.mark=".", big.mark=","), ifelse(metric != 'n', 'min', '')),
      icon = icon('arrow-up')
    )
  })

  output$minBike = renderInfoBox({
    metric = input$bikesMetric
    bks = orderBikes(bikes, metric)

    minVal = as.integer(bks[1, metric])
    minBike = as.integer(bks[1, 'Bike.No'])

    infoBox(
      h4(paste('Bike', minBike)),
      paste(round(minVal / ifelse(metric != 'n', 60, 1), 1), ifelse(metric != 'n', 'min', '')),
      icon = icon('arrow-down')
    )
  })

  output$bikesPlot = renderGvis(
    bikeHisto(bikes, input$bikesMetric)
  )

  output$bikesOps = renderGvis(
    bikeTimeLine(input$bikeOpsDays)
  )

  # STATIONS
  output$stationsMap = renderLeaflet(
    stationMap('stations')
  )

  output$staCount = renderInfoBox(
    infoBox(h4('Stations'), nrow(stations), icon = icon('home'))
  )

  output$cityCount = renderInfoBox(
    infoBox(h4('Cities'), nrow(distinct(stations, landmark)), icon = icon('map-signs'))
  )

  output$dockCount = renderInfoBox(
    infoBox(h4('Docks'), format(sum(stations$dockcount), scientific=F, decimal.mark=".", big.mark=","), icon = icon('plug'))
  )

  output$staPerCity = renderGvis(
    #stationBar('staPerCity')
    stationColumn('staPerCity')
  )

  output$dockPerCity = renderGvis(
    #stationBar('dockPerCity')
    stationColumn('dockPerCity')
  )

  output$staStartByHour = renderPlot(
    stationRoseChart(staStartByHour, input$stationDetail)
  )

  output$staEndByHour = renderPlot(
    stationRoseChart(staEndByHour, input$stationDetail)
  )

  output$staStartByHourComp1 = renderPlot(
    stationRoseChart(staStartByHour, input$staComp1)
  )

  output$staStartByHourComp2 = renderPlot(
    stationRoseChart(staStartByHour, input$staComp2)
  )

  output$staEndByHourComp1 = renderPlot(
    stationRoseChart(staEndByHour, input$staComp1)
  )

  output$staEndByHourComp2 = renderPlot(
    stationRoseChart(staEndByHour, input$staComp2)
  )

  output$staCalendar = renderGvis(
    stationCalendar(staStartByDate, stations, input$stationDetail)
  )

  output$staCalendarComp1 = renderGvis(
    stationCalendar(staStartByDate, stations, input$staComp1)
  )

  output$staCalendarComp2 = renderGvis(
    stationCalendar(staStartByDate, stations, input$staComp2)
  )

  output$tripsSankey = renderGvis({
    if (input$tripsWhich == 0) {
      routesData = data.frame(nameStart='From', nameEnd='To', n=input$tripsCutoff+1)
    } else {
      routesData = eval(parse(text = paste0(input$tripsWhich, 'Sankey')))
    }

    sankeyTrips(routesData, input$tripsCutoff)
  })

  output$tripsTable = renderGvis(
    gvisTable(
      transmute(
        eval(parse(text = paste(input$tripsWhich))),
        Start = nameStart,
        End = nameEnd,
        Trips = n,
        Total.Duration = round(totalDur / 60, 2),
        Min.Duration = round(minDur / 60, 2),
        Max.Duration = round(maxDur / 60, 2),
        Avg.Duration = round(avgDur / 60, 2),
        Med.Duration = round(medDur / 60, 2)
      )
    )
  )

  output$custSubscr = renderValueBox(
    valueBox(
      format(as.integer(select(filter(cust, Subscriber.Type == 'Subscriber'), n)), scientific=F, decimal.mark=".", big.mark=","),
      'Trips by Subscribers',
      icon=icon('address-book-o')
    )
  )

  output$custCust = renderValueBox(
    valueBox(
      format(as.integer(select(filter(cust, Subscriber.Type == 'Customer'), n)), scientific=F, decimal.mark=".", big.mark=","),
      'Trips by Customers',
      icon=icon('credit-card')
    )
  )

  output$custSubscrVsCust = renderValueBox(
    valueBox(
      round(
        as.integer(select(filter(cust, Subscriber.Type == 'Subscriber'), n)) /
        as.integer(select(filter(cust, Subscriber.Type == 'Customer'), n)),
        3
      ),
      'Trips: Customers vs. Subscribers',
      icon=icon('line-chart')
    )
  )

  output$custSubscrDur = renderValueBox(
    valueBox(
      paste0(
        format(as.integer(select(filter(cust, Subscriber.Type == 'Subscriber'), dur) / 60 / 60), scientific=F, decimal.mark=".", big.mark=","),
        'h'
      ),
      'Duration by Subscribers',
      icon=icon('clock-o')
    )
  )

  output$custCustDur = renderValueBox(
    valueBox(
      paste0(
        format(as.integer(select(filter(cust, Subscriber.Type == 'Customer'), dur) / 60 / 60), scientific=F, decimal.mark=".", big.mark=","),
        'h'
      ),
      'Duration by Customers',
      icon=icon('clock-o')
    )
  )

  output$custSubscrVsCustDur = renderValueBox(
    valueBox(
      round(
        as.integer(select(filter(cust, Subscriber.Type == 'Subscriber'), dur)) /
        as.integer(select(filter(cust, Subscriber.Type == 'Customer'), dur)),
        3
      ),
      'Duration: Customers vs. Subscribers',
      icon=icon('bar-chart')
    )
  )

  output$custSubscrMedDur = renderValueBox(
    valueBox(
      paste0(
        format(as.integer(select(filter(cust, Subscriber.Type == 'Subscriber'), medDur) / 60), scientific=F, decimal.mark=".", big.mark=","),
        'min'
      ),
      'Med. Duration by Subscribers',
      icon=icon('clock-o')
    )
  )

  output$custCustMedDur = renderValueBox(
    valueBox(
      paste0(
        format(as.integer(select(filter(cust, Subscriber.Type == 'Customer'), medDur) / 60), scientific=F, decimal.mark=".", big.mark=","),
        'min'
      ),
      'Med. Duration by Customers',
      icon=icon('clock-o')
    )
  )

  output$custSubscrVsCustMedDur = renderValueBox(
    valueBox(
      round(
        as.integer(select(filter(cust, Subscriber.Type == 'Subscriber'), medDur)) /
          as.integer(select(filter(cust, Subscriber.Type == 'Customer'), medDur)),
        3
      ),
      'Med. Duration: Customers vs. Subscribers',
      icon=icon('area-chart')
    )
  )

  output$datesLoaded = renderInfoBox(
    infoBox(
      h4('Data loaded for'),
      paste(
        min(trips$Start.Date),
        '-',
        max(trips$End.Date)
      ),
      icon = icon('calendar')
    )
  )
})

