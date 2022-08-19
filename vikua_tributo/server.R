
shinyServer(function(input, output, session) {
  
  # check_credentials returns a function to authenticate users------
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -64.6313, lat = 10.1989, zoom = 14)
  })
  
  # A reactive expression that returns the set of zips that are in bounds right now
  contribuyentesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(contribuyente_features[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(contribuyente_features,
           lat >= latRng[1] & lon <= latRng[2] &
           lat >= lngRng[1] & lon <= lngRng[2])
  })
  
  output$MapHistImporte <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(contribuyentesInBounds()) == 0)
      return(NULL)
    
    # hist(contribuyentesInBounds()$importe_usd,
    #      breaks = centileBreaks,
    #      main = "Importe USD",
    #      xlab = "Percentile",
    #      col = '#00DD00',
    #      border = 'white')
    
    contribuyentesInBounds() %>% 
      ggplot(aes(importe_usd)) +
      geom_histogram(bins = 30) +
      theme_minimal() +
      labs(x = 'Importe USD', y = 'N Contribuyentes')
  })
  
  output$mapTopContribuyentes <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(contribuyentesInBounds()) == 0)
      return(NULL)
    
    contribuyente_features %>% 
      group_by(actividad_econ = str_trunc(actividad_econ, 40)) %>% 
      summarise(importe_usd = sum(importe_usd)) %>% 
      slice_max(order_by = importe_usd, n = 10) %>% 
      ggplot(aes(x = importe_usd, y = reorder(actividad_econ, importe_usd))) +
      geom_col() +
      theme_minimal() +
      scale_x_continuous(labels = scales::label_dollar(scale = 1/1000)) +
      labs(x = 'Importe USD', y ='')
    
    
    # print(xyplot(pagado ~ pendiente, data = contribuyentesInBounds(), 
    #              xlim = range(contribuyente_features$pendiente), ylim = range(contribuyente_features$pagado)))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    
    act_econ_elegida <- input$color_act_econ
    color_definition <- input$color_definition
    
    if(is.null(color_definition)){color_definition <- 'importe_usd'}
    
    if(color_definition == 'act_econ'){
      colorData <- contribuyente_features %>%
        mutate(
          colorBy = if_else(
            actividad_econ == act_econ_elegida, act_econ_elegida, 'Otros') %>% 
            as_factor() %>% 
            forcats::fct_relevel('Otros'),
          opacityBy = if_else(colorBy == 'Otros', .6, 1),
          sizeBy = if_else(colorBy == 'Otros', 10, 15)
        )
      pal <- colorFactor(c('darkgreen', 'tomato'), colorData$colorBy)
      
      leafletProxy("map", data = colorData) %>%
        clearShapes() %>%
        addCircles(
          ~lon, ~lat, layerId = ~registro_de_informacion_fiscal_rif, stroke=FALSE, radius = ~sizeBy,
          label = ~lineas_de_factura_empresa_mostrar_nombre,
          fillOpacity=~opacityBy, 
          fillColor=pal(colorData$colorBy),
        ) %>%
        addLegend("bottomleft", pal=pal, values=colorData$colorBy, title='Actividad Economica',
                  layerId="colorLegend")
      
    } else{
      
      color_definition_values <- contribuyente_features[[color_definition]] %>% 
        cut(c(0, 100, 500, 1000, 10000, 50000, max(.)))
            
      pal <- colorFactor("viridis", color_definition_values)
      
      leafletProxy("map", data = contribuyente_features) %>%
        clearShapes() %>%
        addCircles(~lon, ~lat, layerId = ~registro_de_informacion_fiscal_rif, radius = 12, 
                   label = ~lineas_de_factura_empresa_mostrar_nombre,
                   stroke=FALSE, fillOpacity=0.8, fillColor=pal(color_definition_values)) %>%
        addLegend("bottomleft", pal=pal, values=color_definition_values, title = 'Importe USD',
                  layerId="colorLegend")
    }
    

  })

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showContribuyentePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      rif <- input$goto$rif
      lat <- input$goto$lat
      lng <- input$goto$lng
      showContribuyentePopup(rif, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$contribuyente <- DT::renderDataTable({
    
    df <- contribuyente_features %>%
      filter(
        actividad_econ %in% input$actividad_economica
      ) %>%
      mutate(
        Action = paste(
          '<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '" data-rif="',
          registro_de_informacion_fiscal_rif, '"><i class="fa fa-crosshairs"></i></a>', 
          sep="")
      )
    
    action <- DT::dataTableAjax(session, df, outputId = "contribuyente")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
})
