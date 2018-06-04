library(shiny)
library(geosphere)
library(htmltools)
library(leaflet)
library(data.table)
library(plotGoogleMaps)
library(reshape)
library(proxy)


shinyServer(function(input, output,session){
  
  # Initializing the data
  loc_data <- data.table(readRDS('data/All_data_app.Rds'))
  e_dt <- read.csv('data/Entities_mapping.csv', stringsAsFactors = F)
  
  api="https://maps.googleapis.com/maps/api/js?key=AIzaSyBk1iNGBRPz1DQuKwtKzSwRFHckK996YPw"
  
  
  output$plotLocations = renderUI({
    
    loc_sub = loc_data[loc_data$e_key %in% input$entity & loc_data$city_id == input$city,1:3]
    loc_sub = merge(loc_sub,e_dt, by = 'e_key')
    
    coordinates(loc_sub) <-~ longitude +latitude # Create cordinates
    proj4string(loc_sub) = CRS('+proj=longlat +datum=WGS84') # Add Projections
    
    m <- mcGoogleMaps(loc_sub, mapTypeId='ROADMAP', zcol = "entity", api = api,filename = 'myMap1.html', openMap = F)
    
    tags$iframe(
      srcdoc = paste(readLines('myMap1.html'), collapse = '\n'),
      width = "100%",
      height = "800px"
    )
  })
  
  output$plotClusters = renderUI({
    
    loc_sub = loc_data[loc_data$e_key %in% input$entity & loc_data$city_id == input$city,1:3]
    loc_sub = merge(loc_sub,e_dt, by = 'e_key',na.rm=TRUE)
    
    coordinates(loc_sub) <-~ longitude +latitude # Create cordinates
    proj4string(loc_sub) = CRS('+proj=longlat +datum=WGS84') # Add Projections
    
    
    m <- mcGoogleMaps(loc_sub,filename = 'myMap2.html', mapTypeId='ROADMAP', zcol = "entity", api = api, apiMarkerClusterer = 'markerclusterer.js', openMap = F)
    
    tags$iframe(
      srcdoc = paste(readLines('myMap2.html'), collapse = '\n'),
      width = "100%",
      height = "800px"
    )
    
  })
  
  output$plotClusteredLocations = renderUI({ 
    
    loc_sub = loc_data[loc_data$e_key %in% input$entity & loc_data$city_id == input$city,1:3]
    loc_sub = merge(loc_sub,e_dt, by = 'e_key')
    loc_sub = loc_sub[,2:3]
    
    # loc_sub$id = 1:nrow(loc_sub)
    loc_dt <- expand.grid.df(loc_sub,loc_sub)
    names(loc_dt)[3:4] <- c("latitude_dest","longitude_dest")
    # 
    setDT(loc_dt)[ , dist_km := distGeo(matrix(c(longitude, latitude), ncol = 2),
                                        matrix(c(longitude_dest, latitude_dest), ncol = 2))/1000]
    distm = matrix(loc_dt$dist_km,sqrt(nrow(loc_dt)),sqrt(nrow(loc_dt)))
    
    # Create clusters based in distances
    fit <- hclust(as.dist(distm), method="ward.D2")
    plot(fit) # display dendogram
    #
    groups <- cutree(fit, k=input$k) # cut tree into 18 clusters
    # # draw dendogram with red borders around the 18 clusters
    rect.hclust(fit, k=input$k, border="red")
    loc_sub$group = groups # Assign cluster groups
    # # Plot stores with clustor as label
    
    coordinates(loc_sub) <-~ longitude +latitude # Create cordinates
    proj4string(loc_sub) = CRS('+proj=longlat +datum=WGS84') # Add Projections
    
    # Assign cluster groups
    # Plot stores with clustor as label
    m <- mcGoogleMaps(loc_sub, mapTypeId='ROADMAP', zcol="group", api = api,filename = 'myMap3.html', openMap = F)
    
    tags$iframe(
      srcdoc = paste(readLines('myMap3.html'), collapse = '\n'),
      width = "100%",
      height = "800px"
    )
    
  })
  
  })
