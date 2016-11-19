#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
# Mapa de localización ####
  
  output$Location_Map <- renderLeaflet({
    if (input$Location_Map == TRUE) {
      leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      ####  Map####
    #Initial view #
    setView(lng = -102.5528, 
            lat = 23.6345,
            zoom = 5)
}
  })
  
  
  
  # Mapa de resultados ####

  FEDECOOP <- paste(sep = "<br/>",
                   "<b><a href='http://www.fedecoop.com.mx'>MetaID 342</a></b>",
                   "Lobster Stock Assesment and Catch from FEDECOOP since 1970",
                   "Private Dataset"
  )
  
PescaIcon <- makeIcon(
  iconUrl = "http://www.clker.com/cliparts/9/g/m/K/T/2/fish-icon-md.png",
  iconWidth = 20, iconHeight = 20
  
)

CasaIcon <- makeIcon(
  iconUrl = "https://cdn3.iconfinder.com/data/icons/glypho-free/64/home-128.png",
  iconWidth = 20, iconHeight = 20
)

output$map <- renderLeaflet({
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    ####  Map####
  #Initial view #
  setView(lng = -102.5528, 
          lat = 23.6345,
          zoom = 5) %>% 
    # Data Points ####
  addMarkers(lng = -109.4725,
             lat = 24.6356, 
             popup = "7654")%>%
    addMarkers(lng = -95.8084852,
               lat = 20.5764432,
               popup = "MetaID 423"
               )%>%
    addMarkers(lng = -107.8084852,
               lat = 15.5764432,
               popup = "Meta ID = 343. Short Title: Buoy ID4: sst, salinity and currents since 1980"
    )%>%
  addRectangles(
    lng1=-107.607877, lat1=18.459820,
    lng2=-110.557877, lat2=17.431188,
    color="red",
    fillColor = "red",
    fillOpacity = 0,
    popup = "Meta ID = 546. Short Title: Hammerhead Shark Stock Assesment between 2000-2014"
  ) %>% 
    addRectangles(
      lng1=-115.507877, lat1=28.400000,
      lng2=-115.557877, lat2=28.421188,
      color="green",
      fillColor = "green",
      fillOpacity = 0.2,
      popup = FEDECOOP
    )
  
})


})