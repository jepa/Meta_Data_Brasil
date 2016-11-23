#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet) #For the maps
library(DT) #For showing nice tables
library(dplyr) #Data wrangling
library(ggplot2) #Plots
#install.packages('wordcloud')
library(wordcloud) #For Word Mining
#install.packages('tm')
library(tm) #For Word Mining
library(xlsx)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  #### Input Data Tab ####  
  # Input datatable ####
  
  myData <- reactive({
    inFile <- input$Data_Upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  })
  
  # Mapa de localización ####
  output$Location_Map <- renderLeaflet({
    if (input$Location_Map == TRUE) {
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
    #Initial view #
      setView(lng = -102.5528, 
              lat = 23.6345,
              zoom = 5)
    }
  })
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #### Results Tab ####
  # Mapa de resultados ####
  # Data points ####
  
  FEDECOOP <- paste(sep = "<br/>",
                    "<b><a href='http://www.fedecoop.com.mx'>MetaID 342</a></b>",
                    "Lobster Stock Assesment and Catch from FEDECOOP since 1970",
                    "Private Dataset"
  )
  
  
  output$Data_Map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      #### The Actual Map####
    #Initial view #
    setView(lng = -102.5528, 
            lat = 23.6345,
            zoom = 5) %>% 
      # markers ####
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
  
  # Results Tab ####
  
  # Reading the dataset ####
  
  datasetInput <- reactive({
    data <- read.xlsx("/Users/jpalacios/Documents/Box Sync/UBC/Metadata_Mexico/English/Templates/Template_1.3.xlsx","Template")
    Template <- data.frame(data)
  })
  
  #Metadata Display ####
  output$Metadata <- renderDataTable({
    Temp <- datasetInput()
    Temp
    
  })
  
  #### Quantitative Results ####
  # Number of entries ####
  output$Number_Entries <- renderPrint({
    Number_entries <- datasetInput()
    Number_entries$MMID[length(Number_entries$MMID)]
  })
  
  
  output$Number_spp <- renderPlot({
    #### By Subject_Name####
    if(input$Plot_Option == 1){
      Spp <- datasetInput() %>% 
        group_by(Subject_Name) %>% 
        summarise(Entradas = sum(Dataset_Available))
      
      ggplot(data= Spp,
             aes(
               x=reorder(Subject_Name, -Entradas),
               y=Entradas,
               fill=Subject_Name
             )) +
        geom_bar(stat="identity")+
        coord_flip()+
        theme_classic() +
        ylab("Number of Data Enteries")+
        xlab("Subject Name")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title = element_text(size=20,
                                        face="bold")
        )
    }else{
      #### By Region####
      if(input$Plot_Option == 2){
        Spp2 <- datasetInput() %>%
          group_by(Region) %>%
          summarise(Value = sum(Dataset_Available))
        ggplot(data= Spp2,
               aes(
                 x=reorder(Region, -Value),
                 y=Value,
                 fill=Region
               )) +
          geom_bar(stat="identity")+
          theme_classic() +
          ylab("Number of Data Enteries")+
          xlab("Region")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle = 45),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title = element_text(size=20,
                                          face="bold")
          )
      }else{
        #### By Location####
        if(input$Plot_Option == 3){
          Spp3 <- datasetInput() %>%
            filter(Location != "NA") %>% 
            group_by(Location) %>%
            summarise(Value = sum(Dataset_Available))
          ggplot(data= Spp3,
                 aes(
                   x=reorder(Location, -Value),
                   y=Value,
                   fill=Location
                 )) +
            geom_bar(stat="identity")+
            theme_classic() +
            coord_flip() +
            ylab("Number of Data Enteries")+
            xlab("State")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14),
                  axis.text.y = element_text(size = 14),
                  legend.position = "none",
                  axis.title = element_text(size=20,
                                            face="bold")
            )
        }
      }
    }
  })
  
  #### Qualitative Analysis ####
  
  # Keywords_Plot ####
  
  output$Keywords_Plot <- renderPlot({
    Words <- datasetInput()
    WordsCorpus <- Corpus(VectorSource(Words$Keywords)) #Selects only Keywords
    WordsCorpus <- tm_map(WordsCorpus, PlainTextDocument) #Converts to plain text
    WordsCorpus <- tm_map(WordsCorpus, removePunctuation) #Removes punctuation
    
    Word_Remove <- c(input$Keyword_Remove1,input$Keyword_Remove2)
    
    WordsCorpus <- tm_map(WordsCorpus, removeWords,Word_Remove ) #Removes a word of user preference 
    wordcloud(WordsCorpus, #Plots the words
              max.words = 100,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
              
    
  })
  

})

