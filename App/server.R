
####Libraries needed ####

library(shiny)
library(leaflet) #For the maps
library(DT) #For showing nice tables
library(dplyr) #Data wrangling
library(ggplot2) #Plots
#install.packages('wordcloud')
library(wordcloud) #For Word Mining
#install.packages('tm')
library(tm) #For Word Mining
#library(xlsx)
library(networkD3)


#Functions #

source('./Functions/Fun_Dat_links.r')

#The begining #

shinyServer(function(input, output) {
  
  
  #### INPUT DATA TAB ####  
  
  # Upload datatable ####
  myData <- reactive({
    inFile <- input$Data_Upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath,
                     header = TRUE)
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
                zoom = 5) %>% 
        #Adding Data localization ####
      addMarkers(lng = input$Map_Long,
                 lat= input$Map_Lat,
                 popup=input$Short_Title)
    }
  })
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # METADATA TAB ####
  
  # Reading the Template ####
  datasetInput <- reactive({
    
    # PATH FOR HALL 2000 
    #library(xlsx)
    #data <- read.xlsx("/Users/jpalacios/Documents/Box Sync/UBC/Metadata_Mexico/English/Templates/Template_1.5.xlsx","Template")
    
    #PATH FOR CARMELIA #
    data<- read.csv("./Template.csv", 
                    header = TRUE,
                    na="NA")
    data.frame(data)
  })
  
  
  #Metadata Display ####
  output$Metadata <- renderDataTable({
    
    #you will need this function on the "Functions" folder (it is already sourced at the beginning of the app)
    #Fun_Dat_links
    x <- Ref_Links(datasetInput()$Reference,
                   datasetInput()$Subject_name)
    
    #Re order the datatable
    Final <- datasetInput() %>% 
      bind_cols(x) %>% 
      select(-Reference) #Elminates the original Reference column (not applicable if downloaded)
    
    #Show the datatable 
    datatable(Final,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 50,
                             autoWidth = TRUE,
                             lengthMenu = c(10, 50, 100, 500, 1000)
                             )
    )
  })
  
  # Data_Available Display ####
  
  output$Available_Data <- renderDataTable({
    Data_Available <- datasetInput() %>% 
      filter(Available_Metadata == "YES" )
    
    datatable(Data_Available,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 50,
                             autoWidth = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'))
    )
    
  })
  
  # Download Data ####
  #Specific Data #
  #First we create a path where the data is saved
  MMID_Data <- reactive({
    Dfile = paste("./Data_Download/MMID_",
                  input$MMID_Download_Selection, #THis includes the number
                  '.csv',
                  sep='')
    read.csv(Dfile, header = TRUE)
    
  })
  
  #This is just for the download button and the name of the file...
  output$Data_Download <- downloadHandler(
    filename = function() {  
      paste("MMID_",
            input$MMID_Download_Selection, 
            '.csv',
            sep='') 
    },
    content = function(file) {
      write.csv(MMID_Data(), file)
    }
    
  )
  
  # All Meta-dataset #
  output$MMID_Download <- downloadHandler(
    filename = function() { 
      paste(input$MMID_Download, 
            '.csv', 
            sep='') 
    },
    content = function(file) {
      write.csv(datasetInput(), file)
    }
  )
  
  #### Reference ####
  
  # output$Ref_Download <- downloadHandler(
  #   filename = "Reporte Anchoveta.pdf",
  #   
  #   content = function(file) {
  #     src <- normalizePath('Reference_List.Rmd')
  #     
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     # owd <- setwd(tempdir())
  #     # on.exit(setwd(owd))
  #     file.copy(src, 'Reference_List.Rmd')
  #     
  #     
  #     out <- render('Reference_List.Rmd', pdf_document())
  #     file.rename(out, file)
  #   }
  # )
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  #### PRELIMINARY RESULTS TAB ####
  # Mapa de resultados ####
  
  # Data points shown in the map ###
  #Fedecop information 
  FEDECOOP <- paste(sep = "<br/>",
                    "<b><a href='http://www.fedecoop.com.mx'>MetaID 342</a></b>",
                    "Lobster Stock Assesment and Catch from FEDECOOP since 1970",
                    "Private Dataset"
  )
  
  #### The Actual Map ####
  #(requieres leaflet package)
  output$Data_Map <- renderLeaflet({
    data = datasetInput() %>% 
      filter(!is.na(Lat))
    leaflet(data=data) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      #Initial view #
      setView(lng = -102.5528, 
              lat = 23.6345,
              zoom = 5) %>% 
      # Data examples as markers ####
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
      # Data examples as rectangles ####
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
      ) %>% 
      #### Lat & Long in dataset ####
    addMarkers(
      lng = ~Long,
      lat = ~Lat,
      popup = ~as.character(MMID),
      clusterOptions = markerClusterOptions()
    )
    
  })
  
  #### Quantitative Results ####
  # Number of entries ####
  output$Number_Entries <- renderPrint({
    Number_entries <- datasetInput() %>% 
      filter(MMID != "na")
    Number_entries$MMID[length(Number_entries$MMID)]
  })
  
  
  output$Number_spp <- renderPlot({
    #### By Area####
    
    if(input$Plot_Option == 1){
      Spp <- datasetInput() %>% 
        group_by(Area) %>% 
        summarise(Entradas = sum(Data_Time_Points,na.rm=T)) %>% 
        filter(Area !="na") # %>% 
      # filter(Entradas >= input$Num_Data_Range[1]) %>% 
      # filter(Entradas <= input$Num_Data_Range[2])
      
      ggplot(data= Spp,
             aes(
               x=reorder(Area, -Entradas),
               y=Entradas,
               fill=Area
             )) +
        geom_bar(stat="identity")+
        #coord_flip()+
        theme_classic() +
        ylab("Number of Data Points")+
        xlab("Research Field")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle=45),
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
          summarise(Value = sum(Data_Time_Points,na.rm=T)) %>% 
          filter(Region != "na") %>% 
          filter(Region != "")
        
        ggplot(data= Spp2,
               aes(
                 x=reorder(Region, -Value),
                 y=Value,
                 fill=Region
               )) +
          geom_bar(stat="identity")+
          theme_classic() +
          ylab("Number of Data Points")+
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
            filter(Location != "na") %>% 
            group_by(Location) %>%
            summarise(Value = sum(Data_Time_Points,na.rm=T)) %>% 
            arrange(desc(Value))
          
          Head_Spp3 <- head(Spp3,input$Num_Data_Range) %>% 
            arrange(desc(Value))
          
          
          ggplot(data= Head_Spp3,
                 aes(
                   x=reorder(Location, -Value),
                   y=Value,
                   fill=Location
                 )) +
            geom_bar(stat="identity")+
            theme_classic() +
            coord_flip() +
            ylab("Number of Data Points")+
            xlab("Location Name")+
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
  
  #### SE_Component ####
  output$SE_Component <- renderPlot({
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(SE_Interaction)) %>% 
      filter(SE_Interaction != "Otros") %>% 
      group_by(SE_Interaction) %>% 
      summarise(Value = sum(Data_Time_Points,na.rm=T))
    
    ggplot(data=Se_Plot,
           aes(
             y = Value,
             x = SE_Interaction,
             fill =SE_Interaction
           ))+
      geom_bar(stat = "identity")+
      theme_classic() +
      ylab("Number of Data Points")+
      xlab("Social Economic Component")+
      theme(axis.text.x = element_text(hjust = 1,
                                       size=14,
                                       angle= 45),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title = element_text(size=20,
                                      face="bold"))
  })
  
  
  #### Qualitative Analysis ####
  
  # Keywords_Plot ####
  
  output$Keywords_Plot <- renderPlot({
    Words <- datasetInput()
    WordsCorpus <- Corpus(VectorSource(Words$Keywords)) #Selects only Keywords
    WordsCorpus <- tm_map(WordsCorpus, PlainTextDocument) #Converts to plain text
    WordsCorpus <- tm_map(WordsCorpus, removePunctuation) #Removes punctuation
    
    Word_Remove <- c(input$Keyword_Remove1,input$Keyword_Remove2)
    
    #Removes a word of user preference 
    WordsCorpus <- tm_map(WordsCorpus, removeWords,Word_Remove ) 
    #
    
    wordcloud(WordsCorpus, #Plots the words
              max.words = 100,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  #### Subject_Name Word Cloud ####
  
  output$Subject_name_Plot <- renderPlot({
    Words <- datasetInput()
    WordsCorpus <- Corpus(VectorSource(Words$Subject_name)) #Selects only Subject_name
    WordsCorpus <- tm_map(WordsCorpus, PlainTextDocument) #Converts to plain text
    WordsCorpus <- tm_map(WordsCorpus, removePunctuation) #Removes punctuation
    
    Word_Remove <- c(input$Subject_Remove,input$Subject_Remove2)
    
    #Removes a word of user preference 
    WordsCorpus <- tm_map(WordsCorpus, removeWords,Word_Remove ) 
    #
    
    wordcloud(WordsCorpus, #Plots the words
              max.words = 100,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  #### Experimental Analysis ####  
  
  output$SE_Component_Area <- renderPlot({
    
    #### By Area ####
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(Area))
    
    if(input$SE_E_Plot_Option == 1){
      ggplot(data=Se_Plot,
             aes(
               x=Area,
               fill= SE_Interaction
             ))+
        geom_bar()+
        theme_classic() +
        ylab("Number of Data Points")+
        xlab("Area")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle= 45),
              axis.text.y = element_text(size = 14),
              legend.position = "top",
              axis.title = element_text(size=20,
                                        face="bold"))+ 
        guides(fill = guide_legend(title = "Social Economic Component",
                                   title.position = "left"))
    }else{
      #### By Region ####
      
      if(input$SE_E_Plot_Option == 2){
        Se_Plot <- datasetInput() %>% 
          filter(!is.na(Region))
        
          ggplot(data=Se_Plot,
               aes(
                 x=Region,
                 fill= SE_Interaction
               ))+
          geom_bar()+
          theme_classic() +
          ylab("Number of Data Points")+
          xlab("Region")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle= 45),
                axis.text.y = element_text(size = 14),
                legend.position = "top",
                axis.title = element_text(size=20,
                                          face="bold"))+ 
          guides(fill = guide_legend(title = "Social Economic Component",
                                     title.position = "left"))
      }else{
        #### By Location ####
        
        if(input$SE_E_Plot_Option == 3){
          
          Se_Plot <- datasetInput() %>% 
            filter(!is.na(Location))
          
          ggplot(data=Se_Plot,
                 aes(
                   x=Location,
                   fill= SE_Interaction
                 ))+
            geom_bar()+
            theme_classic() +
            ylab("Number of \nData Points")+
            xlab("Location")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle= 45),
                  axis.text.y = element_text(size = 14),
                  legend.position = "top",
                  axis.title = element_text(size=20,
                                            face="bold"))+ 
            guides(fill = guide_legend(title = "Social Economic Component",
                                       title.position = "left"))
          
        }
      }
    }
  })
})

