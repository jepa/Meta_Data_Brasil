#### NOTA ####
#Read ui.R for information between branches


 #### Libraries needed ####

library(shiny)
library(leaflet) #For the maps
library(DT) #For showing nice tables
library(dplyr) #Data wrangling
library(tidyr) #Data wrangling
library(ggplot2) #Plots
#install.packages('wordcloud')
library(wordcloud) #For Word Mining
#install.packages('tm')
library(tm) #For Word Mining
#library(xlsx)
library(networkD3)
library(data.table)
library(dygraphs)

#install.packages('networkD3')


#Functions #

#Commented for publication of project
#source('Fun_Dat_links.r')

#The begining #

shinyServer(function(input, output) {
  
  ####.############### ####
  #### INPUT DATA TAB ####  
  
  # Upload datatable ####
  myData <- reactive({
    inFile <- input$Data_Upload
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath,
                     header = TRUE)
    data
  })
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ####.############### ####
  # METADATA TAB ####
  
  # Reading the Template ####
  datasetInput <- reactive({
    
    data<- fread("~/Documents/Dropbox/Metadata_Mexico/English/Templates/Template_6.4.csv",
                 colClasses = c(Area = 'character',
                                Notes = 'character',
                                # Data_Uncertanty ='character',
                                Data_Time_Points = 'numeric'))
    # data.frame(data)
  })
  
  
  #Metadata Display ####
  output$Metadata <- renderDataTable({
    
    #you will need this function on the "Functions" folder (it is already sourced at the beginning of the app)
    #Fun_Dat_links
    # x <- Ref_Links(datasetInput()$Reference,
    #                datasetInput()$Subject_name)
    
    #Re order the datatable
     Final <- datasetInput() #%>% 
    #   bind_cols(x) %>% 
    #   select(-Reference) #Elminates the original Reference column (not applicable if downloaded)
    
    #Show the datatable 
    datatable(Final,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 50,
                             autoWidth = TRUE,
                             lengthMenu = c(10, 50, 100, 500, 1000),
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
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
  
  #Resultados iniciales ####
  
  
  #### Reference ####
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Metadata Reference List', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('./Reference/Reference_List.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Reference_List.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('Reference_List.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  ##### Reference Display ####
  
  getPage<-function() {
    return(includeHTML("./Reference/Reference_List.html"))
  }
  output$Reference<-renderUI({getPage()
    })
  
  
  ##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  ####.############### ####
  #### PRELIMINARY RESULTS TAB ####
  # Mapa de resultados ####
  
  # Data points shown in the map ###
  #Fedecop information 
  
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
      # Markers for those that we have ####
    addMarkers(
      lng = ~Long,
      lat = ~Lat,
      popup = ~as.character(MMID),
      clusterOptions = markerClusterOptions()
    )
    
  })
  
  #Creating TS graph ####
  
  TFdatasetInput <- reactive({
    
    x <- fread("Data_Curve_I.csv")
    x <- x %>%
      select(-1)
    
  })
  
  output$TFgraph <- renderDygraph({
    x <- TFdatasetInput() %>% 
      select(-Real_Total,
             -Temp)
    
    Dt_Points <- ts(x,
                    start=c(2016,11),
                    end = c(2017,8), # <- this has to be changed everytime we add a month
                    frequency= 12)
    
    dygraph(Dt_Points) %>% #Creats the graph
      dyOptions(stackedGraph = TRUE, #Makes it stacked
                drawPoints = TRUE, #Shows each data point
                pointSize = 4) %>%
      dyRangeSelector(height = 20) %>%
      dyAxis("x", drawGrid = FALSE) %>% #Removes the grid
      dyAxis("y", drawGrid = FALSE) %>%
      dyAxis("y", label = "Number of Records") %>%  #Labels
      dyLegend(width = 600) %>% 
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 0.2,
                  hideOnMouseOut = FALSE)
    
  })
  
  ####.############### ####
  #### Quantitative Results ####
  # Number of entries ####
  
  #### Quantitative Analysis ####
  
  output$Number_Entries <- renderText({
    Number_entries <- datasetInput() %>% 
      filter(MMID != "na")
    paste(Number_entries$MMID[length(Number_entries$MMID)])
    
  })
  
  #_____________________ END ___________________________ #  
  
  # Number of Data Points ####
  output$Number_Data_Points <- renderText({
    Number_entries <- datasetInput()
    paste(sum(Number_entries$Data_Time_Points,na.rm=T))
    
  })
  
  #_____________________ END ___________________________ #  
  
  # Number of Repositories ####
  output$Sources <- renderText({
    z<- datasetInput() %>% 
      group_by(Compilation_Title) %>% 
      summarise(sum(Data_Time_Points)) %>% 
      select(-2) %>% 
      filter(!is.na(Compilation_Title)) %>% 
      mutate(z = 1)
    
    paste(sum(z$z))
    
  })
  
  #_____________________ END ___________________________ #  
  
  #### NUMBER OF DATA POINTS ####
  
  output$Number_spp <- renderPlot({
    #### Entries By Area####
    
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
        ylab("Número de Datos")+
        xlab("Campo de Investigación")+
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
          ylab("Número de Datos")+
          xlab("Región")+
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
            filter(Location != "Multiple States") %>% 
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
            ylab("Número de Datos")+
            xlab("Localidad")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle=45),
                  axis.text.y = element_text(size = 14),
                  legend.position = "none",
                  axis.title = element_text(size=20,
                                            face="bold")
            )
        }
      }
    }
  })
  
  #### NUMBER OF Records ####
  
  output$Records <- renderPlot({
    #### Entries By Area####
    
    if(input$Plot_Option_B == 1){
      Spp <- datasetInput() %>% 
        group_by(Area) %>% 
        summarise(Entradas = n()) %>% 
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
        ylab("Registros")+
        xlab("Campo de Investigación")+
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
          summarise(Value = n()) %>% 
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
          ylab("Registros")+
          xlab("Región")+
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
            filter(Location != "Multiple States") %>% 
            group_by(Location) %>%
            summarise(Value = n()) %>% 
            arrange(desc(Value))
          
          Head_Spp3 <- head(Spp3,input$Num_Data_Range_R) %>% 
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
            ylab("Registros")+
            xlab("Localidad")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle=45),
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
      ylab("Número de Datos")+
      xlab("Componente Socio Económico")+
      theme(axis.text.x = element_text(hjust = 1,
                                       size=14,
                                       angle= 45),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title = element_text(size=20,
                                      face="bold"))
  })
  
  #### SE_Component Registros ####
  output$SE_Component_Reg <- renderPlot({
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(SE_Interaction)) %>% 
      filter(SE_Interaction != "Otros") %>% 
      group_by(SE_Interaction) %>% 
      summarise(Value = n())
    
    ggplot(data=Se_Plot,
           aes(
             y = Value,
             x = SE_Interaction,
             fill =SE_Interaction
           ))+
      geom_bar(stat = "identity")+
      theme_classic() +
      ylab("Número de Registros")+
      xlab("Componente Socio Económico")+
      theme(axis.text.x = element_text(hjust = 1,
                                       size=14,
                                       angle= 45),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title = element_text(size=20,
                                      face="bold"))
  })
  
  ####.############### ####
  #### Qualitative Analysis ####
  
  # Keywords_Plot ####
  
  output$Keywords_Plot <- renderPlot({
    
    if(input$Discipline == "NOT"){
      stop()
    }
    
    if(input$Discipline == "Todas"){
      Words <- datasetInput()
    }else{
      Words <- datasetInput() %>% 
        filter(Research_Field == input$Discipline)
    }
    WordsCorpus <- Corpus(VectorSource(Words$Keywords)) #Selects only Keywords
    # WordsCorpus <- tm_map(WordsCorpus,
    #                       PlainTextDocument) #Converts to plain text
    # WordsCorpus <- tm_map(WordsCorpus,
    #                       removePunctuation) #Removes punctuation
    # 
    # Word_Remove <- c(input$Keyword_Remove1, #<- For optional word removing
    #                  input$Keyword_Remove2)
    
    #Removes a word of user preference
    # WordsCorpus <- tm_map(WordsCorpus,
    #                       removeWords,
    #                       Word_Remove )
    # 
    
    wordcloud(WordsCorpus, #Plots the words
              max.words = 100,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
    
    #}
  })
  
  #### Subject_Name Word Cloud ####
  
  output$Subjects_Plot <- renderPlot({
    
    if(input$Discipline_Subject == "NOT"){
      stop()
    }
    
    if(input$Discipline == "Todas"){
      Words <- datasetInput()
    }else{
      Words <- datasetInput() %>% 
        filter(Research_Field == input$Discipline)
    }
    WordsCorpus <- Corpus(VectorSource(Words$Subject_name)) #Selects only Keywords
    # WordsCorpus <- tm_map(WordsCorpus,
    #                       PlainTextDocument) #Converts to plain text
    # WordsCorpus <- tm_map(WordsCorpus,
    #                       removePunctuation) #Removes punctuation
    # 
    # Word_Remove <- c(input$Keyword_Remove1, #<- For optional word removing
    #                  input$Keyword_Remove2)
    
    #Removes a word of user preference
    # WordsCorpus <- tm_map(WordsCorpus,
    #                       removeWords,
    #                       Word_Remove )
    # 
    
    wordcloud(WordsCorpus, #Plots the words
              max.words = 100,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
    
    #}
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
        ylab("Número Registros")+
        xlab("Area")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle= 45),
              axis.text.y = element_text(size = 14),
              legend.position = "top",
              axis.title = element_text(size=20,
                                        face="bold"))+ 
        guides(fill = guide_legend(title = "Componente Socio-Económico",
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
            ylab("Número de Registros")+
          xlab("Región")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle= 45),
                axis.text.y = element_text(size = 14),
                legend.position = "top",
                axis.title = element_text(size=20,
                                          face="bold"))+ 
          guides(fill = guide_legend(title = "Componente Socio-Económico",
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
            ylab("Número de Registros")+
            xlab("Localidad")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle= 45),
                  axis.text.y = element_text(size = 14),
                  legend.position = "top",
                  axis.title = element_text(size=20,
                                            face="bold"))+ 
            guides(fill = guide_legend(title = "Componente Socio-Económico",
                                       title.position = "left"))
          
        }
      }
    }
  })
  
  #### Research_Field_DP ####
  output$RF_Plot <- renderPlot({
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(Research_Field)) %>% 
      #filter(Research_Field != "Otros") %>% 
      group_by(Research_Field) %>% 
      summarise(Value = sum(Data_Time_Points,na.rm=T))
    
    ggplot(data=Se_Plot,
           aes(
             y = Value,
             x = Research_Field,
             fill =Research_Field
           ))+
      geom_bar(stat = "identity")+
      theme_classic() +
      ylab("Número de Datos")+
      xlab("Campos de Investigación")+
      theme(axis.text.x = element_text(hjust = 1,
                                       size=14,
                                       angle= 45),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title = element_text(size=20,
                                      face="bold"))
  })
  
  output$RF_Plot_Rec <- renderPlot({
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(Research_Field)) %>% 
      #filter(Research_Field != "Otros") %>% 
      group_by(Research_Field) %>% 
      summarise(Value = n())
    
    ggplot(data=Se_Plot,
           aes(
             y = Value,
             x = Research_Field,
             fill =Research_Field
           ))+
      geom_bar(stat = "identity")+
      theme_classic() +
      ylab("Número de Registros")+
      xlab("Campos de Investigación")+
      theme(axis.text.x = element_text(hjust = 1,
                                       size=14,
                                       angle= 45),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title = element_text(size=20,
                                      face="bold"))
  })
  
  ## Research Field Plot DP ####
  output$Research_Field_Plot <- renderPlot({
    
    #### By Area ####
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(Area)) %>% 
      group_by(Area,Research_Field) %>% 
      summarise(Entradas = sum(Data_Time_Points,na.rm=T))
    
    if(input$Research_Field_Plot_Option == 1){
      ggplot(data=Se_Plot,
             aes(
               x=Area,
               y=Entradas,
               fill= Research_Field
             ))+
        geom_bar(stat = "identity")+
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
        guides(fill = guide_legend(title = "Research Field",
                                   title.position = "left"))
    }else{
      #### By Region ####
      
      if(input$Research_Field_Plot_Option == 2){
        Se_Plot <- datasetInput() %>% 
          filter(!is.na(Region)) %>% 
          group_by(Region,Research_Field) %>% 
          summarise(Entradas = sum(Data_Time_Points,na.rm=T))
        
        ggplot(data=Se_Plot,
               aes(
                 x=Region,
                 y= Entradas,
                 fill= Research_Field
               ))+
          geom_bar(stat = "identity")+
          theme_classic() +
          ylab("Number of Data Points")+
          xlab("Region")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle= 45),
                axis.text.y = element_text(size = 14),
                legend.position = "right",
                axis.title = element_text(size=20,
                                          face="bold"))+ 
          guides(fill = guide_legend(title = "Research Field",
                                     title.position = "left"))
      }else{
        #### By Location ####
        
        if(input$Research_Field_Plot_Option == 3){
          
          Se_Plot <- datasetInput() %>% 
            filter(!is.na(Location)) %>% 
            group_by(Location,Research_Field) %>% 
            summarise(Entradas = sum(Data_Time_Points,na.rm=T))
          
          ggplot(data=Se_Plot,
                 aes(
                   x=Location,
                   y = Entradas,
                   fill= Research_Field
                 ))+
            geom_bar(stat = "identity")+
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
            guides(fill = guide_legend(title = "Research Field",
                                       title.position = "left"))
          
        }
      }
    }
  })
  
  output$Research_Field_Plot_Rec <- renderPlot({
    
    #### By Area ####
    Se_Plot <- datasetInput() %>% 
      filter(!is.na(Area)) %>% 
      group_by(Area,Research_Field) %>% 
      summarise(Entradas = n())
    
    if(input$Research_Field_Plot_Option == 1){
      ggplot(data=Se_Plot,
             aes(
               x=Area,
               y=Entradas,
               fill= Research_Field
             ))+
        geom_bar(stat = "identity")+
        theme_classic() +
        ylab("Numero de Registros")+
        xlab("Area")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle= 45),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title = element_text(size=20,
                                        face="bold"))+ 
        guides(fill = guide_legend(title = "Research Field",
                                   title.position = "left"))
    }else{
      #### By Region ####
      
      if(input$Research_Field_Plot_Option == 2){
        Se_Plot <- datasetInput() %>% 
          filter(!is.na(Region)) %>% 
          group_by(Region,Research_Field) %>% 
          summarise(Entradas = n())
        
        ggplot(data=Se_Plot,
               aes(
                 x=Region,
                 y= Entradas,
                 fill= Research_Field
               ))+
          geom_bar(stat = "identity")+
          theme_classic() +
          ylab("Numero de Registros")+
          xlab("Region")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle= 45),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title = element_text(size=20,
                                          face="bold"))+ 
          guides(fill = guide_legend(title = "Research Field",
                                     title.position = "left"))
      }else{
        #### By Location ####
        
        if(input$Research_Field_Plot_Option == 3){
          
          Se_Plot <- datasetInput() %>% 
            filter(!is.na(Location)) %>% 
            group_by(Location,Research_Field) %>% 
            summarise(Entradas = n())
          
          ggplot(data=Se_Plot,
                 aes(
                   x=Location,
                   y = Entradas,
                   fill= Research_Field
                 ))+
            geom_bar(stat = "identity")+
            theme_classic() +
            ylab("Numero de Registros")+
            xlab("Location")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle= 45),
                  axis.text.y = element_text(size = 14),
                  legend.position = "none",
                  axis.title = element_text(size=20,
                                            face="bold"))+ 
            guides(fill = guide_legend(title = "Research Field",
                                       title.position = "left"))
          
        }
      }
    }
  })
  
  #### Netword D3 ####
  
output$Network <- renderSankeyNetwork({
  
  Category <- data.table::data.table(Name=c(
    "AAcademic",
    "Aquaculture",
    "Goverment",
    "Inter. Gov (IGO)",
    "International",
    "NGO",
    "Unknown",
    "Conservation",
    "Ecology",
    "Fisheries",
    "Oceanography",
    "Sociology",
    "Turism",
    "Other",
    "NGO Funding",
    "Private Funding",
    "Goverment Fu",
    "ACA_F",
    "Inter. Gov (IGO) Funding",
    "International Funding"
  )
  ) %>% 
    arrange(Name)
  
  # The rest of the needed information
  Template <- datasetInput()
  
  #First Research Funding
  R_Fund_Org <-Template %>%
    filter(!is.na(Research_Fund)) %>%
    filter(Research_Fund != "NA") %>%
    group_by(Research_Fund,Institution_Type) %>%
    summarise(Value =n()) %>%
    rename(Source = Research_Fund,
           Target = Institution_Type)
  
  Inst_Field <-Template %>%
    filter(!is.na(Institution_Type)) %>%
    filter(Institution_Type != "NA") %>%
    group_by(Institution_Type,Research_Field) %>%
    summarise(Value = n()) %>%
    rename(Source = Institution_Type,
           Target = Research_Field)
  
  Final_Table <- R_Fund_Org %>%
    bind_rows(Inst_Field) 
  
  Final_Table <- data.frame(ID = seq(1:nrow(Final_Table))) %>% 
    bind_cols(Final_Table)
  
  Final_Table_N <- Final_Table %>% 
    gather("Category","Character",2:3) %>% 
    arrange(Character)
  
  Final_Table_N$Character <- as.integer(as.factor(Final_Table_N$Character))
  Final_Table_N$Character <- as.numeric(as.integer(Final_Table_N$Character)-1)
  
  Source <- Final_Table_N %>% 
    filter(Category == "Source") %>% 
    select(-Category) %>% 
    rename(Source = Character)
  
  Target <- Final_Table_N %>% 
    filter(Category == "Target") %>% 
    select(-Category,-Value) %>% 
    rename(Target = Character) %>% 
    left_join(Source,
              by ="ID")
  
  sankeyNetwork(Links = Target, #Dataset with Source, Target and value
                Nodes = Category, #Dataset withe the Names
                Source = "Source", #Source column in Links dataset
                Target = "Target", #Target column in Links dataset
                Value = "Value", # The amount to plot from the Links dataset
                NodeID = "Name", #What's showing when mouse over Node
                units = "Records", #Units to show
                fontSize = 12,
                nodeWidth = 30)
  
  
})
})

