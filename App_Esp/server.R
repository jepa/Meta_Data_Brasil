#### NOTA ####
##############################
##############################
##############################
##############################

##### MUY IMPORTANTE ####

#Para modificar la información de la página informativa TIENES que Data_Curver en la "branch" Internet, de github. si no, vas a modificar la versión pensada para cuándo los datos estén listos.

#Read ui.R for information between branches
##############################
##############################
##############################
##############################



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
library(dygraphs)
library(data.table)


#Functions #

#Commented for publication of project
#source('Fun_Dat_links.r')

#The begining #

shinyServer(function(input, output, session) {
  
  
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
  
  # HOME PAGE ####
  #Number of points
  
  output$Datapoints_Intro <- renderText({
    Number_entries <- datasetInput()
    paste(sum(Number_entries$Data_Time_Points,na.rm=T))
    
  })
  
  observeEvent(input$Collaborate_But, {
    updateNavbarPage(session,
                     inputId = "MMM_Nav_Bar", 
                     selected = "Collaborate")
  })
  
  
  # METADATA TAB ####
  
  # Reading the Template ####
  datasetInput <- reactive({
    
    data<- fread("./Template.csv")
    #data.frame(data)
  })
  
  # Reading the Metadata_Key ####
  Key_datasetInput <- reactive({
    
    K_data<- read.csv("./Metadata_Key.csv", 
                      header = TRUE,
                      na="NA")
    data.frame(K_data)
  })
  
  #Metadata Display ####
  output$Metadata <- renderDataTable({
    Final <- datasetInput()
    #Show the datatable 
    datatable(Final,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 28,
                             autoWidth = TRUE,
                             lengthMenu = c(10, 20,30),
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              )
    )
  })
  
  #Metadata Summary Display ####
  source('Fun_Dat_links.R')
  output$Metadata_Summary <- renderDataTable({
    
    x <- Ref_Links(datasetInput()$Reference,
                   datasetInput()$Subject_name)
    
    #Re order the datatable
    Final <- datasetInput() %>% 
      bind_cols(x) %>% 
      select(MMID,
             Short_Title,
             Author,
             Link)
    
    #Show the datatable 
    datatable(Final,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 25,
                             autoWidth = TRUE,
                             lengthMenu = c(50, 100, 200,500),
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              )
    )
  })
  
  # Metadata_Key Display ####
  
  output$Metadata_Key <- renderDataTable({
    Metadata_Key <- Key_datasetInput() 
    
    datatable(Metadata_Key,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 50,
                             autoWidth = TRUE,
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              )
    )
    
  })
  
  
  #### PRELIMINARY RESULTS TAB ####
  
  
  #### Timeseries of data gathering ####
  
  #Import dataframe (for now)
  TFdatasetInput <- reactive({
    
    x <- read.csv("Data_Curve.csv")
    x <- x %>% 
      select(-1)
    
  })
  
  #Creating the graph
  output$TFgraph <- renderDygraph({
    x <- TFdatasetInput()
    Dt_Points <- ts(x,
                    start=c(2016,11),
                    end = c(2017,1), 
                    frequency= 12)
    
    dygraph(Dt_Points) %>% #Creats the graph
      dyOptions(stackedGraph = TRUE, #Makes it stacked
                drawPoints = TRUE, #Shows each data point
                pointSize = 4) %>% 
      dyRangeSelector(height = 20) %>% 
      dyAxis("x", drawGrid = FALSE) %>% #Removes the grid
      dyAxis("y", drawGrid = FALSE) %>% 
      dyAxis("y", label = "Información colectada (n Datos)") %>%  #Labels
      dyLegend(width = 600)
  })
  
  source('ts_fun.R')
  output$TSgraph <- renderDygraph({
    ts_plot(datasetInput(),1900,2050)
  })
  
  #### Quantitative Results ####
  # Number of entries ####
  
  output$date <- renderText({
    paste(Sys.Date())
    
  })
  
  output$Number_Entries <- renderText({
    Number_entries <- datasetInput() %>% 
      filter(MMID != "na")
    paste(Number_entries$MMID[length(Number_entries$MMID)])
    
  })
  
  output$Number_Data_Points <- renderText({
    Number_entries <- datasetInput()
    paste(sum(Number_entries$Data_Time_Points,na.rm=T))
    
  })
  
  output$Sources <- renderText({
    z<- datasetInput() %>% 
      group_by(Compilation_Title) %>% 
      summarise(sum(Data_Time_Points)) %>% 
      select(-2) %>% 
      filter(!is.na(Compilation_Title)) %>% 
      mutate(z = 1)
    
    paste(sum(z$z))
    
  })
  
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
        ylab("Datos")+
        xlab("Campo de Investigación")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle=45),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title = element_text(size=14,
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
          ylab("Datos")+
          xlab("Región")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle = 45),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title = element_text(size=14,
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
            ylab("Datos")+
            xlab("Localidad")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle=45),
                  axis.text.y = element_text(size = 14),
                  legend.position = "none",
                  axis.title = element_text(size=14,
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
    
    #Removes a word of user preference 
    WordsCorpus <- tm_map(WordsCorpus, removeWords,Word_Remove ) 
    #
    
    wordcloud(WordsCorpus, #Plots the words
              max.words = 100,
              random.order = FALSE,
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  
  #### Collaboration ###
  output$Institutions <- renderDataTable({
    
    Inst_Table <- datasetInput() %>% 
      group_by(Institution,
               Compilation_Title) %>% 
      summarise(x = n()) %>% 
      filter(Institution != "na",
             Institution != "Varios") %>% 
      select(Institution,
             Compilation_Title)
    colnames(Inst_Table) <- c("Institution","Repository")
    
    
    datatable(Inst_Table,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 5,
                             autoWidth = TRUE,
                             lengthMenu = c(10, 15, 20, 50)
              )
    )
  })
  
  output$People <- renderDataTable({
    
    #Create a Table with the authors
    P_Table <- datasetInput() %>% 
      group_by(Author,
               Compilation_Title) %>% 
      summarise(x = n()) %>% 
      filter(Author != "na",
             Author != "Varios") %>% 
      select(Author,
             Compilation_Title) 
    
    #Create a Table with the Institutions to remove duplications
    I_Table <- datasetInput() %>% 
      group_by(Institution,
               Compilation_Title) %>% 
      summarise(x = n()) %>% 
      filter(Institution != "na",
             Institution != "Varios") %>% 
      select(Institution,
             Compilation_Title)
    
    #Set names equal for "ati_join" function
    colnames(P_Table) <- c("Institution","Repository")
    
    #Select those that are different from each other
    F_Table <- anti_join(P_Table,I_Table,
                         by ="Institution")
    colnames(F_Table) <- c("Author","Repository")
    
    #Final datatable
    datatable(F_Table,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 5,
                             autoWidth = TRUE,
                             lengthMenu = c(10, 15, 20, 50)
              )
    )
  })  
})

