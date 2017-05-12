#### Server for Metadata APP ####

# This script is part of the Metadata of Marine Research in Mexico app.
#Started on October, 2016
#Juliano Palacios Abrantes, j.palacios@oceans.ubc.ca
#________________________________________________________________________#

#### NOTES ####
# everything you do here, you have to miror in the "App_Esp". If the spanish webpage is stil on-line.

#_______________________________ END NOTES __________________________________#

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

#________________________________________________________________________#

#Functions #

# This function is needed to create the dataset with the quick links
source('Fun_Dat_links.R')
source('ts_fun.R')

##### shinyServer #####

shinyServer(function(input, output, session) {
  
  
##### Template #####
  datasetInput <- reactive({
    
    data<- fread("./Template.csv",
                 colClasses = c(Location = 'character',
                                Notes = 'character')
                 )
    
                 
    data.frame(data)
  })
  
  
#_______________ END ___________________________ #
  
#### HOME PAGE ####
  
### Floating Panel ####
  
  # Number of datapoints 
  output$Datapoints_Intro <- renderText({
    Number_entries <- datasetInput()
    paste(sum(Number_entries$Data_Time_Points,na.rm=T))
    
  })
  
  # Date updated
  output$date <- renderText({
    paste(Sys.Date())
    
  })
  
  #_______________ END ___________________________ #
  
#### Collaborate buttons ####
  
  observeEvent(input$Collaborate_But, { #<- observeEvent allows you to navegate within the app. NOTE: you have to give the destination a name
    updateNavbarPage(session,
                      inputId = "MMM_Nav_Bar",
                      selected = "Collaborate") # <- destination name
  })
  
#_______________ HOME PAGE END ___________________________ #
  
#### METADATA TAB ####
  
  # Reading the Metadata_Key ####
  Key_datasetInput <- reactive({
    
    K_data<- read.csv("./Metadata_Key.csv", 
                    header = TRUE,
                    na="NA")
    data.frame(K_data)
  })
  
  #_____________________ END ___________________________ #
  
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
                             lengthMenu = c(10, 20,30)
                             )
    )
  })
  
#_____________________ END ___________________________ #
  
#Metadata Summary Display ####
  # This section uses the function: 'Fun_Dat_links.R' loaded at the begining
  
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
                             lengthMenu = c(50, 100, 200,500)
              )
    )
  })
  
#_____________________ END ___________________________ #

    # Metadata_Key Display ####
  
  output$Metadata_Key <- renderDataTable({
    Metadata_Key <- Key_datasetInput() 
    
    datatable(Metadata_Key,
              rownames = FALSE,
              filter = 'top',
              escape = FALSE,
              options = list(pageLength = 50,
                             autoWidth = TRUE
                             )
              )
    
  })
  
#_____________________ END ___________________________ #  
  
#_____________________ METADATA TAB END ___________________________ #  

#### PRELIMINARY RESULTS ####
  
  #### Timeseries of data gathering ####
  
# Import dataframe (for now)
TFdatasetInput <- reactive({

x <- fread("Data_Curve.csv")
x <- x %>%
  select(-1)

})
  
  #Creating TS graph
  
  output$TFgraph <- renderDygraph({
  x <- TFdatasetInput()

  Dt_Points <- ts(x,
                  start=c(2016,11),
                  end = c(2017,4), # <- this has to be changed everytime we add a month
                  frequency= 12)

  dygraph(Dt_Points) %>% #Creats the graph
    dyOptions(stackedGraph = TRUE, #Makes it stacked
              drawPoints = TRUE, #Shows each data point
              pointSize = 4) %>%
    dyRangeSelector(height = 20) %>%
    dyAxis("x", drawGrid = FALSE) %>% #Removes the grid
    dyAxis("y", drawGrid = FALSE) %>%
    dyAxis("y", label = "Number of Data Points") %>%  #Labels
    dyLegend(width = 600)
    
  })
  
  #_____________________ END ___________________________ #  
  
  
  #### Qualitative Analysis ####
  
  # Keywords_Plot ####
  
  output$Keywords_Plot <- renderPlot({
    input$Words_But
    
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
    WordsCorpus <- tm_map(WordsCorpus,
                          PlainTextDocument) #Converts to plain text
    WordsCorpus <- tm_map(WordsCorpus,
                          removePunctuation) #Removes punctuation
    
    Word_Remove <- c(input$Keyword_Remove1, #<- For optional word removing
                     input$Keyword_Remove2)
    
    #Removes a word of user preference
    WordsCorpus <- tm_map(WordsCorpus,
                          removeWords,
                          Word_Remove )
    
      
      wordcloud(WordsCorpus, #Plots the words
                max.words = 100,
                random.order = FALSE,
                colors=brewer.pal(8, "Dark2"))
      
    #}
  })
  
  #_____________________ END ___________________________ #    
  
  
  #### Spatial Plot ####
  
  #### By Area ####
  output$Number_spp <- renderPlot({
    
    if(input$Plot_Option == 1){
      Spp <- datasetInput() %>% 
        group_by(Area) %>% 
        summarise(Entradas = sum(Data_Time_Points,na.rm=T)) %>% 
        filter(Area !="na") %>% 
        filter(Area != "TBD") 
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
        ylab("Data Points")+
        xlab("Research Field")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle=45),
              axis.text.y = element_text(size = 14),
              legend.position = "none",
              axis.title = element_text(size=14,
                                        face="bold")
              
        )
      
    }else{
      #### By Region ####
      if(input$Plot_Option == 2){
        Spp2 <- datasetInput() %>%
          group_by(Region) %>%
          summarise(Value = sum(Data_Time_Points,na.rm=T)) %>% 
          filter(Region != "na") %>% 
          filter(Region != "") %>% 
          filter(Region != "TBD")
        
        ggplot(data= Spp2,
               aes(
                 x=reorder(Region, -Value),
                 y=Value,
                 fill=Region
               )) +
          geom_bar(stat="identity")+
          theme_classic() +
          ylab("Data Points")+
          xlab("Region")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle = 45),
                axis.text.y = element_text(size = 14),
                legend.position = "none",
                axis.title = element_text(size=14,
                                          face="bold")
          )
        
      }else{
        #### By Location ####
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
            ylab("Data Points")+
            xlab("Location")+
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
  
#_____________________ END ___________________________ #
  
  # #### Qualitative Analysis ####
  # 
  # # Keywords_Plot ####
  # 
  # output$Keywords_Plot <- renderPlot({
  #   
  #   if(input$Words_But==TRUE){
  #     if(input$Discipline == "Todas"){
  #       Words <- datasetInput()
  #     }else{
  #       Words <- datasetInput() %>% 
  #         filter(Research_Field == input$Discipline)
  #     }
  #     
  #     WordsCorpus <- Corpus(VectorSource(Words$Keywords)) #Selects only Keywords
  #     WordsCorpus <- tm_map(WordsCorpus,
  #                           PlainTextDocument) #Converts to plain text
  #     WordsCorpus <- tm_map(WordsCorpus,
  #                           removePunctuation) #Removes punctuation
  #     
  #     Word_Remove <- c(input$Keyword_Remove1, #<- For optional word removing
  #                      input$Keyword_Remove2)
  #     
  #     #Removes a word of user preference
  #     WordsCorpus <- tm_map(WordsCorpus,
  #                           removeWords,
  #                           Word_Remove )
  #     
  #     #
  #     
  #     wordcloud(WordsCorpus, #Plots the words
  #               max.words = 100,
  #               random.order = FALSE,
  #               colors=brewer.pal(8, "Dark2"))
  #     
  #   }
  # })
  
  #_____________________ END ___________________________ #
  
  #### Timeseries of data History ####
  
  #### Timeseries of data History ####
  
  PedroData <- reactive({
    
    Pedroche<- fread("./Pedroche_Hist.csv")
    Pedroche <- Pedroche %>%
      select(-1)
    
  })
  
  
  output$TSgraph <- renderDygraph({
    if(input$Tomeseries_But == TRUE){
      Hist <- datasetInput() %>%
        filter(MMID <=2861) %>%
        select(Start_Year,End_Year) # <- Temporary fix for Brusca and Pedroche data
      
      #Reads Predoche fixed data from the Parallel Analysis
      
      # #Join both datasets and removes NA's
      Fin_Plot <- PedroData() %>%
        bind_rows(Hist) %>%
        filter(!is.na(Start_Year))
      
      
      #Plots the Historic contribution
      ts_plot(Fin_Plot,1920,2020)
    }
  })
  #_____________________ END ___________________________ #  
  
  #_____________________ END PREELIMINARY RESULTS ___________________________ #
  
  #### COLLABORATION ####
  
  #### Download Metadata Template ####
  TempInput <- reactive({
    
    data<- fread("./Data_Download/Metadata_Template.csv")
    data.frame(data)
  })
  
  output$downloadTemp <- downloadHandler(
    filename = function() { 
      paste(input$dataset, 'MIM_Template',".csv", sep='') 
    },
    content = function(file) {
      write.csv(TempInput(), file)
    }
  )
  
#_____________________ END ___________________________ #
  
  #### Institutions ####
  
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
  
  #_____________________ END ___________________________ #
  
  #### Researchers ####
  
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

#_____________________ END ___________________________ #
  
#_____________________ END COLLABORATION ___________________________ #
  
}) #<- END OF SERVER ! 

