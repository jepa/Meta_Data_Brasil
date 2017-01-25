#### NOTA ####
##############################
##############################
##############################
##############################

##### MUY IMPORTANTE ####

#Para modificar la información de la página informativa TIENES que estar en la "branch" Internet, de github. si no, vas a modificar la versión pensada para cuándo los datos estén listos.

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


#Functions #

#Commented for publication of project
#source('Fun_Dat_links.r')

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
  
  
  #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  # METADATA TAB ####
  
  # Reading the Template ####
  datasetInput <- reactive({
    
    data<- read.csv("./Template.csv", 
                    header = TRUE,
                    na="NA")
    data.frame(data)
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
                             lengthMenu = c(10, 20,30)
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
                             autoWidth = TRUE
                             )
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
  
  #### PRELIMINARY RESULTS TAB ####
  
  
  #### Timeseries of data gathering ####
  
#Import dataframe (for now)
  TFdatasetInput <- reactive({
    
  x <- read.csv("ESTA.csv")
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
    dyAxis("y", label = "Number of Data Points") %>%  #Labels
    dyLegend(width = 600)
  })
  
  #### Quantitative Results ####
  # Number of entries ####
  
  output$Number_Entries <- renderPrint({
    Number_entries <- datasetInput() %>% 
      filter(MMID != "na")
    Number_entries$MMID[length(Number_entries$MMID)]
    
  })
  
  output$Number_Data_Points <- renderPrint({
    Number_entries <- datasetInput()
    sum(Number_entries$Data_Time_Points,na.rm=T)
    
  })
  
  output$Sources <- renderPrint({
     z<- datasetInput() %>% 
      group_by(Compilation_Title) %>% 
      summarise(sum(Data_Time_Points)) %>% 
      select(-2) %>% 
      filter(!is.na(Compilation_Title)) %>% 
      mutate(z = 1)
    
    sum(z$z)
    
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
      ylab("Data Points")+
      xlab("Socio-econimic Component")+
      theme(axis.text.x = element_text(hjust = 1,
                                       size=14,
                                       angle= 45),
            axis.text.y = element_text(size = 14),
            legend.position = "none",
            axis.title = element_text(size=14,
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
        ylab("Data Points")+
        xlab("Area")+
        theme(axis.text.x = element_text(hjust = 1,
                                         size=14,
                                         angle= 45),
              axis.text.y = element_text(size = 14),
              legend.position = "top",
              axis.title = element_text(size=14,
                                        face="bold"))+ 
        guides(fill = guide_legend(title = "Socioeconomic Component",
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
            ylab("Número de Datos")+
          xlab("Region")+
          theme(axis.text.x = element_text(hjust = 1,
                                           size=14,
                                           angle= 45),
                axis.text.y = element_text(size = 14),
                legend.position = "top",
                axis.title = element_text(size=14,
                                          face="bold"))+ 
          guides(fill = guide_legend(title = "Socioeconomic Component",
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
            ylab("Número de Datos")+
            xlab("Location")+
            theme(axis.text.x = element_text(hjust = 1,
                                             size=14,
                                             angle= 45),
                  axis.text.y = element_text(size = 14),
                  legend.position = "top",
                  axis.title = element_text(size=14,
                                            face="bold"))+ 
            guides(fill = guide_legend(title = "Socioeconomic Component",
                                       title.position = "left"))
          
        }
      }
    }
  })
  
  ## Research Field Plot ####
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
              axis.title = element_text(size=14,
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
                axis.title = element_text(size=14,
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
                  axis.title = element_text(size=14,
                                            face="bold"))+ 
            guides(fill = guide_legend(title = "Research Field",
                                       title.position = "left"))
          
        }
      }
    }
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

