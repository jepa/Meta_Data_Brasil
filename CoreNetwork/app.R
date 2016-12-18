#
# Core Network App
#
#

library(shiny)
library(dplyr)
library(networkD3)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Core Group Network"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("members",
                     "Core Group Member",
                     choices = c(
                       "Chore Group" = "All",
                       "Andres Cisneros" = "A",
                       "Francisco Algo" = "P",
                       "Juliano Palacios" ="J",
                       "Laura Rodriguez" = "L",
                       "Miguel Angel Cisneros" = "MAC",
                       "William Cheung" = "W"
                     ),
                     selected = "All"
                     )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        simpleNetworkOutput("Network_Plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  RedInput <- reactive({
    
    read_csv("./Red.csv")
    
  })
  
  output$Network_Plot <- renderSimpleNetwork({
    
    Red <- RedInput()
    
    if(input$members == "All"){
      networkData <- data.frame(Red$CGM, Red$Contact) 
      
      }else{
        
        if(input$members == "L"){
          networkData <- data.frame(Red$CGM, Red$Contact) 
        networkData <- networkData%>% 
          filter(Red.CGM == "Laura")
        
    }else{
      
      if(input$members == "A"){
      networkData <- data.frame(Red$CGM, Red$Contact)
      networkData <- networkData%>%
        filter(Red.CGM == "Andres")
      
    }else{
      
      if(input$members == "J"){
      networkData <- data.frame(Red$CGM, Red$Contact)
      networkData <- networkData%>%
        filter(Red.CGM == "Juliano")
      
    }else{
      
      if(input$members == "MAC"){
      networkData <- data.frame(Red$CGM, Red$Contact)
      networkData <- networkData%>%
        filter(Red.CGM == "Miguel Angel")
      
    }else{
      
      if(input$members == "P"){
      networkData <- data.frame(Red$CGM, Red$Contact)
      networkData <- networkData%>%
        filter(Red.CGM == "Paco")
      
    }else{
      if(input$members == "W"){
      networkData <- data.frame(Red$CGM, Red$Contact)
      networkData <- networkData%>%
        filter(Red.CGM == "William")
      }
    }
    }
    }
    }
    }
      }
    
    simpleNetwork(networkData,
                  linkColour="red",
                  zoom=T,
                  fontSize = 15)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

