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
        selectInput("information",
                    "Show Results by:",
                    choices = c("Person",
                                "Institution"),
                    selected="Person"
                    ),
        selectInput("members",
                     "Core Group Member",
                     choices = c(
                       "Chore Group" = "All",
                       "Andres Cisneros" = "A",
                       "Francisco Arreguín" = "P",
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
    
    read.csv("./Red.csv", header = TRUE)
    
  })
  
  output$Network_Plot <- renderSimpleNetwork({
    
    Red <- RedInput()
    
    if(input$information == "Person"){
    if(input$members == "All"){
      networkData <- data.frame(Red$CGM_Member, Red$Contact_Person) 
      
      }else{
        
        if(input$members == "L"){
          networkData <- data.frame(Red$CGM_Member, Red$Contact_Person) 
        networkData <- networkData%>% 
          filter(Red.CGM_Member == "Laura")
        
    }else{
      
      if(input$members == "A"){
      networkData <- data.frame(Red$CGM_Member, Red$Contact_Person)
      networkData <- networkData%>%
        filter(Red.CGM_Member == "Andres")
      
    }else{
      
      if(input$members == "J"){
      networkData <- data.frame(Red$CGM_Member, Red$Contact_Person)
      networkData <- networkData%>%
        filter(Red.CGM_Member == "Juliano")
      
    }else{
      
      if(input$members == "MAC"){
      networkData <- data.frame(Red$CGM_Member, Red$Contact_Person)
      networkData <- networkData%>%
        filter(Red.CGM_Member == "Miguel Angel")
      
    }else{
      
      if(input$members == "P"){
      networkData <- data.frame(Red$CGM_Member, Red$Contact_Person)
      networkData <- networkData%>%
        filter(Red.CGM_Member == "Paco")
      
    }else{
      if(input$members == "W"){
      networkData <- data.frame(Red$CGM_Member, Red$Contact_Person)
      networkData <- networkData%>%
        filter(Red.CGM_Member == "William")
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
    }else{
      if(input$information == "Institution"){
      if(input$members == "All"){
        networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution) 
        
      }else{
        
        if(input$members == "L"){
          networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution) 
          networkData <- networkData%>% 
            filter(Red.CGM_Member == "Laura")
          
        }else{
          
          if(input$members == "A"){
            networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution)
            networkData <- networkData%>%
              filter(Red.CGM_Member == "Andres")
            
          }else{
            
            if(input$members == "J"){
              networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution)
              networkData <- networkData%>%
                filter(Red.CGM_Member == "Juliano")
              
            }else{
              
              if(input$members == "MAC"){
                networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution)
                networkData <- networkData%>%
                  filter(Red.CGM_Member == "Miguel Angel")
                
              }else{
                
                if(input$members == "P"){
                  networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution)
                  networkData <- networkData%>%
                    filter(Red.CGM_Member == "Paco")
                  
                }else{
                  if(input$members == "W"){
                    networkData <- data.frame(Red$CGM_Member, Red$Contact_Institution)
                    networkData <- networkData%>%
                      filter(Red.CGM_Member == "William")
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
      }
    }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

