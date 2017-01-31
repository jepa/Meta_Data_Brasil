# Time frame of datapoints production in Mexico
#This script is a modification of Andres Cisnero's script for time series analysis.

#The original file is in this folder under the name "Codigo_Andres"

#Libraries and Dataset #
library(dplyr)  #<- For data wrangling
library(dygraphs) # <- for interactive TS graphs
library(data.table)

meta <- fread("~/Documents/Github/Meta_Data_Mexico/App/Template.csv") 

#---Temporal dataset coverage--------------------------
#Start_Year <- 1900
#End_Year <- 2050
#Data <- meta

#ts_subset(meta,"",1900,2050)

ts_subset<- function(Data,Start_Year,End_Year,Category="NA"){
  
  library(dplyr)  #<- For data wrangling
  library(dygraphs)
# Data <- Data %>%
#     filter(Research_Field == "Fisheries")
  
tdat= na.omit(Data[,c("Start_Year","End_Year")]) #Subset columns with 
tdat$Start_Year<- as.numeric(as.character(tdat$Start_Year))
tdat$End_Year<- as.numeric(as.character(tdat$End_Year))
#mean(tdat$Start_Year) #just checking they are number
#mean(tdat$End_Year) #just checking they are number

#start and end year data
years= Start_Year:End_Year #Limit time span considered

x <- tdat$Start_Year[1]:tdat$End_Year[1]

ts= cbind(years, !is.na(match(years, x))==1 ) #Flag years in time series

for(i in 2:dim(tdat)[1]) #Repeat for all data
{
  x <- tdat$Start_Year[i]:tdat$End_Year[i]
  ts= cbind(ts, !is.na(match(years, x))==1)
}
tssum= cbind(ts[,1], rowSums(ts[,2:dim(ts)[2]]) ) #Sum over years

J <- data.frame(tssum) #<- Converts to data.frame

J_TS <- ts(J$X2,
           start=c(Start_Year,1),
           end = c(End_Year,12),
           frequency= 1)

#Plots it "nicelly"
tsplot<- dygraph(J_TS) %>% #Creats the graph
  dySeries( "V1", label= "Data Points") %>%
  dyOptions(stackedGraph = TRUE, #Makes it stacked
            drawPoints = TRUE, #Shows each data point
            pointSize = 4) %>%
  dyRangeSelector(height = 20) %>%
  dyAxis("x", drawGrid = FALSE) %>% #Removes the grid
  dyAxis("y", drawGrid = FALSE) %>%
  dyAxis("y", label = "Records") %>%  #Labels
  dyLegend(width = 600)

return(tsplot)

}


C <- ts_subset(Template,1900,2050,"Conservation")
Fi <- ts_subset(meta,1900,2050,"Fisheries")
O <- ts_subset(meta,1900,2050,"Oceanography")
S <- ts_subset(meta,1900,2050,"Sociology")

Fin <- cbind(A,B,C,Fi,O,S)
colnames(Fin) <- c("A","B","C","Fi","O","S")

#Transforms the results to time series
J_TS <- ts(Fin,
           start=c(1900,1),
           end = c(2050,1), 
           frequency= 12)

#Plots it "nicelly"
dygraph(J_TS) %>% 
  # dySeries("V1", label = "Data Points") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)



### End of "Mi cuchara ####