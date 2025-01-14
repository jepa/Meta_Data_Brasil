#install.packages("dygraphs")
library(dplyr)
library(dygraphs)

#### EXAMPLE ####

# This graph is made with the package dygraphs foind in 
#http://rstudio.github.io/dygraphs/

# Create a dataframe of points
Points <- c(100,650,1400,1602,numeric(11))
Point2 <- c(300,750,1800,2002,numeric(11))

#Convert them to time series using the "ts()" function
Points <- ts(Points, 
             start=c(2016,10),
             end = c(2017,12), 
             frequency= 12)

Point2 <- ts(Point2,
             start=c(2016,1),
             end = c(2017,1),
             frequency = 12)

#Bind both datasets
XX <- cbind(Points,Point2)


#Graph it!
dygraph(XX) %>%
  dySeries("Points", label = "Male") %>%
  dySeries("Point2", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

####

#### Metadatabase ####

#### Data Enteries #### 

# Division of data inout by excell sheet (aprox.) #
#T_1_Oct <- 4
#T_1.3_Nov <- 595
#T.4_Dic <- 1000
#T.5_Jan <- 1602


#Subset Template by Eddition
###

#I needed to add Conservartion manually since there is no data collected this month

y <- data.frame(matrix(c("Conservation",0),nrow = 1))
colnames(y) <- c("Research_Field","n")
y$n<- as.numeric(as.factor(y$n))

T1 <- Template %>% 
  filter(MMID <= 595) %>% 
  group_by(Research_Field) %>% 
  summarise(n=n()) %>% 
  filter(Research_Field !="na",
         Research_Field != "Marine") %>% 
  bind_rows(y)

Tt1<-data.frame(t(T1)) %>% 
  slice(2)
colnames(Tt1) <- T1$Research_Field


###
#I needed to add Aquaculture manually since there is no data collected this month

x <- data.frame(matrix(c("Aquaculture",0),nrow = 1))
colnames(x) <- c("Research_Field","n")
x$n<- as.numeric(as.factor(x$n))
#
T2 <- Template %>% 
  filter(MMID >= 595) %>% 
  filter(MMID <= 1000) %>% 
  group_by(Research_Field) %>% 
  summarise(n=n()) %>% 
  filter(Research_Field !="na",
         Research_Field != "Marine") %>% 
  bind_rows(x)

Tt2<-data.frame(t(T2))%>% 
  slice(2)
colnames(Tt2) <- T2$Research_Field

###

T3 <- Template %>% 
  filter(MMID >= 1000) %>% 
  filter(MMID <= 1602) %>% 
  group_by(Research_Field) %>% 
  summarise(n=n()) %>% 
  filter(Research_Field !="na")

Tt3<-data.frame(t(T3)) %>% 
  slice(2)
colnames(Tt3) <- T3$Research_Field


# Bind all edditions
TT <- cbind(T1,T2,T3)
TTt <-rbind(Tt1,Tt2,Tt3)

#write.csv(ESTA,"ESTA.csv")

#### Data Points ####

yy <- data.frame(matrix(c("Conservation",0),nrow = 1))
colnames(yy) <- c("Research_Field","n")
yy$n<- as.numeric(as.factor(yy$n))

T1_DP <- Template %>% 
  filter(MMID <= 595) %>% 
  group_by(Research_Field) %>% 
  summarise(Value = sum(Data_Time_Points,na.rm=T)) %>%  
  filter(Research_Field !="na",
         Research_Field != "Marine") %>% 
  bind_rows(yy)

Tt1_DP<-data.frame(t(T1_DP)) %>% 
  slice(2)
colnames(Tt1_DP) <- T1_DP$Research_Field


###
#I needed to add Aquaculture manually since there is no data collected this month

xx <- data.frame(matrix(c("Aquaculture",0),nrow = 1))
colnames(xx) <- c("Research_Field","n")
xx$n<- as.numeric(as.factor(xx$n))
#
T2_DP <- Template %>% 
  filter(MMID >= 595) %>% 
  filter(MMID <= 1000) %>% 
  group_by(Research_Field) %>% 
  summarise(Value = sum(Data_Time_Points,na.rm=T)) %>%  
  filter(Research_Field !="na",
         Research_Field != "Marine") %>% 
  bind_rows(xx)

Tt2_DP<-data.frame(t(T2_DP))%>% 
  slice(2)
colnames(Tt2_DP) <- T2_DP$Research_Field

###

T3_DP <- Template %>% 
  filter(MMID >= 1000) %>% 
  filter(MMID <= 1602) %>% 
  group_by(Research_Field) %>% 
  summarise(Value = sum(Data_Time_Points,na.rm=T)) %>%  
  filter(Research_Field !="na")

Tt3_DP<-data.frame(t(T3_DP)) %>% 
  slice(2)
colnames(Tt3_DP) <- T3_DP$Research_Field


# Bind all edditions
# TT <- cbind(T1,T2,T3)
TTt_DP <-rbind(Tt1_DP,Tt2_DP,Tt3_DP)

#write.csv(TTt_DP,"ESTA2.csv")


# Convert them to ts
# D_Points <- ts(TT,
#                start=c(2016,10),
#                end = c(2017,12), 
#                frequency= 12)

#### START HERE FOR DATA POINTS ####

# Import ESTA2 dataset 

Dt_Points <- ts(ESTA2,
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

