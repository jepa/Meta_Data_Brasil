#install.packages("dygraphs")
library(dygraphs)
library(dplyr)


#T_1_Oct <- 4
#T_1.3_Nov <- 595
#T.4_Dic <- 1000
#T.5_Jan <- 1602



Points <- c(100,650,1400,1602,numeric(11))
Point2 <- c(300,750,1800,2002,numeric(11))


Points <- c(100,650,1400,1602)
Point2 <- c(200,950,1900,2302)


Points <- ts(Points, 
             start=c(2016,10),
             end = c(2017,12), 
             frequency= 12)

Point2 <- ts(Point2,
             start=c(2016,1),
             end = c(2017,1),
             frequency = 12)

XX <- cbind(Points,Point2)



dygraph(XX) %>%
  dySeries("Points", label = "Male") %>%
  dySeries("Point2", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)
