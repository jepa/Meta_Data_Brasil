#Juliano Palacios#
# GoM Workshop for International Research #

#Libraries and data
library('dplyr')
setwd("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis")
Data <- read.csv("./Data/Atlantic_Data.csv")


#
Clear <- Data %>% 
  group_by(Subject_name,Location) %>% 
  summarise(n=sum(Data_Time_Points, na.rm=T))
  

