### This script is to see how would a map look like for the geographic data #

library(ggmap)
library("rgdal") 

# for file_path_sans_ext()
#install.packages('tools')
library("tools") 

# for inner_join(), summarise() and the pipe operator (%>%)
#install.packages('dplyr')
library("dplyr") 

# for fortify() and plotting
#install.packages('ggplot2')
library("ggplot2") 

# for point.in.polygon() and spDists()
#install.packages('sp')
library("sp") 

path.ne.coast <- ("/Users/jpalacios/Documents/Github/studyGroup/lessons/Intro_Map/Data/ne_10m_coastline")
file_name <- "ne_10m_coastline.shp"

# Loading the shapefile:
data_coast <- readOGR(dsn = path.ne.coast, 
                      layer = file_path_sans_ext(file_name))

quick.subset <- function(x, domain){
  x@data$id <- rownames(x@data)
  x.f = fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
  x.subset <- subset(x.join, x.join$long > domain[1] & 
                       x.join$long < domain[2] & 
                       x.join$lat > domain[3] & 
                       x.join$lat < domain[4])
  x.subset
}

#### _______________ End of Function _____________________###

#### Subsetting our data ####

# Specify the desired domain (the West Coast of Alaska, Canada and US):
P_Lat_N  <- 23     #Pacific_Latitude_North
P_Lat_S  <- 102     #Pacific_Latitude_South
P_Long_W <- -179.5 #Pacific_Longitude_West
P_Long_E <- -120.5 #Pacific_Longitude_East

domain <- c(P_Long_W, P_Long_E, P_Lat_S, P_Lat_N)

# Extract the coastline data for the desired domain using quick.subset():
data_coast.wc <- quick.subset(data_coast, #Orginal shapefile
                              domain #Limits
) # 4132x8

kable(head(data_coast.wc,5),
      caption = "Table 1. First 5 rows of the Subsetted Data") #Kable is just a fancy way to show data frames ;)