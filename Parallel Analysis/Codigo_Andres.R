#### Andres Cisnero's code for Temporal analysis of the data ####
# With my own modifications in sections that say: "mi cuchara" or otherwise commented with a (J)

#Libraries and Dataset #
library(dplyr)  #<- For data wrangling
library(dygraphs) # <- for interactive TS graphs

meta <- fread("~/Documents/Github/Meta_Data_Mexico/App/Template_4.1.csv") 

#---Temporal dataset coverage--------------------------

tdat= na.omit(meta[,c("Start_Year","End_Year")]) #Subset columns with 
tdat$Start_Year<- as.numeric(as.character(tdat$Start_Year))
tdat$End_Year<- as.numeric(as.character(tdat$End_Year))
#mean(tdat$Start_Year) #just checking they are number
#mean(tdat$End_Year) #just checking they are number

#start and end year data
years= 1900:2050 #Limit time span considered

#x = tdat[1,1]:tdat[1,2] #First full time series #<- not working for me (J) So this should work as well:
x <- tdat$Start_Year[1]:tdat$End_Year[1] # <- (J)
#It works!

ts= cbind( years, !is.na(match(years, x))==1 ) #Flag years in time series
for(i in 2:dim(tdat)[1]) #Repeat for all data
{
  #x= tdat[i,1] : tdat[i,2]
  x <- tdat$Start_Year[i]:tdat$End_Year[i]
  ts= cbind(ts, !is.na(match(years, x))==1)
}
tssum= cbind(ts[,1], rowSums(ts[,2:dim(ts)[2]]) ) #Sum over years


### Un poco de "mi cuchara" ####

J <- data.frame(tssum) #<- Converts to data.frame

#Transforms the results to time series
J_TS <- ts(J$X2,
          start=c(1900,1),
          end = c(2050,12), 
          frequency= 1)

#Plots it "nicelly"
dygraph(J_TS) %>%
  dyAxis("x",
         labelWidth = 26,
         drawGrid = FALSE) %>%
  dyAxis("y", label = "Number of data points in Metadata Records",
         labelWidth = 26,
         drawGrid = FALSE) %>%
  dySeries("V1", label = "Data Points") %>%
  dyOptions(stackedGraph = TRUE,
            axisLabelFontSize = 26,
            axisLabelWidth = 150
            ) %>%
  dyRangeSelector(dateWindow = c("1950-1-1","2017-1-1"))
  

### End of "Mi cuchara ####


#Fig Temporal Coverage
ftc= function()
{
  xpos= barplot(tssum[,2], ylim= c(0,max(tssum[,2])+50), xlab="Year", ylab="# Data Points",
                col="slategrey", border="slategrey")
  axis(1, at=xpos[c(1,seq(16,151,15))], labels=c(1900,seq(1915,2050,15)))
}
jpeg("Fig Temporal Coverage.jpg", height=5, width=6, units= "in", res=400)
ftc()
dev.off()


#Matplot total and ecological assessments
xx= subset(meta, Area=="Atlantic" | Area=="Pacific" ) # <- (J) changed the area for the Mexico metadata

tdat= na.omit(xx[,c("Start_Year","End_Year")])
years= 1900:2050

#x= tdat[i,1] : tdat[i,2] # <- (J) Not working for me (??)
x <- tdat$Start_Year[1]:tdat$End_Year[1] # <- (J)

ts= cbind( years, !is.na(match(years, xx))==1 )
for(i in 2:dim(tdat)[1])
{
  #x= tdat[i,1] : tdat[i,2]
  x <- tdat$Start_Year[1]:tdat$End_Year[1]
  ts= cbind(ts, !is.na(match(years, xx))==1)
}
tdat.eco= cbind(ts[,1], rowSums(ts[,2:dim(ts)[2]]) )
colnames(tdat.eco)= c("Year","Value")

#Frequency of years in all data (from Metadata)
tdat= na.omit(meta[,c("Start_Year","End_Year")])
years= 1900:2050
#x= tdat[1,1]:tdat[1,2]
x <- tdat$Start_Year[1]:tdat$End_Year[1]

ts= cbind( years, !is.na(match(years, x))==1 )
for(i in 2:dim(tdat)[1])
{
  #x= tdat[i,1] : tdat[i,2]
  x <- tdat$Start_Year[1]:tdat$End_Year[1]
  ts= cbind(ts, !is.na(match(years, x))==1)
}
tdat= cbind(ts[,1], rowSums(ts[,2:dim(ts)[2]]) )
colnames(tdat)= c("Year","Value")


tdats= as.data.frame(cbind(tdat, tdat.eco[,2]))
colnames(tdats)= c("Year","Total","Eco")
tdats$Ratio= tdats$Eco / tdats$Total
tdats= subset(tdats, Year<2016)

fig.eco.dat= function()
{
  par(bty="l", cex=0.9)
  
  stackpoly(tdats[,3:2], stack=F, xat=c(seq(1,length(tdats$Year),by=20),length(tdats$Year)),
            xaxlab=c(seq(1900,max(tdats$Year),by=20),max(tdats$Year)),
            cex.lab=1, xlab="Year",ylab= "No. of assessments including data",
            col=c("slategrey","lightgrey"),border=NA,lwd=1,axis4=F)
  
  legend("topleft", c("All Assessments","Multi-Species and Ecosystem-Based"),
         pch=15, pt.cex= 2, col=c("lightgrey","slategrey"), bty="n")
  
}
jpeg("Fig Temporal Coverage Ecological.jpg", height=5, width=6, units= "in", res=400)
fig.eco.dat()
dev.off()


#Proportion of data in assessments that are multi-species or ecosystem-based 
plot(tdats$Year, tdats$Ratio, t="b", xlab="Year",ylab="Ecosystem-based assessment data as proportion of total")

mean(subset(tdats, Year>1950 & Year<2000)$Ratio)
mean(subset(tdats, Year>=2000)$Ratio)
mean(subset(tdats, Year>=2010)$Ratio)
tdats[tdats$Year==2015,"Ratio"]

