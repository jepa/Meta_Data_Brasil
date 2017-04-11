
#library(dplyr)

#Est sript es para intentar hacer que el historial de puntos clectados sea automatizado

Oct <- TemplateB %>% 
  filter(MMID <= 591) %>% 
  group_by(Research_Field) %>% 
  summarise(Oct=n())

Nov <- TemplateB %>% 
  filter(MMID >= 592) %>% 
  filter(MMID <= 1589) %>% 
  group_by(Research_Field) %>% 
  summarise(Nov=n())

F_Table <- merge(Oct,
                 Nov,
                 by= "Research_Field",
                 all=TRUE)

Dec <- TemplateB %>% 
  filter(MMID >= 1590) %>% 
  filter(MMID <= 3190) %>% 
  group_by(Research_Field) %>% 
  summarise(Dec=n())

F_Table <- merge(F_Table,
                 Dec,
                 all=TRUE)

Jan <- TemplateB %>% 
  filter(MMID >= 3191) %>% 
  filter(MMID <= 10624) %>% 
  group_by(Research_Field) %>% 
  summarise(Jan=n())

F_Table <- merge(F_Table,
                 Jan,
                 all=TRUE)

Feb <- TemplateB %>% 
  filter(MMID >= 10624) %>% 
  filter(MMID <= 13789) %>% 
  filter(Research_Field != "Fisheries; Aquaculture") %>% 
  group_by(Research_Field) %>% 
  summarise(Feb=n())

F_Table <- merge(F_Table,
                 Feb,
                 all=TRUE)

Mar <- TemplateB %>% 
  filter(MMID >= 13790) %>% 
  filter(MMID <= 37993) %>% 
  filter(Research_Field != "Fisheries; Aquaculture") %>% 
  filter(Research_Field != "Fisheries; Conservation") %>% 
  group_by(Research_Field) %>% 
  summarise(Mar=n())

Names <- paste(F_Table$Research_Field)

F_Table <- merge(F_Table,
                 Mar,
                 all=TRUE) 

Names <- paste(F_Table$Research_Field)

Final_T <- F_Table %>% 
  select(-1)
  
Final_T <- transpose(Final_T)

colnames(Final_T) <- Names




Dt_Points <- ts(Final_T,
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
