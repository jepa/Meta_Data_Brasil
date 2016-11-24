####
# Data wrangling script 
####

library(xlsx)
library(dplyr)
library(tidyr)

#### SEa Around Us Data ####
SAU <- read.csv("/Users/jpalacios/Downloads/SAU EEZ 945 v1-43/SAU EEZ 945 v1-43.csv")

Common_Names <- levels(SAU$common_name)
Scin_names <- levels(SAU$scientific_name)
Comm_Sci_Names <- levels(c(SAU$common_name,SAU$scientific_name))

Data <- SAU %>% 
  select(scientific_name, common_name) %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Suma = sum(Value)) %>% 
  select(-Suma) %>% 
  mutate(Nombre = "Catch of")

Data$common_name <-as.factor(Data$common_name)
Data$Nombre <- as.factor(Data$Nombre)

write.csv(Data, "SAUdata.csv")


  

  
  