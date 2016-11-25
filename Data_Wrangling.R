####
# Data wrangling script 
####

#library(xlsx)
library(dplyr)
library(tidyr)

#### Sea Around Us Data, Pacific ####

# Title ####

#I'm trying to add the text "catch of" and "In mexico" to every single row of the SAU database so I only need to copy and paste and not go manually row by row...

## Hal 1000 path ##
#SAU <- read.csv("/Users/jpalacios/Downloads/SAU EEZ 945 v1-43/SAU EEZ 945 v1-43.csv")

## Clementina path ##
SAU <- read_csv("~/Documents/Box Sync/UBC/Metadata_Mexico/English/Datasets/SAU EEZ 945 v1-43/SAU EEZ 945 v1-43.csv")


#Step 1 ###
#Separates the vectors I want (common_name and scientific_name) and add the two new columns ("Catch of" and "in Mexico")

Data <- SAU %>% 
  select(scientific_name, common_name) %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Suma = sum(Value)) %>% 
  select(-Suma) %>% 
  mutate(Nombre = "Catch of") %>% 
  mutate(Nombre2 = "in Mexico")

#Step 2 ###
# the paste function pastest whatever factor you tell it to, so I just paste them in a new dataframe and, vuala!!!! 

a<- data.frame(paste(Data$Nombre,
                     Data$scientific_name,
                     Data$Nombre2))

#write.csv(a, "SAUdata.csv")

# Necesito el listado de nombres scientíficos porque Excel apesta...

b <- data.frame(Data$scientific_name)
colnames(b) <- "Scientific_Name"
#write.csv(b, "SAUSci_Names.csv")

#Step 3 ###
#Now we do the same for the Keywords
Keywords <- b %>% 
  mutate(KW = "Catch; Reconstructed Data;")


Keywords_F <- data.frame(paste(Keywords$KW,
                               Keywords$Scientific_Name))
#write.csv(Keywords_F, "Keywords_F.csv")


# Number of observations per species ####
#Now I need to figure out how many observations exists for each species, this should be easy, however..

Observations <- SAU %>% 
  mutate(Value =1) %>% 
  group_by(common_name, year) %>% 
  summarise(top = sum(Value)) %>% 
  mutate(Value2 =1) %>% 
  group_by(common_name) %>% 
  summarise(top2 =sum(Value2))


#write.csv(Observations,"Observations.csv")

#Just checking that the info is correct  
Haliotis <- SAU %>% 
  filter(scientific_name =="Haliotis") %>% 
  group_by(year) %>% 
  summarise(Total=sum(landed_value))



#Timeframe ####
# And now... when does it starts and when does it finishes?

Timeframe <- SAU %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name, year) %>% 
  summarise(top = sum(Value)) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(inicio = min(year), fin = max(year))
  
#write.csv(Timeframe, "Timeframe.csv")


####################### END ########################

#### Sea Around Us Data, Atlantic ####

# Title ####

#I'm trying to add the text "catch of" and "In mexico" to every single row of the SAU database so I only need to copy and paste and not go manually row by row...

## Hal 1000 path ##

## Clementina path ##
SAU_A <- read_csv("~/Documents/Box Sync/UBC/Metadata_Mexico/English/Datasets/SAU EEZ 944 v1-43/SAU EEZ 944 v1-43.csv")


#Step 1 ###
#Separates the vectors I want (common_name and scientific_name) and add the two new columns ("Catch of" and "in Mexico")

Data <- SAU_A %>% 
  select(scientific_name, common_name) %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Suma = sum(Value)) %>% 
  select(-Suma) %>% 
  mutate(Nombre = "Catch of") %>% 
  mutate(Nombre2 = "in Mexico")

#Step 2 ###
# the paste function pastest whatever factor you tell it to, so I just paste them in a new dataframe and, vuala!!!! 

a<- data.frame(paste(Data$Nombre,
                     Data$scientific_name,
                     Data$Nombre2))

#write.csv(a, "SAUdata.csv")

# Necesito el listado de nombres scientíficos porque Excel apesta...

b <- data.frame(Data$scientific_name)
colnames(b) <- "Scientific_Name"
#write.csv(b, "SAUSci_Names.csv")

#Step 3 ###
#Now we do the same for the Keywords
Keywords <- b %>% 
  mutate(KW = "Catch; Reconstructed Data;")


Keywords_F <- data.frame(paste(Keywords$KW,
                               Keywords$Scientific_Name))
#write.csv(Keywords_F, "Keywords_F.csv")


# Number of observations per species ####


Observations_A <- SAU_A %>% 
  mutate(Value =1) %>% 
  group_by(common_name, scientific_name, year) %>% 
  summarise(top = sum(Value)) %>% 
  mutate(Value2 =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Data_Points =sum(Value2))


#Timeframe ####
# And now... when does it starts and when does it finishes?

Timeframe_A <- SAU_A %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name, year) %>% 
  summarise(top = sum(Value)) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(inicio = min(year), fin = max(year))

#write.csv(Timeframe, "Timeframe.csv")


Atlantic <- data.frame(a,Keywords_F,Timeframe_A,Observations_A$Data_Points)
#write.csv(Atlantic, "Atlantic_Clear_SAU.csv")
####################### END ########################

