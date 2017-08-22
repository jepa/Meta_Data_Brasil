library(data.table)
library(dplyr)

#### Step 1 ####
# Upload the latest Template from the English file

Template <- fread("~/Documents/Github/Meta_Data_Mexico/App_Eng/Template.csv")

#### Step 2 ####
# Fill in the fields that will be repeated in the template

Short_Title <- "Coastal Biodiversity Surveys: Point Contact (Punta Abreojos BCS SON)"
Keywords <- "Oceano; Procesos Intertidal; Zona entre mareas; Biologaa Marina; Invertebrados marinos; Plantas marinas; Habitat marino; Biosfera; Habitats acuaticos; Habitat Costero; Datos de la Encuesta Comunitaria; 
Datos intermareales; 
Datos de la encuesta de la comunidad intermareal; 
Encuesta Intertidal de Biodiversidad Costera; 
Encuesta SWAT"
Author <- "Raimondi et al"
Institution <- "PISCO"
Dataset_Available <- 1 #Options: 1 (open source); 2 (Available after request); 3 (Private/Not available)
Subject_name <- "Multiple Species"
Area <- "Pacific" #Options (Pacific, Atlantic, National, Freshwater/Terrestrial)
Region <- "G. of California" 
Location <- "Punta Abreojos"
Start_Year <- 2001
End_Year <- 2010
Data_Time_Points <- 10
Unit_Type <- "Percentage Cover" 
Temporal_Resolution <- "Year"
Spatial_Resolution <- "Georeferenced"
Dataset_Title <- "PISCO: Intertidal: Coastal Biodiversity Surveys: Point Contact" 
Compilation_Title <- ""
Publication_Year <- 2008
Reference <- "doi:10.6085/AA/pisco_intertidal.50.6."
Available_Metadata <- "NA" 
Metadata_name <- "NA" 
User_Contact <- "Melissa Miner, mwilson@biology.ucsc.edu; Dr. Carol Blanchette, blanchet@lifesci.ucsb.edu" 
Institution_Type <- "INT" #Options: ACA, GOV, NGO, IGO, PRI, INT 
Research_Fund <- "INT" #Options: ACA, GOV, NGO, IGO, PRI, INT
Research_Field <- "Ecology" 
SE_Interaction <- "Status" 
Data_Uncertanty <- "NA" 
Notes <- "NA" 
Lat <- -113.5763
Long <- 26.7054833



  # INCLUR EL PRIMERO y TERCERO DESPUES

#### Step 3 ####
# Merge them intoo one dataset

New_Template <- data.table(
  Short_Title,
  Keywords,
  Author,
  Institution,
  Dataset_Available,
  Subject_name,
  Area,
  Region, 
  Location, 
  Start_Year, 
  End_Year, 
  Data_Time_Points, 
  Unit_Type, 
  Temporal_Resolution, 
  Spatial_Resolution, 
  Dataset_Title, 
  Compilation_Title, 
  Publication_Year, 
  Reference, 
  Available_Metadata, 
  Metadata_name, 
  User_Contact, 
  Institution_Type, 
  Research_Fund, 
  Research_Field, 
  SE_Interaction, 
  Data_Uncertanty, 
  Notes, 
  Lat, 
  Long
  )

### Step 3.1 ### 
# All es gut?
#View(New_Template)


#### Step 4 ####
#Merge the New Template to the old Template many times...

#Templateb <- New_Template

Templateb <- Templateb %>%
  bind_rows(New_Template)

#### Step 5 ####
#Save it!
  
write.csv(Templateb,
          "TemplateB.csv")



# Voala!!


#### Eddits ###
### NOTE: KEEP COMMENTED WHEN NOT IN USE

#### Removing rows ####

# Templateb <- Templateb %>%
#   slice(-21:-30)


#### Correcting Entire values or Columns ####

Templateb$Reference[21:30]= "doi:10.6085/AA/pisco_intertidal.51.6"

#### Changing Words ####
# Only one column
Templateb$COLUMN[21:30] <- gsub(
  "", #Word you want to change
  "", #New Word
  Templateb$COLUMN)

#All Columns #

Templatebb <- data.frame(lapply(Templateb, function(x) {
                  gsub("Swath Surveys", #Word you want to change
                       "Quadrat Surveys", #New Word
                       x)
              }))

#### Free for all ####

Templateb <- Templateb %>% 
  bind_rows(Templateb[1:10])

TemplateC <- Templateb[1:10]
View(TemplateC)
