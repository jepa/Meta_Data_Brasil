# Function for GBIF data processing...

GBIF_Create <- function(Inicio, Fin, Keywords, Title){
  library(dplyr)
  library(data.table)  
  
  Data <- read_delim(paste("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/",Title,".csv", sep=""), 
                             "\t", escape_double = FALSE)
  
  Species <- Data %>%  #Datos con especie
    filter(taxonrank =="SPECIES") %>% 
    group_by(species) %>% 
    summarise(
      mean_Long = mean(decimallongitude, na.rm =T),
      mean_Lat = mean(decimallatitude, na.rm =T),
      min_y = min(year, na.rm = T),
      max_y = max(year, na.rm=T),
      n = length(unique(year))
    ) %>% 
    mutate(Titulo = paste(Inicio,species, Fin)) %>% 
    mutate(Key = paste(Keywords))
  
  Genus <- Data %>%  #Datos con especie
    filter(taxonrank =="GENUS") %>% 
    group_by(genus) %>% 
    summarise(
      mean_Long = mean(decimallongitude, na.rm =T),
      mean_Lat = mean(decimallatitude, na.rm =T),
      min_y = min(year, na.rm = T),
      max_y = max(year, na.rm=T),
      n = length(unique(year))
    ) %>% 
    mutate(Titulo = paste(Inicio, genus, "(Gen.)",Fin)) %>% 
    mutate(Key = paste(Keywords)) %>% 
    rename(species = genus)
  
  Family <- Data %>%  #Datos con especie
    filter(taxonrank =="FAMILY") %>% 
    group_by(family) %>% 
    summarise(
      mean_Long = mean(decimallongitude, na.rm =T),
      mean_Lat = mean(decimallatitude, na.rm =T),
      min_y = min(year, na.rm = T),
      max_y = max(year, na.rm=T),
      n = length(unique(year))
    ) %>% 
    mutate(Titulo = paste(Inicio, family, "(Fam.)",Fin)) %>% 
    mutate(Key = paste(Keywords)) %>% 
    rename(species = family) %>% 
    bind_rows(Species,
              Genus)
  
  ### Build Metadata template ##
  New_Template<-data.table(
  Short_Title = Family$Titulo,
  Keywords = Family$Key,
  Author = Author,
  Institution = Institution,
  Dataset_Available = 1,
  Subject_name = Family$species,
  Area = Area,
  Region = Region, 
  Location = Location, 
  Start_Year = Family$min_y, 
  End_Year = Family$max_y, 
  Data_Time_Points = Family$n, 
  Unit_Type = "Value", 
  Temporal_Resolution = "Year", 
  Spatial_Resolution = "Georeferenced", 
  Dataset_Title = Dataset_Title, 
  Compilation_Title = "Global Biodiversity Information Facility", 
  Publication_Year = 2017, 
  Reference = Reference, 
  Available_Metadata = "NA", 
  Metadata_name = "NA", 
  User_Contact = User_Contact, 
  Institution_Type = "ACA", 
  Research_Fund = "GOV", 
  Research_Field = "Ecology", 
  SE_Interaction = "Status", 
  Data_Uncertanty = "NA", 
  Notes = "NA", 
  Lat = Family$mean_Lat, 
  Long = Family$mean_Long
  )
  
  
  write.csv(New_Template,
            paste(Title,"_Final.csv",sep=""))
}
