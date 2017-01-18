####
# Data wrangling script 
#The data used is added in the MMd as "Reconstructed" data, I added "Industria, artisanal, etc to keywords
####


#### NOTE Allways run this first ####
#library(xlsx)
library(dplyr)
library(tidyr)

setwd("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis")

#________________________________________________________#

#### Sea Around Us Data, Pacific ####

# Title ####

#I'm trying to add the text "catch of" and "In mexico" to every single row of the SAU database so I only need to copy and paste and not go manually row by row...

## Hal 1000 path ##
#SAU <- read.csv("/Users/jpalacios/Downloads/SAU EEZ 945 v1-43/SAU EEZ 945 v1-43.csv")

## Clementina path ##
SAU <- read_csv("~/Documents/Box Sync/UBC/Metadata_Mexico/English/Datasets/SAU_Pacific/SAU EEZ 945 v1-43.csv")



#Step 1 ###
#Separates the vectors I want (common_name and scientific_name) and add the two new columns ("Catch of" and "in Mexico")

Data <- SAU %>% 
  select(scientific_name, common_name) %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Suma = sum(Value)) %>% 
  select(-Suma) %>% 
  mutate(Nombre = "Catch of") %>% 
  mutate(Nombre2 = "in Mexico (Pacific)")

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
  mutate(KW = "Captura; Reconstruida; Pacifico; Industrial; Subsistencia;Artesanal;Deportiva")


Keywords_F <- data.frame(paste(Keywords$KW,
                               Keywords$Scientific_Name))
#write.csv(Keywords_F, "Keywords_F.csv")


# Number of observations per species ####
#Now I need to figure out how many observations exists for each species, this should be easy, however..

Observations <- SAU %>% 
  mutate(Value =1) %>% 
  group_by(scientific_name,common_name, year) %>% 
  summarise(top = sum(Value)) %>% 
  mutate(Value2 =1) %>% 
  group_by(common_name,scientific_name) %>% 
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

#A as a final act, we put them together in one big table and save it

Final_SAU_Pacific <- data.frame(a, #Short title
                                Keywords_F, #Keywords and scientific name
                                Observations$top2, #Number of data points
                                Timeframe) #Begining and end

colnames(Final_SAU_Pacific) <- c("Short Title","Keywords","Number Data Points", "Time Frame","Scientific Name","First Y","End")
#write.csv(Final_SAU_Pacific, "Final_SAU_Pacific.csv")

####################### END ########################

#### Sea Around Us Data, Atlantic ####

# Title ####

#I'm trying to add the text "catch of" and "In mexico" to every single row of the SAU database so I only need to copy and paste and not go manually row by row...

## Hal 1000 path ##

## Clementina path ##
SAU_A <- read_csv("~/Documents/Box Sync/UBC/Metadata_Mexico/English/Datasets/SAU_Atlantic/SAU EEZ 944 v1-43.csv")


#Step 1 ###
#Separates the vectors I want (common_name and scientific_name) and add the two new columns ("Catch of" and "in Mexico")

Data <- SAU_A %>% 
  select(scientific_name, common_name) %>% 
  mutate(Value =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Suma = sum(Value)) %>% 
  select(-Suma) %>% 
  mutate(Nombre = "Catch of") %>% 
  mutate(Nombre2 = "in Mexico (Atlantic)")

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
  mutate(KW = "Captura; Reconstruida; Atlantico; Industrial; Subsistencia;Artesanal;Deportiva")


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


Final_SAU_Atlantic <- data.frame(a,Keywords_F,Timeframe_A,Observations_A$Data_Points)
#write.csv(Final_SAU_Atlantic, "Final_SAU_Atlantic.csv")
####################### END ########################

#### Prices ####
#http://www.economia-sniim.gob.mx/SNIIM-PESCA/Consolidados.asp

Price <- read.csv("./Data/Precios.csv")

Precio_d_Venta <- Price %>% 
  select(Spp_Origen,Por_Estado,Por_Punto) %>% 
  mutate(Inicio = "Precio de Venta de") %>% 
  mutate(Fin = "en el Origen (1998-2016)") %>% 
  mutate(P_Venta = paste(Inicio,Spp_Origen,Fin)) %>% 
  mutate(Inicio_M = "Precio de Menudeo de") %>% 
  mutate(Fin_M = "(1998-2016)") %>% 
  mutate(P_Menudeo = paste(Inicio_M,Spp_Origen,Fin_M)) %>%   mutate(Inicio_D = "Precio de") %>% 
  mutate(Fin_D = "en Destino de Venta (1998-2016)") %>% 
  mutate(P_Destino = paste(Inicio_D,Spp_Origen,Fin_D)) %>%
  mutate(Inicio_E = "Precio de Venta de Pescado en") %>% 
  mutate(P_Estado = paste(Inicio_E,Por_Estado,Fin_M)) %>% 
  mutate(Inicio_P = "Precio de Venta de Pescado en Punto de Cotizacion") %>% 
  mutate(P_Punto = paste(Inicio_P,Por_Punto,Fin_M)) %>% 
  select(P_Venta,P_Menudeo,P_Destino,P_Estado,P_Punto)

#write.csv(Precio_d_Venta, "Precio_d_Venta.csv")

####################### END ########################

# Atlas UNAM ####
UNAM <- read.csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Data/UNAM.csv") %>% 
  select(1:13)

UNAM_X <- UNAM %>% 
  mutate(SST_X = paste(SST,F_SST,"(2003-2012)")) %>% 
  mutate(Anomalia = paste(A_T,"en",F_A_T)) %>% 
  mutate(Clorofila_Conce = paste(Clorofila,F_C,"(2003-2012)")) %>% 
  mutate(Clorofila_Ano = paste(A_C,"en",A_F)) %>% 
  mutate(Vientos_X = paste(Vientos,Fecha,"(1999-2006)")) %>%
  mutate(Topografia_X = paste(Topografia,Fecha, "(1992-1999)")) %>% 
  mutate(Nivel_X = paste(Nivel,Fecha,"(1992-1999)")) %>% 
  mutate(Velocidad_x = paste(Velocidad,Fecha,"(1992-1999)"))
  
#write.csv(UNAM_X[18:21],"UNAM_MMID.csv")

####################### END ########################
# BALANZA COMERCIAL POR PRINCIPALES PRODUCTOS PESQUEROS, 2004 - 2013. Anuario Sagarpa Pg. 186

#Exportacion
Exp <- data.frame(matrix(c("Algas y Sargazos",
              "Calamar","Camaron",
              "Pulpo",
              "Sardina y Macarela",
              "Org. Acuats. Vivos"
              ),ncol=1))
colnames(Exp) <- ("Specie")

Exp <-Exp %>% 
  mutate(Volumen="Volumen de Exportacion de") %>% 
  mutate(Valor="Valor de Exportacion de") %>% 
  mutate(Year = "2004-2013") %>% 
  mutate(Vol.Final = paste(Volumen,Specie,Year)) %>% 
  mutate(Val.Final = paste(Valor,Specie,Year)) %>% 
  mutate(Key.Vol = paste("Volumen; Exportacion; Balanza Comercial;",Specie)) %>% 
  mutate(Key.Val = paste("Valor; Exportacion; Balanza Comercial;",Specie)) %>% 
  select(-2:-4)
  
  
#Importacion
Imp <- data.frame(matrix(c("Derivados de Algas",
                           "Calamar",
                           "Camaron",
                           "Salmon",
                           "Org. Acuats. Vivos"
),ncol=1))
colnames(Imp) <- ("Specie")

Imp <-Imp %>% 
  mutate(Volumen="Volumen de Importacion de") %>% 
  mutate(Valor="Valor de Importacion de") %>% 
  mutate(Year = "2004-2013") %>% 
  mutate(Vol.Final = paste(Volumen,Specie,Year)) %>% 
  mutate(Val.Final = paste(Valor,Specie,Year)) %>% 
  mutate(Key.Vol = paste("Volumen; Importacion; Balanza Comercial;",Specie)) %>% 
  mutate(Key.Val = paste("Valor; Importacion; Balanza Comercial;",Specie)) %>% 
  select(-2:-4)
  
Final <- bind_rows(Exp,Imp)
write.csv(Final,"Balanza.csv")

Disp <- data.frame(matrix(c("Tunidos (Fresco)",
                            "Mojarra",
                            "Camaron",
                            "Ostion",
                            "Tiburon y Cazon",
                            "Otros",
                            "Tunidos (Enlatado)",
                            "Sardinas",
                            "Harina y Aceite",
                            "Total"
                            
                                   
),ncol=1))
colnames(Disp) <- ("Specie")

Disp <-Disp %>% 
  mutate(Legend="Disponibilidad Interna de") %>% 
  mutate(Year = "1988-2013") %>% 
  mutate(Final = paste(Legend,Specie,",",Year)) %>% 
  mutate(Key = paste("Volumen; Disponibilidad; Interna; Fresco; Congelado; Enlatado; Reduccion;",Specie)) %>% 
  select(-Legend,-Year)

write.csv(Disp,"Disponibilidad.csv")

####################### END ########################

Embarcaciones <- read_csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Embarcaciones.csv")

Emb1 <- Embarcaciones %>% 
  mutate(Fin1= paste(Leyenda,Estado,Year)) %>%
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Total; Nacional; Pesca"))
  select(10,11)
Emb2 <- Embarcaciones %>% 
  filter(Leyenda2 !="na") %>% 
  mutate(Fin1= paste(Leyenda2,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca")) %>% 
  select(10,11)
Emb3 <- Embarcaciones %>% 
  filter(Leyenda3 !="na") %>% 
  mutate(Fin1= paste(Leyenda3,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca; Camaron")) %>% 
  select(10,11)
Emb4 <- Embarcaciones %>% 
  filter(Leyenda4 !="na") %>% 
  mutate(Fin1= paste(Leyenda4,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca; Atun; Tunidos")) %>% 
  select(10,11)
Emb5 <- Embarcaciones %>% 
  filter(Leyenda5 !="na") %>% 
  mutate(Fin1= paste(Leyenda5,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca; Sardina; Anchoveta")) %>%
  mutate(Subject = Estado) %>% 
  select(10,11,12)
Emb6 <- Embarcaciones %>% 
  filter(Leyenda6 !="na") %>% 
  mutate(Fin1= paste(Leyenda6,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca; Escama")) %>%
  mutate(Subject = Estado) %>% 
  select(10,11,12)

Emb7 <- Embarcaciones %>% 
  filter(Leyenda7 !="na") %>% 
  mutate(Fin1= paste(Leyenda7,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Rios; Estado; Pesca; Riberena")) %>%
  mutate(Subject = Estado) %>% 
  select(10,11,12)


Final <- bind_rows(Emb1,Emb2,Emb3,Emb4,Emb5,Emb6,Emb7)

  


  
  
