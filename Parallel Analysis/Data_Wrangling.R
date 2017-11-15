####
# Data wrangling script 
#The data used is added in the MMd as "Reconstructed" data, I added "Industria, artisanal, etc to keywords
####


#### NOTE Allways run this first ####
#library(xlsx)
library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(leaflet)
library(dataone)
library(rgdal)
library(tools)
library(ggplot2)
library(leaflet)
library(taxize) # For scientific names
library(stringr)
setwd("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis")

# source("./Functions/gbif_import.r")

#### _________________________________ ####

#### DATA ####

#### Template

Template <- read_csv("~/Documents/Github/Meta_Data_Mexico/App/Template_4.1.csv")
# View(Template)

#### Mexico's EEZ

# EEZ_Mex <- read.csv("./eez_Mex.csv")
# 
# # Get Mexico's Pacific EEZ
# eez_Mex_P <- EEZ_Mex %>% 
#   filter(piece == 1)
# 
# # Get Mexico's Atlantic EEZ (2)
# eez_Mex_A <- EEZ_Mex %>% 
#   filter(piece == 2)
#________________________________________________________#


### Spatial Analysis ####

source("./Functions/Politic_Distribution.r")

New_Temp <- Politic_Distribution(Template, Save = F)


Old <- Template %>% 
  group_by(Area) %>% 
  summarise(Old=n())

c <- New_Temp %>% 
  group_by(Area) %>% 
  summarise(New=n()) %>% 
  bind_cols(Old)

# Check TBD and Pacific data cus it went "under"
#Method, filter pacific in both fdatas and then use anti join to see which entries are not iqual ;) maybe not filter?

####



#### Sea Around Us Data, Pacific ####

# Title ###

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


# Number of observations per species ###
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



#Timeframe ###
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

# Title ###

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


# Number of observations per species ###


Observations_A <- SAU_A %>% 
  mutate(Value =1) %>% 
  group_by(common_name, scientific_name, year) %>% 
  summarise(top = sum(Value)) %>% 
  mutate(Value2 =1) %>% 
  group_by(common_name,scientific_name) %>% 
  summarise(Data_Points =sum(Value2))


#Timeframe ###
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
#### Prices SE ####
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
#### CONAPESCA ####

# ANUARIO CONAPESCA EMBARCACIONES REGISTRADAS POR PRINCIPALES PESQUERÍAS, SEGÚN LITORAL Y ENTIDAD FEDERATIVA, 2013 (UNIDADES)

Embarcaciones <- read.csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Embarcaciones.csv")

Emb1 <- Embarcaciones %>% 
  mutate(Fin1= paste(Leyenda,Estado,"2013")) %>%
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Total; Nacional; Pesca")) %>% 
  mutate(Subject = Estado) %>% 
  select(Fin1,Key,Subject)
Emb2 <- Embarcaciones %>% 
  mutate(Fin1= paste(Leyenda2,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca")) %>% 
  mutate(Subject = Estado) %>% 
  select(10,11,Subject)
Emb3 <- Embarcaciones %>% 
  filter(Leyenda3 !="na") %>% 
  mutate(Fin1= paste(Leyenda3,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca; Camaron")) %>% 
  mutate(Subject = Estado) %>% 
  select(10,11,Subject)
Emb4 <- Embarcaciones %>% 
  filter(Leyenda4 !="na") %>% 
  mutate(Fin1= paste(Leyenda4,Estado,Year))%>% 
  mutate(Key = paste(Estado, "Embarcaciones; Principales Pesqeurias; Altura; Estado; Pesca; Atun; Tunidos")) %>% 
  mutate(Subject = Estado) %>% 
  select(10,11,Subject)
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
#write.csv(Final, "Embarcaciones.csv")

#POBLACIÓN REGISTRADA EN LA CAPTURA Y ACUACULTURA,SEGÚN LITORAL Y ENTIDAD FEDERATIVA 2003-2013

Pescadores <- Embarcaciones %>% 
  select(Estado) %>% 
  arrange(Estado) %>% 
  mutate(Titulo="Poblacion Registrada en Captura y Acuacultura en") %>% 
  mutate(Fecha ="(2003-2013)") %>% 
  mutate(Fin = paste(Titulo,Estado,Fecha)) %>% 
  mutate(Key =paste("Poblacion; Acuacultura; Pesca; Captura; Pescador; Poblacion; Registro;",Estado))

#write.csv(Pescadores, "Pescadores.csv")


#FINANCIAMIENTO AL SECTOR PESQUERO POR FIRA-FOPESCA POR ENTIDAD FEDERATIVA 2004 - 2013.

FIRA <- Embarcaciones %>% 
  select(Estado) %>% 
  arrange(Estado) %>% 
  mutate(Titulo="Financiamiento al sector pesquero de") %>% 
  mutate(Fecha ="(2004-2013)") %>% 
  mutate(Fin = paste(Titulo,Estado,Fecha)) %>% 
  mutate(Key =paste("Sector pesquero; Pesca; Presupuesto; Financiamiento; FIRA;FOPESCA;",Estado))

write.csv(FIRA, "FIRA.csv")

#SERIE HISTORICA DE INVERSION EN EL PROYECTO DE RETIRO VOLUNTARIO DE EMBARCACIONES CAMARONERAS 2008 - 2013.

Retiro <- Embarcaciones %>% 
  select(Estado) %>% 
  arrange(Estado) %>% 
  filter(Estado =="Baja California"|
           Estado == "Campeche"|
           Estado == "Colima"|
           Estado == "Chiapas"|
           Estado == "Guerrero"|
           Estado == "Nayarit"|
           Estado == "Oaxaca"|
           Estado == "Quintana Roo"|
           Estado == "Sinaloa"|
           Estado == "Sonora"|
           Estado == "Tabasco"|
           Estado == "Tamaulipas"|
           Estado == "Veracruz"|
           Estado == "Yucatan") %>% 
  mutate(Titulo="Inversion en Retiro Voluntario de Embarcaciones Camaroneras de") %>% 
  mutate(Fecha ="(2008-2013)") %>% 
  mutate(Fin = paste(Titulo,Estado,Fecha)) %>% 
  mutate(Key =paste("Inversion; Retiro Voluntario; Camaron; Proyecto; Embarcacion;",Estado))

#write.csv(Retiro, "Retiro.csv")


#SERIE HISTORICA DE MONTOS RECAUDADOS POR PERMISOS DE PESCA DEPORTIVA SEGUN ENTIDAD FEDERATIVA 2008 - 2013.

Permisos <- Embarcaciones %>% 
  select(Estado) %>% 
  arrange(Estado) %>% 
  mutate(Titulo="Monto Recaudado por Premisos de Pesca Deportiva en") %>% 
  mutate(Fecha ="(2008-2013)") %>% 
  mutate(Fin = paste(Titulo,Estado,Fecha)) %>% 
  mutate(Key =paste("Monto; Total; Premisos; Pesca; Deportiva; Pesos;",Estado))

#write.csv(Permisos, "Permisos.csv")  


# Allocation of random numbers to the SE data

Lista <- c(3,3,3,3,3,3,3 # <- Random datapoints generaed before
           ,3
           ,3
           ,3
           ,3
           ,3
           ,3
           ,3
           ,3
           ,3
           ,6
           ,6
           ,6
           ,9
           ,9
           ,9
           ,9
           ,9
           ,9
           ,9
           ,9
           ,12
           ,12
           ,12
           ,12
           ,12
           ,12
           ,15
           ,15
           ,18
           ,18
           ,18
           ,18
           ,18
           ,21
           ,21
           ,21
           ,24
           ,27
           ,27
           ,27
           ,33
           ,33
           ,39
           ,42
           ,48
           ,48
           ,51
           ,54
           ,54
           ,54
           ,54
           ,54
           ,54
           ,54
           ,57)

SE <- data.frame(sample(Lista, #List of random numbers
                        426, #Number of entries to substitute
                        replace = TRUE)) # <- random allocation of the datapoints to all data (expcluding those that generatedd the random numbers)

#write.csv(SE,"SE.csv")


# BALANZA COMERCIAL ####
#POR PRINCIPALES PRODUCTOS PESQUEROS, 2004 - 2013. Anuario Sagarpa Pg. 186

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
#### NOMS ####
NOMS <- fread("./Parallel Analysis/NOMS.csv")

Nom <- NOMS %>% 
  mutate(Title= paste(Titulo,Comun,"(",Especie,")")) %>%
           mutate(key = paste("Nom; Veda; Temporal; SEMARNAT; Secretaria; Medio; Ambiente; Recursos; Naturales; Conservacion; Proteccion;",Comun,Especie))
  
write.csv(Nom, "NOM.csv")         

####################### END ########################
#### Paco's Catch Data from the past ####

Paco <- fread("./Paco.csv")

PacoC <- Paco %>% 
  mutate(Y = paste("(1956-2009)")) %>% 
  mutate(Titulo_C = paste(Titulo,Especie,"en",Fin,Y)) %>% 
  mutate(Key= paste(Key,Especie))
           
write.csv(PacoC,"Paco.csv")

####################### END ########################
### Catalogo Algas Marinas Vol I ####
O_Data <- fread("./Data/Catalogo_I.csv")

N_Data <- data.frame(Especie =c( 
                       "Valoniopsis hancockii",
                     "Valoniopsis pachynema",
                     "Bryopsis corticulans",
                     "Bryopsis galapagensis",
                     "Bryopsis hypnoides",
                     "Bryopsis muscosa",
                     "Bryopsis pennata (pennata)",
                     "Bryopsis pennata",
                     "Bryopsis plumosa",
                     "Derbesia hollenbergii",
                     "Derbesia lamourouxii",
                     "Derbesia marina",
                     "Derbesia prolifica",
                     "Derbesia tenuissima",
                     "Derbesia turbinata",
                     "Pseudobryopsis hainanensis",
                     "Caulerpa ambigua (ambiguia)",
                     "Caulerpa ambigua (luxurians)",
                     "Caulerpa arenicola",
                     "Caulerpa cupressoides (cupressoides)",
                     "Caulerpa cupressoides (lycopodium)",
                     "Caulerpa fastigiata",
                     "Caulerpa imbricata",
                     "Caulerpa mexicana",
                     "Caulerpa mexicana (pectinata)",
                     "Caulerpa peltata",
                     "Caulerpa racemosa",
                     "Caulerpa racemosa (racemosa)",
                     "Caulerpa racemosa (macrophysa)",
                     "Caulerpa racemosa (occidentalis)",
                     "Caulerpa racemosa (turninata)",
                     "Caulerpa sertularioides",
                     "Caulerpa sertularioides (brevipes)",
                     "Caulerpa sertularioides (longiseta)",
                     "Caulerpa vanbosseae",
                     "Caulerpa verticillata",
                     "Codium amplivesciculatum",
                     "Codium brandegeei",
                     "Codium dawsonii",
                     "Codium decorticatum",
                     "Codium dichotomum",
                     "Codium fragile",
                     "Codium giraffa",
                     "Codium hubbsii",
                     "Codium isabelae",
                     "Codium johnstonei",
                     "Codium latum",
                     "Codium picturatum",
                     "Codium schmiederi",
                     "Codium setchellii",
                     "Codium simulans",
                     "Halimeda cuneata",
                     "Halimeda discoidea",
                     "Halimeda hederacea",
                     "Halimeda opuntia",
                     "Halimeda scabra",
                     "Halimeda tuna",
                     "Ostreobium quekettii",
                     "Chlorodesmis caespitosa",
                     "Chlorodesmis hildebrandtii",
                     "Geppella decussata",
                     "Penicillus sibogae",
                     "Batophora oerstedii",
                     "Neomeris annulata",
                     "Neomeris van-bosseae",
                     "Acetabularia calyculus",
                     "Acetabularia crenulata",
                     "Acetabularia farlowii",
                     "Acetabularia parvula",
                     "Acetabularia pusilla",
                     "Pilinella californica",
                     "Pilinia maritima"
                     )
)

F_Data <- bind_rows(O_Data,
                    N_Data
                   ) %>% 
  select(Especie) %>% 
  mutate(InicioA ="Distribucion de") %>% 
  mutate(InicioB ="Primer registro ") %>% 
  mutate(Fin = "en la costa del Pacifico") %>% 
  mutate(KeyA ="Distribucion; Alga; Bentos; Bentonica; Catalogo; Chlorophycota") %>% 
  mutate(KeyB ="Primer Registro; Alga; Bentos; Bentonica; Catalogo; Chlorophycota") %>% 
  mutate(Registro_A =paste(InicioA,Especie,Fin)) %>% 
  mutate(Registro_B =paste(InicioB,Especie,Fin)) %>% 
  select(1,5:8)

#write.csv(F_Data,"Catalogo_I_Data.csv")


### Catalogo Algas Marinas Vol I (REMASTERIZADO) ####
O_Data <- fread("./Data/O_Data.csv")

F_Data <- O_Data %>% 
  select(1,2) %>% 
  mutate(Especie = paste(V1,V2)) %>% 
  group_by(Especie) %>% 
  summarise(N=n()) %>% 
  mutate(Nombre = paste("Distribucion de", Especie, "en la costa del Pacifico")) %>% 
  select(-2)
  

write.csv(F_Data,"O_Data.csv")

### Catalogo Algas Marinas Vol II ####

N_Data <- data.table(Especie =c(
  "Asteronema breviarticulatum",
  "Ectocarpus acutus",
  "Ectocarpus bryantii",
  "Ectocarpus chantransoides",
  "Ectocarpus commensalis",
  "Ectocarpus corticulatus",
  "Ectocarpus ensenadanus",
  "Ectocarpus gonodioide",
  "Ectocarpus hancockii",
  "Ectocarpus siliculosus (siliculosus)",
  "Ectocarpus siliculosus (confervoides)",
  "Ectocarpus siliculosus (dasycarpus)",
  "Ectocarpus siliculosus (pygmaeus)",
  "Ectocarpus siliculosus (subulatus)",
  "Ectocarpus simulans",
  "Ectocarpus sonorensis",
  "Feldmannia hemispherica",
  "Feldmannia indica",
  "Feldmannia irregularis",
  "Feldmannia simplex",
  "Hincksia granulosa",
  "Hincksia mitchelliae",
  "Hincksia rallsiae",
  "Hincksia sandriana",
  "Hincksia saundersii",
  "Kuetzingiella elachistaeformis",
  "Pilocladus codicola",
  "Spongonema tomentosum",
  "Streblonema investiens",
  "Streblonema penetrale",
  "Streblonema transfixum",
  "Bachelotia antillarum",
  "Diplura simulans",
  "Endoplura aurea",
  "Hapalospongidion gelatinosum",
  "Hapalospongidion pangoense",
  "Hapterophycus anastomosans",
  "Hapterophycus canaliculatus",
  "Petroderma maculiforme",
  "Pseudolithoderma nigrum",
  "Ralfsia californica",
  "Ralfsia confusa",
  "Ralfsia fungiformis",
  "Ralfsia hancockii",
  "Ralfsia hesperia",
  "Ralfsia integra",
  "Ralfsia pacifica",
  "Sphacelaria brevicornis",
  "Sphacelaria californica",
  "Sphacelaria divaricata",
  "Sphacelaria masonii",
  "Sphacelaria novae-hollandiae",
  "Sphacelaria rigidula",
  "Sphacelaria tribuloides",
  "Dictyopteris delicatula",
  "Dictyopteris johnstonei",
  "Dictyopteris polypodioides",
  "Dictyopteris repens",
  "Dictyopteris undulata",
  "Dictyota bartayresiana",
  "Dictyota binghamiae",
  "Dictyota cervicornis",
  "Dictyota ciliolata",
  "Dictyota concrescens",
  "Dictyota crenulata",
  "Dictyota dichotoma",
  "Dictyota divaricata",
  "Dictyota flabellata",
  "Dictyota friabilis",
  "Dictyota linearis",
  "Dictyota masonii",
  "Dictyota okamurae",
  "Dictyota pinnata",
  "Dictyota volubilis",
  "Lobophora variegata",
  "Pachydictyon coriaceum",
  "Padina caulescens",
  "Padina concrescens",
  "Padina crispata",
  "Padina durvillei",
  "Padina mexicana (mexicana)",
  "Padina mexicana (erecta)",
  "Padina ramonribae",
  "Padina sanctae-crucis",
  "Spatoglossum howellii",
  "Spatoglossum lanceolatum", 
  "Spatoglossum schroederi",
  "Spatoglossum subflabellatum",
  "Taonia lennebackerae",
  "Zonaria farlowii",
  "Cutleria cylindrica",
  "Cutleria hancockii",
  "Haplogloia andersonii",
  "Ishige sinicola",
  "Leathesia marina",
  "Leathesia nana",
  "Myriactula johnstonii",
  "Myriactula marchantiae",
  "Petrospongium rugosum",
  "Halothrix lumbricalis",
  "Hecatonema streblonematoides",
  "Nemacystus brandegeei",
  "Chnoospora implexa",
  "Chnoospora minima",
  "Chnoospora pannosa",
  "Colpomenia peregrina",
  "Colpomenia phaeodactyla",
  "Colpomenia ramosa",
  "Colpomenia sinuosa (sinuosa)",
  "Colpomenia sinuosa (expansa)",
  "Colpomenia tuberculata",
  "Compsonema immixtum",
  "Compsonema intricatum",
  "Compsonema secundum (secundum)",
  "Compsonema secundum (terminale)",
  "Compsonema serpens",
  "Hydroclathrus clathratus",
  "Petalonia binghamiae",
  "Petalonia fascia",
  "Rosenvingea antillarum",
  "Rosenvingea intricata",
  "Rosenvingea sanctae-crucis",
  "Scytosiphon dotyi",
  "Scytosiphon gracilis",
  "Scytosiphon lomentaria",
  "Stragularia clavata",
  "Coilodesme californica",
  "Coilodesme rigida",
  "Coilodesme rigida",
  "Asperococcus fistulosus",
  "Halorhipis winstonii",
  "Melanosiphon intestinalis",
  "Punctaria hesperia",
  "Soranthera ulvoidea",
  "Carpomitra costata",
  "Sporochnus bolleanus",
  "Sporochnus pedunculatus",
  "Desmarestia herbacea",
  "Desmarestia latifrons",
  "Desmarestia mexicana",
  "Desmarestia tabacoides",
  "Desmarestia viridis",
  "Egregia australis",
  "Egregia laevi",
  "Egregia menziesii",
  "Eisenia arborea",
  "Eisenia desmarestioides",
  "Pterygophora californica",
  "Undaria pinnatifida",
  "Agarum fimbriatum",
  "Laminaria farlowii",
  "Laminaria setchellii",
  "Dictyoneurum reticulatum",
  "Macrocystis angustifolia",
  "Macrocystis pyrifera",
  "Pelagophycus porra",
  "Cystoseira neglecta",
  "Cystoseira setchellii",
  "Halidrys dioica",
  "Stolonophora brandegeei",
  "Hesperophycus californicus",
  "Silvetia compressa",
  "Silvetia compressa",
  "Sargassum acinacifolium",
  "Sargassum agardhianum",
  "Sargassum brandegeei",
  "Sargassum filicinum",
  "Sargassum herporhizum",
  "Sargassum horridum",
  "Sargassum howellii",
  "Sargassum johnstonii",
  "Sargassum johnstonii (gracile)",
  "Sargassum macdougalii",
  "Sargassum muticum",
  "Sargassum pacificum (pacificum)",
  "Sargassum pacificum (megaphyllum)",
  "Sargassum palmeri",
  "Sargassum sinicola (sinicola)",
  "Sargassum sinicola (camouii)",
  "Sargassum sonorense",
  "Sargassum templetonii",
  "Sargassum vizcainense",
  "Sargassum vizcainense"
)
)

F_Data <- N_Data %>% 
  mutate(InicioA ="Distribucion de") %>% 
  mutate(InicioB ="Primer registro ") %>% 
  mutate(Fin = "en la costa del Pacifico") %>% 
  mutate(KeyA ="Distribucion; Alga; Bentos; Bentonica; Catalogo; Phaeophycota") %>% 
  mutate(KeyB ="Primer Registro; Alga; Bentos; Bentonica; Catalogo; Phaeophycota") %>% 
  mutate(Registro_A =paste(InicioA,Especie,Fin)) %>% 
  mutate(Registro_B =paste(InicioB,Especie,Fin)) %>% 
  select(1,5:8)

write.csv(F_Data,"Catalogo_II_Data.csv")

###################### END ########################

#### Macro Invertebrates ####

Brusca <- fread("./Data/Brusca.csv")

Inv <- Brusca %>% 
  mutate(TituloI = paste(Inicio,Genus,Species,Fin)) %>% 
  mutate(TituloII = paste(Inicio_II,Genus,Species)) %>% 
  mutate(Keywords = paste(Key,Common_synonyns)) %>% 
  mutate(Subject = paste(Genus,Species)) %>% 
  select(TituloI,TituloII,Keywords,Subject)

write.csv(Inv, "Inv_Brusca.csv")

###################### END ########################

#### Pedroche et al ####
#I had to reshape the Pedroche et al dataset from the .text file he sent. I will then add this new dataset with the Template

# First <- Read Pedroche's dataset
Data <- read_csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Data/Names&Dates.csv")

# Second <- Eliminate those entries that have no year record                
Clean_D <- Data %>% 
  filter(!is.na(Y1))

# Third <- Stack all years in one single column
Stacked_D <- stack(Clean_D[3:61])
colnames(Stacked_D) <- c("Start_Year","xf")
Stacked_D$Start_Year <- as.numeric(Stacked_D$Start_Year)

# Final Step <- Select only the starting year and create an Ending Year
Final_D <- Stacked_D %>% 
  select(Start_Year) %>% 
  mutate(End_Year=Start_Year)

#Just for checking

# source('~/Documents/Github/Meta_Data_Mexico/App_Eng/ts_fun.R')
# ts_subset(Final_D,
#           min(Final_D$Start_Year,na.rm = T),
#           max(Final_D$End_Year,na.rm = T))

#


write.csv(Final_D, "Pedroche_Hist.csv")




#### Rolando Batista 
#Template 1.9

Species <-(c("Macroalgae",
"Sponges",
"Cnidarians",
"Flatworms",
"Nemerteans",
"Annelids",
"Crustaceans",
"Mollusks",
"Bryozoans",
"Echinoderms",
"Fish",
"Amphibian and reptilian",
"Birds",
"Mammals"
))
Especies <-(c("Macroalgas",
             "Esponjas",
             "Cnidarios",
             "Plantelmintos",
             "Nematodos",
             "Anelidos",
             "Crustaceos",
             "Moluscos",
             "Bryozoos",
             "Equinodermos",
             "Peces",
             "Anfibios y Reptiles",
             "Pajaros",
             "Mamiferos"
))
Initial <- "Number of"
Final <- "in Oaxaca State"

Table <- data.table(
  Name = Species,
  Initial,
  Final
)

F_Table <- Table %>% 
  mutate(Title= paste(Initial,Species,Final)) %>% 
  mutate(Keywords = paste(Especies,"Oaxaca","Abundancia","Grupo taxonomico","Diversidad","Especies", sep="; ")) %>% 
  select(Title,
         Keywords,
         Name) 
  
#write.csv(F_Table, "F_Table.csv")
###################### END ########################

##### Andres ####
#Template 1.10
# No lo logre, por ahora...

Andres <- read.csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Data/Carajo.csv")
Andres$Estado <-as.character(as.factor(Andres$Estado))

A <- Andres %>% 
  select(1)

Ta <- A %>% 
  group_by(Estado) %>% 
  summarise(n=n())

y <- data.table(rep(MP$Estado[1], each=Ta$n[1]))
for(i in 1:nrow(Ta)){
  x <- data.table(rep(MP$Estado[i], each=Ta$n[i]))
  y <- bind_rows(y,x)
}

ys <- y %>% 
  group_by(V1) %>% 
  summarise(n=n())

NAs <- filter(yyy, is.na(V1))



yy <- data.table(rep(MP$Region[1], each=Ta$n[1]))
for(i in 2:nrow(Ta)){
  x <- data.table(rep(MP$Region[i], each=Ta$n[i]))
  yy <- bind_rows(yy,x)
}

yyy <- data.table(rep(MP$Area[1], each=Ta$n[1]))
for(i in 2:nrow(Ta)){
x <- data.table(rep(MP$Area[i], each=Ta$n[i]))
yyy <- bind_rows(yyy,x)
}

FIN <- bind_cols(y,yy,yyy)


#write.csv(FIN,"AndresFin.csv")

###################### END ########################
# GOb.mx info####

#Embarcaciones en zonas de ballenas

EmbB <- Emb %>% 
  group_by(ENTIDAD_FEDERATIVA) %>% 
  summarise(n=n())

EmbA <- Emb %>% 
  group_by(ENTIDAD_FEDERATIVA) %>% 
  summarise(n=n())

EmbF <- EmbB %>% 
  select(-2) %>% 
  mutate(paste("Numero de Embarcaciones en zonas con presencia de ballenas en",ENTIDAD_FEDERATIVA)) %>% 
  slice(-4) %>% 
  mutate(paste("Embarcaciones; Ballenas; Colision; Presencia;",ENTIDAD_FEDERATIVA))


write.csv(EmbF,"Embarcaciones.csv")
###################### END ########################
# COBI information for the Caribbeean ####
# March 26, 2017; La Paz, Mexico


###NOTE: I Notice a mistake on the dataset, species from the Pacific are catalogued as Atlantic... FIX IT!!!
### Explore Atlantic observations ###



Data_CONABIO_A <- Template %>% 
  filter(Subject_name %in% Especies_CONABIO$Cientifico) %>% 
  filter(Area == "Atlantic") %>% 
  filter(Institution != "COBI")

unique(Data_CONABIO_A$Subject_name)
length(unique(Data_CONABIO_A$Subject_name)) #41 spp con COBI y solo 28 sin COBI
unique(Data_CONABIO_A$Institution) ## 11 institutions

#### 

Cobi_Imp <- read_csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Cobi_Imp.csv")
# View(Cobi_Imp)

C_Atlantico <- Cobi_Imp %>% 
  filter(Region =="SAM") %>% 
  mutate(AbundanciaA = paste("Abundancia de", Especie, "en Punta Herrero, Sian Kaan")) %>% 
  mutate(AbundanciaB = paste("Abundancia de", Especie, "en Maria Elena, Sian Kaan")) %>% 
  mutate(AbundanciaC = paste("Abundancia de", Especie, "en Puerto Morelos")) %>% 
  mutate(AbundanciaD = paste("Abundancia de", Especie, "en Banco Chinchorro")) %>% 
  mutate(Key = paste(Comun,"Abundancia; Ausencia; Presencia; Quinta Roo; Yucatan; Monitoreo, Peces, Invertebrado")) %>% 
  mutate(TallaI = paste("Talla de", Especie, "en Punta Herrero, Sian Kaan")) %>% 
  mutate(TallaII = paste("Talla de", Especie, "en Maria Elena, Sian Kaan")) %>% 
  mutate(TallaIII = paste("Talla de", Especie, "en Puerto Morelos")) %>% 
  mutate(TallaIIII = paste("Talla de", Especie, "en Banco Chinchorro")) %>% 
  mutate(Key = paste(Comun,"Tallas; Quinta Roo; Yucatan; Monitoreo, Peces, Invertebrado")) %>% 
  gather("Esta","Title",4:12)


write.csv(C_Atlantico,
          "C_Atlantico.csv")

C_Pacifico <- Cobi_Imp %>% 
  filter(Region =="PBC") %>% 
  mutate(AbundanciaA = paste("Abundancia de", Especie, "en Isla Guadalupe, BC")) %>% 
  mutate(AbundanciaB = paste("Abundancia de", Especie, "en Rosario, BC")) %>% 
  mutate(AbundanciaC = paste("Abundancia de", Especie, "en Isla Magdalena, BCS")) %>% 
  mutate(AbundanciaD = paste("Abundancia de", Especie, "en Isla Natividad, BCS")) %>% 
  mutate(AbundanciaE = paste("Abundancia de", Especie, "en Loreto, BCS")) %>% 
  mutate(AbundanciaF = paste("Abundancia de", Especie, "en Ligui, BCS")) %>% 
  mutate(AbundanciaG = paste("Abundancia de", Especie, "en Loreto, BCS")) %>% 
  mutate(AbundanciaH = paste("Abundancia de", Especie, "en Cabo Pulmo, BCS")) %>% 
  mutate(AbundanciaI = paste("Abundancia de", Especie, "en Loreto, BCS")) %>% 
  mutate(AbundanciaJ = paste("Abundancia de", Especie, "en Isla Datil, SON")) %>% 
  mutate(AbundanciaK = paste("Abundancia de", Especie, "en Isla San Pedro Martir, SON")) %>% 
  mutate(AbundanciaL = paste("Abundancia de", Especie, "en Isla San Pedro Nolasco, SON")) %>% 
  mutate(AbundanciaM = paste("Abundancia de", Especie, "en Isla Datil, SON")) %>% 
  mutate(AbundanciaN = paste("Abundancia de", Especie, "en Bahia de Kino, SON")) %>% 
  mutate(AbundanciaO = paste("Abundancia de", Especie, "en Puerto Libertad, SON")) %>% 
  mutate(TallaA = paste("Talla de", Especie, "en Isla Guadalupe, BC")) %>% 
  mutate(TallaB = paste("Talla de", Especie, "en Rosario, BC")) %>% 
  mutate(TallaC = paste("Talla de", Especie, "en Isla Magdalena, BCS")) %>% 
  mutate(TallaD = paste("Talla de", Especie, "en Isla Natividad, BCS")) %>% 
  mutate(TallaE = paste("Talla de", Especie, "en Loreto, BCS")) %>% 
  mutate(TallaF = paste("Talla de", Especie, "en Ligui, BCS")) %>% 
  mutate(TallaG = paste("Talla de", Especie, "en Loreto, BCS")) %>% 
  mutate(TallaH = paste("Talla de", Especie, "en Cabo Pulmo, BCS")) %>% 
  mutate(TallaI = paste("Talla de", Especie, "en Loreto, BCS")) %>% 
  mutate(TallaJ = paste("Talla de", Especie, "en Isla Datil, SON")) %>% 
  mutate(TallaK = paste("Talla de", Especie, "en Isla San Pedro Martir, SON")) %>% 
  mutate(TallaL = paste("Talla de", Especie, "en Isla San Pedro Nolasco, SON")) %>% 
  mutate(TallaM = paste("Talla de", Especie, "en Isla Datil, SON")) %>% 
  mutate(TallaN = paste("Talla de", Especie, "en Bahia de Kino, SON")) %>% 
  mutate(TallaO = paste("Talla de", Especie, "en Puerto Libertad, SON")) %>% 
  mutate(Key_T = paste(Comun,"Tallas; Baja California; Baja California Sur; Sonora; Monitoreo, Peces, Invertebrados")) %>% 
  mutate(Key = paste(Comun,"Abundancia; Baja California; Baja California Sur; Sonora; Monitoreo, Peces, Invertebrados")) %>% 
  gather("Titulo", "Otro",4:33)
  

write.csv(C_Pacifico, "C_Pacifico.csv")



#### FIXING COBI ####

COBI_Old <- Template_4_1 %>% 
  filter(Institution == "COBI")


###################### END ########################
# Hector Reyes Labs ####
## Luis Hernandez data
# April 4, 2017; La Paz, Mexico

Luis <- read.csv("Data/Luis_In.csv")

Luis_Out <- Luis %>% 
  mutate(I_Pacifico = paste("Abundancia de",Especie,"en arrecifes del Pacifico")) %>% 
  mutate(K_Pacifico =paste("Arrecife; Abundancia; Presencia; Ausencia; Monitoreo;",Tipo)) %>% 
  mutate(I_Cozumel = paste("Abundancia de",Especie,"en Cozumel")) %>% 
  mutate(K_Cozumel =paste("Abundancia; Presencia; Ausencia; Monitoreo; Arrecife;",Tipo)) 

#write.csv(Luis_Out,"Luis_Out.csv")
  

## Violeta data
# April 4, 2017; La Paz, Mexico

V <- read.csv("Data/Violeta_In.csv")

LL <- paste(V$Localidades[1:31], sep = "")

V_Peces <- V %>% 
  select(Peces) %>% 
  mutate(I_P_A = paste("Abundancia de",Peces,"en el Golfo de California")) %>% 
  mutate(K_P_A =paste("Abundancia; Presencia; Ausencia; Monitoreo; PANGAS",Peces)) %>% 
  mutate(I_P_T = paste("Talla de",Peces,"en el Golfo de California")) %>% 
  mutate(K_P_T =paste("Tallas; Tamano; Monitoreo; PANGAS",Peces))

V_Inv <- V %>% 
  select(Invertebrados) %>% 
  mutate(I_I_A = paste("Abundancia de",Invertebrados,"en el Golfo de California")) %>% 
  mutate(K_I_A =paste("Abundancia; Presencia; Ausencia; Monitoreo; PANGAS",Invertebrados)) %>% 
  mutate(I_I_T = paste("Talla de",Invertebrados,"en el Golfo de California")) %>% 
  mutate(K_I_T =paste("Tallas; Tamano; Monitoreo; PANGAS",Invertebrados))
    
V_Out <- bind_cols(V_Peces,V_Inv)  
  

#write.csv(V_Out,"V_Out.csv")

## Omar Species data
# April 4, 2017; La Paz, Mexico

O_Spp_I <- read.csv("Data/Omar_In_I.csv")

#Esto resuelve el problema de los "cf."s
O_Spp <- read.csv("Data/Omar_In.csv")

O_cf <- O_Spp %>% 
  filter(X == "cf.") %>% 
  mutate(CF_Spp =paste(LIST,X,X.1)) %>% 
  select(CF_Spp)

colnames(O_cf) <- "Name"

# La base completa

O_Clean <- O_Spp_I %>% 
  select(1,2) %>% 
  filter(!is.na(Genero)) %>% 
  mutate(Name =paste(Genero,Especie)) %>% 
  filter(Especie != "cf.") %>% 
  bind_rows(O_cf) %>% 
  mutate(Inicio =paste("Presencia de",Name, "En Mexico")) %>% 
  mutate(Key =paste("Presencia; Diatomeas; Check list; Pacifico; Caribe; epilithic; epipsammic; epipelic; epiphytic; floristics; grazers; tychoplankton",Name))

#write.csv(O_Clean, "Omar_Out.csv")

# Diatomeas de Guerrero Negro

Data_G_Negro <- read.csv("Data/Omar_In_II.csv",
                         header = FALSE)

Negro_cf <- Data_G_Negro %>% 
  filter(V2 == "cf.") %>% 
  mutate(CF_Spp =paste(V1,V2,V3)) %>% 
  select(CF_Spp)

colnames(Negro_cf) <- "Name"

Negro_Clr <- Data_G_Negro %>% 
  filter(V1 != "Class") %>% 
  filter(V1 != "Family:") %>% 
  filter(V1 != "Order:") %>% 
  filter(V2 != "cf.") %>% 
  slice(-1) %>% 
  select(1,2) %>% 
  mutate(Name = paste(V1,V2)) %>% 
  bind_rows(Negro_cf) %>% 
  mutate(Inicio = paste("Presencia de",Name, "en la Laguna Guerrero Negro, BC-BCS")) %>% 
  mutate(Key = paste("Bacillariophyta; Biogeografia; Reserva de la Biosfera; Area protegida; Biodiversidad; Floristica de diatomeas; Laguna costera; Sistematica; Taxonomia",Name))

write.csv(Negro_Clr, "Omar_Out_II.csv")

# David Extrangero
D_Peces <- read.csv("Data/David_Spp_In.csv",
                         header = TRUE)

Table <- D_Peces %>% 
  mutate(Name =paste(Genero,Especie)) %>% 
  mutate(Inicio = paste("Presencia de",Name,"en el Golfo de California")) %>% 
  mutate(Key = paste("Abundancia; Peces; Vertebrados; Presencia; Ausencia")) %>% 
  mutate(Inicio_II = paste("Diversidad Funccional de",Name,"en el Golfo de California")) %>% 
  mutate(Key_II = paste("Nicho; Funcional; Redundancia; Peces; Vertebrados"))

#write.csv(Table, "David_Spp.csv")

###################### END ########################

#### CONAPESCA ####
#### Script para intentar analizar los datos de conapesca a nivel descrcagas ####
# Function "CON_Dat_Fun.r"
#Falta ahora incorporar la funcion de Tim (Go Tim!)

source("Functions/CON_Dat_Fun.r")
Data <- fread("Data/Plantas.csv",
                   header = TRUE
)

Conapesca_Data(Data)


###################### END ########################

#### Dataone ####
#dataone::downloadCert(CertificateManager())
#/DC=org/DC=cilogon/O=GitHub/CN=Juliano Palacios Abrantes A140271
help("dataone")

return <- "id,title, keywords, isPublic, author, contactOrganization,beginDate, endDate, scientificName, dataUrl"



cn <- CNode("PROD") # Usage form DataOne No need to change
# Ask for the id, title and abstract
queryParams <- list(q="keywords:Mexico",
                    fq="keywords:marine",
                    fl=return,
                    rows="6500"
                    ) 
result <- query(cn, 
                solrQuery=queryParams, 
                as="data.frame",
                parse=FALSE)


###################### END ########################
  #### Bertha L. ####

Berta2 <- read.csv("~/Desktop/Berta2.csv", header = FALSE)
View(Berta2)

Bertha <- Berta %>% 
  mutate(Title = paste("Abundancia de",Taxa,"en el Pacifico Norte")) %>% 
  mutate(Key = "Abundancia; IMECOCAL; zooplancton; grupo; funcional")

Bertha2 <- Berta2 %>% 
  mutate(Title = paste("Abundancia de",V1," en el Pacifico Norte (muestras nocturnas)")) %>% 
  mutate(Key = "Abundancia; IMECOCAL; zooplancton; euphausiid; muestras nocturnas; grupo; funcional; Ocurrencia")


Bertha3 <- Berta2 %>% 
  filter(Table_III != "Family") %>% 
  filter(Table_III != "rder") %>% 
  filter(Table_III != "amily") %>% 
  mutate(Taxa_II = paste(Table_II,Table_Il_B)) %>% 
  mutate(Taxa_III = paste(Table_III,Table_III_B)) %>% 
  mutate(Title_I = paste("Length-weight regressions for",Table_I)) %>% 
  mutate(Key_I = paste("estimado; carboon; cenizas; peso seco; prosome; largo; peso; hembra; copepodo; IMECOCAL;EL ninho; ENSO;")) %>% 
  mutate(Title_II = paste("Presence of",Taxa_II,"in the North Pacific (1997-1999)")) %>% 
  mutate(Key_II = paste("copepodo; IMECOCAL; noche; EL ninho; ENSO; Dominantes")) %>% 
  mutate(Title_III = paste("Presence of",Taxa_III,"in the North Pacific during El Nino and La Nina")) %>% 
  mutate(Key_III = paste("copepodo; IMECOCAL; ENSO; El Nino; La Nina; Presencia; Abundancia"))

Bertha5 <- Berta5 %>% 
  filter(Table_III != "Family") %>% 
  mutate(Taxa_I = paste(Table_I,Table_I_B)) %>% 
  mutate(Taxa_II = paste(Table_II,Table_II_B)) %>% 
  mutate(Taxa_III = paste(Table_III,Table_III_B)) %>% 
  mutate(Title_I = paste("Seasonal variation in population density of",Taxa_I,"in the North Pacific")) %>% 
  mutate(Key_I = paste("Hyperiid; amphipodo; Invierno; Primavera; Otono;IMECOCAL")) %>% 
  mutate(Title_II = paste("Correlation of",Taxa_II,"with salp species")) %>% 
  mutate(Key_II = paste("Hyperiid; amphipodo; Spearman; Salps;IMECOCAL; Cyclosalpa bakeri;Cyclosalpa danae; Salpa  fusiformis; Thalia cicar; Thalia democratica; Thalia orientalis; Weelia cylindrica")) %>% 
  mutate(Title_III = paste("Seasonal abundance of",Taxa_III,"in the North Pacific")) %>% 
  mutate(Key_III = paste("Hyperiid; amphipodo; Enero; Abril; Julio; Octubre; Abundancia; IMECOCAL"))


write.csv(Bertha5,"Bertha5.csv")

#### Limpeiza de Template ####

##### Duplicated Entries from Template 2.0 #####

# Lookig at the repeated ones...

Template <- Template %>% 
  filter(MMID >=1) %>% 
  filter(MMID <=33053)

Template_Explore <- Template %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())

Repeated_Template <-Template[duplicated(Template$Short_Title), ]
Repeated_Temp_Ag <- Repeated_Template %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())

# Remove repeated from dataset

No_Repeated_Template <-Template[!duplicated(Template$Short_Title), ]
#write.csv(No_Repeated_Template, "Clean_Template.csv")


# Now we need to clean the 2.2 Template starting from MMID 33053, oh man!!!!

Templateb <- TemplateB %>% 
  filter(MMID >=33053)

Templateb_Explore <- Templateb %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())


Repeated_Templateb <-Templateb[duplicated(Templateb$Short_Title), ]
Repeated_Tempb_Agb <- Repeated_Templateb %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())

# Remove repeated from dataset

No_Repeated_Templateb <-Templateb[!duplicated(Templateb$Short_Title), ]

Clean_Templateb_Explore <- No_Repeated_Templateb %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())

# Now we put together both cleaned datasets...
Cleaned_Template <- No_Repeated_Template %>% 
  bind_rows(No_Repeated_Templateb) %>% 
  select(-1)

# Just checking to make sure it worked...
Check_Duplication <-Cleaned_Template[duplicated(Cleaned_Template$Short_Title), ]


# NOTE #
#There are still a lot of repeated data (30000)  I think the whole dataset is duplicated. So now what I need to do is extract from the Template 2.0 the new entries I did from La Paz and Ensenada

# Making sure both datasets contain duplicate rows of the whole metadata...
Duplications <-Templateb_Explore %>% 
  anti_join(Template_Explore,
            by ="Compilation_Title")

# Yes they do...

# So, we need to exstract from TemplateB only the NEW entries
New_Entries <- TemplateB %>% 
  anti_join(Template,
            by ="Compilation_Title")

# Now we check for duplications on the New Entries

Repeated_New <-New_Entries[duplicated(New_Entries$Short_Title), ] #There are 3002 duplications...
No_Repeated_New <-New_Entries[!duplicated(New_Entries$Short_Title), ]

# Just checking witch databases are there...
No_Repeated_New_Explore <- No_Repeated_New %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())

#NOW we put them together...

Cleaned_Template <- No_Repeated_Template %>% 
  bind_rows(No_Repeated_New)

Cleaned_Template_Explore <- Cleaned_Template %>% 
  group_by(Institution,
           Compilation_Title) %>% 
  summarise(n=n())

# Again checking there are no duplications....

Repeated_Clean <-Cleaned_Template[duplicated(Cleaned_Template$Short_Title), ] #000000000

##### FINALLY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Lets just check all datasets are there...

#What datasets in TemplateB are NOT in Cleaned_Template
Dataset_Check <- Templateb_Explore %>% 
  anti_join(Cleaned_Template_Explore,
            by="Compilation_Title") # 0000000000000

#Now that we are sure, we export the dataset... 
write.csv(Cleaned_Template, "Template_3.csv")

#### END ####

#### UNINMAR ####

#Bremen ya fue...

### ICMyL ###

### Ecologia Bentos ###

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)

Sci_Name <- Datos %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`Scientific name`,
           `State/Province`,
           `Vernacular name`) %>% 
  summarise(Min=min(year),
            Max =max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Inicio =
    paste(
      "Presencia de",`Scientific name`,"en",`State/Province`
    )
  ) %>% 
  mutate(Inicio_I =
           paste(
             "Presencia de",`Vernacular name`,"en",`State/Province`
           )
  ) %>% 
  mutate(Key =
           paste(
             "Abundancia; Ecologia; Bentos; Historico;",`Vernacular name`
           )
  )


write.csv(Sci_Name, "Eco_Bentos.csv")


#

### 

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)


Mar_Profundo <- Datos %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`Vernacular name`,
           Kingdom,
           Phylum,
           Class,
           Order) %>% 
  summarise(Min=min(year),
            Max =max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Inicio_I =
           paste(
             "Presencia de",`Vernacular name`,"en el Golfo de Mexico"
           )
  ) %>% 
  mutate(Key =
           paste(
             "Mar Profundo; Especimen; Preservado; SIGSBEE; Justo Sierra; Nucleador; Multinucleador; Historico",Kingdom,Phylum,Class,Order, sep = "; "
           )
  ) 

write.csv(Mar_Profundo, "Mar_Profundo.csv")

####### FIN _____________________

# Coleccion Ictiologica

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)
Peces <- Datos %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           `Scientific name`,
           County) %>% 
  summarise(Min=min(year),
            Max =max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Titulo = paste("Especimen preservado de",`Scientific name`, "en", `State/Province`)) %>% 
  mutate(Key = paste("Peces; Ictiologia; Presencia",County,sep="; "))

write.csv(Peces, "Peces.csv")

#Check those entries with NA's

x <- c("Atherinella blackburni","Chaetodipterus faber","Eucinostomus harengulus","Melanocetus murrayi", "Notropis saladonis")

E <- Datos %>% 
  filter(`Scientific name` %in% x )

# Turns out there are records outside Mexico, they were removed.

####### FIN _____________________

# Coleccion de Moluscos

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)
Moluscos <- Datos %>% 
  filter(Country == "México") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           `Scientific name`,
           County) %>% 
  summarise(Inicio=min(year),
            Fin =max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Titulo = paste("Especimen preservado de",`Scientific name`, "en", `State/Province`)) %>% 
  mutate(Key = paste("Moluscos; Malacologia; Presencia",County,sep="; ")) %>% 
  mutate(State =paste(County)) %>% 
  arrange(`Scientific name`)


write.csv(Moluscos, "Moluscos.csv")

####### FIN _____________________

#Equinodermos

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)

Equinos <- Datos %>% 
  filter(Country == "México") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           `Scientific name`) %>% 
  summarise(Inicio=min(year),
            Fin =max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Titulo = paste("Especimen preservado de",`Scientific name`, "en", `State/Province`)) %>% 
  mutate(Key = paste("Equinodermos; Equinodermata; Presencia")) %>% 
  arrange(`Scientific name`)

Lista <- Equinos %>% 
  filter(`State/Province` =="")


Equinos_Localidades <- Datos %>% 
  filter(Country == "México") %>% 
  filter(`Scientific name` %in% Lista$`Scientific name`) %>% 
  filter(`State/Province` =="") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`Sea/Gulf`,
           `Scientific name`
           ) %>% 
  summarise(n())

write.csv(Equinos, "Equinos.csv")

####### FIN _____________________

#Porifera

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)

Pori <- Datos %>% 
  filter(Country == "México") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           `Scientific name`
           ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Titulo = paste("Especimen preservado de",`Scientific name`, "en", `State/Province`)) %>%
  mutate(Key = paste("Porifera; Especimen preservado; Presencia; Calcarea; Demospongiae; Hexactinellida"))



# write.csv(Pori, "Pori.csv")

####### FIN _____________________


#Campaás oceanograficas

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)

Oceano <- Datos %>% 
  filter(Country == "México") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           County
  ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Temp = paste("Temperatura Superficial del Agua en", County,`State/Province`)) %>%
  mutate(Temp_key = paste("CTD; temperatura; superficial; columna de agua; Justo Sierra; MOPEED")) %>%
  mutate(Sal = paste("Salinidad de columna de Agua en", County,`State/Province`)) %>%
  mutate(Sal_Key = paste("CTD; salinidad; partes por millon; superficial; columna de agua; Justo Sierra; MOPEED")) %>%
  mutate(Oxi = paste("Concentracion de Oxigeno disuelto en", County,`State/Province`)) %>%
  mutate(Oxi_Key = paste("CTD; oxigeno; superficial; columna de agua; Justo Sierra; MOPEED")) %>% 
  mutate(Key = paste("Sonda CTD; Especimen preservado; Presencia; Calcarea; Demospongiae; Hexactinellida; MOPEED"))

write.csv(Oceano, "Ocean.csv")
####### FIN _____________________

#Tulum

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)

Tulum <- Datos %>% 
  filter(Country == "México") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`Bay/Sound`,
           County
  ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Temp = paste("Presion atmosferica de",`Bay/Sound`, County)) %>%
  mutate(Temp_key = paste("Observatorios marinos; SETRA; Barometro; TULUM")) %>% 
  mutate(Temp = paste("Temperatura Superficial del Agua en",`Bay/Sound`, County)) %>%
  mutate(Temp_key = paste("Observatorios marinos; SETRA; Barometro; MODIS; NASA; AQUA; Color del Agua; SST; TULUM")) 

write.csv(Tulum, "Tulum.csv")
####### FIN _____________________

### Smitsonian ###

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)
#Hay algunos registros sin "county"í, para esos los organizamos por Estado/Provincia
Smithy_A <- Datos %>% 
  filter(Country == "México") %>% 
  filter(County == "") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           `Scientific name`
  ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Temp = paste("Especimen preservado de",`Scientific name`)) %>%
  mutate(Temp_key = paste("Smithsonian Institution; Equinodermos; Alcohol; Conserva")) %>% 
  rename(County = `State/Province`)

Smithy_B <- Datos %>% 
  filter(Country == "México") %>% 
  filter(County != "") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(County,
           `Scientific name`
  ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Temp = paste("Especimen preservado de",`Scientific name`)) %>%
  mutate(Temp_key = paste("Smithsonian Institution; Equinodermos; Alcohol; Conserva"))


Smithsonian <- bind_rows(Smithy_A,Smithy_B)

write.csv(Smithsonian, "Smithsonian.csv")

####### FIN _____________________

### ICMyL Mazatlan ###

# Copepoda ##

Datos <- read_delim("~/Downloads/uninmarResultados.csv",
                    ";",
                    escape_double = FALSE)
#Hay algunos pocos (15) sin "county", para esos los organizamos por Estado/Provincia
Copepodos_A <- Datos %>% 
  filter(Country == "México") %>% 
  filter(County == "") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(`State/Province`,
           `Scientific name`
  ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Temp = paste("Especimen preservado de",`Scientific name`)) %>%
  mutate(Temp_key = paste("Macho; Hembra; Alcohol; Merodo de colecta; Clasificacion; Copepodos; Disectado; Preservado Conserva")) %>% 
  rename(County = `State/Province`)

Copepodos_B <- Datos %>% 
  filter(Country == "México") %>% 
  filter(County != "") %>% 
  separate(`Event date`,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(County,
           `Scientific name`
  ) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year))) %>% # <- nos da los dp para cada registro
  mutate(Temp = paste("Especimen preservado de",`Scientific name`)) %>%
  mutate(Temp_key = paste("Macho; Hembra; Alcohol; Merodo de colecta; Clasificacion; Copepodos; Disectado; Preservado Conserva"))

Copepodos <- bind_rows(Copepodos_A,Copepodos_B)

# write.csv(Copepodos, "Copepodos.csv")

#### Fin Copepodos

#### Pollution ####

#Impossible to do... 

# FIN _____________________ ICMyL UNIMAR _____________________ ####

#### OBIS ####

# I downloaded obis data for all mexico so I need to subset by eez and within EEZ's. I'll use the point_in_poligon() fun for that

# Data. 
OBIS <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/OBIS/d4f53cc99ff68fbabb4c971b30fd3b39ba00c9db/d4f53cc99ff68fbabb4c971b30fd3b39ba00c9db.csv")

# Spatial analysis

# The path
path_eez_world <- ("/Users/jpalacios/Documents/Box Sync/UBC/Oceans_Project/Distribution/Data/Spatial/World_EEZ_v8_2014")
#The File
fnam_eez_world <- "World_EEZ_v8_2014_HR.shp"
#Load it!
eez_world <- readOGR(dsn = path_eez_world,
                     layer =file_path_sans_ext(fnam_eez_world))

#Fortify function to get a data.frame
fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f = fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

#### The actual EEZ ####

# Extract the EEZ for Mexico:
eez_p <- eez_world[eez_world@data$Country == "Mexico", ]

# Fortify the shapefile data:
eez_p <- fortify(eez_p)

# Get Mexico's Pacific EEZ
eez_Mex_P <- eez_p %>% 
  filter(piece == 1)

# Get Mexico's Atlantic EEZ (2)
eez_Mex_A <- eez_p %>% 
  filter(piece == 2)

# Get the points in polygon  
#Note <- 1 is present| 0 is ausent
OBIS_P <-data.frame(point.in.polygon(OBIS$decimalLongitude,
                                     OBIS$decimalLatitude,
                                     eez_Mex_P$long,
                                     eez_Mex_P$lat))

colnames(OBIS_P) <- "EEZ"

# First the Pacific data ####

# Add a column to Original Subseted Data with this information and then filter only those present:
OBIS_Pf <- OBIS %>% 
  bind_cols(OBIS_P) %>% 
  filter(EEZ == 1)  %>% 
  select(-EEZ) %>% 
  #Get ride of useless columns
  select(1:8) %>% 
  #Separate the date to know how many datapoints there are
  separate(eventDate,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(scientificName,
           institutionCode) %>% 
  summarise(Inicio = min(year),
            Fin = max(year),
            DP = length(unique(year)),
            Mean_Lat = mean(decimalLatitude),
            Mean_Long = mean(decimalLongitude)) %>%
  mutate(Title_P = paste("Records of",scientificName,"in the Pacific", sep=" ")) %>% 
  mutate(Key_P = paste("Presencia; Ausencia; Registro; Observacion"))

# write.csv(OBIS_Pf, "Obis_Final_Pacific.csv")

# Now the Atlantic ####

OBIS_A <-data.frame(point.in.polygon(OBIS$decimalLongitude,
                                     OBIS$decimalLatitude,
                                     eez_Mex_A$long,
                                     eez_Mex_A$lat))

colnames(OBIS_A) <- "EEZ"

OBIS_Af <- OBIS %>% 
  bind_cols(OBIS_A) %>% 
  filter(EEZ == 1)  %>% 
  select(-EEZ) %>% 
  #Get ride of useless columns
  select(1:8) %>% 
  #Separate the date to know how many datapoints there are
  separate(eventDate,
           c("year", "month", "day"), sep = "-") %>% 
  group_by(scientificName,
           institutionCode) %>% 
  summarise(Inicio = min(year,na.rm=T),
            Fin = max(year,na.rm=T),
            DP = length(unique(year,na.rm=T)),
            Mean_Lat = mean(decimalLatitude),
            Mean_Long = mean(decimalLongitude)) %>% 
  mutate(Title_P = paste("Records of",scientificName,"in the Atlantic", sep=" ")) %>% 
  mutate(Key_P = paste("Presencia; Ausencia; Registro; Observacion"))

# write.csv(OBIS_Af, "Obis_Final_Atlantic.csv")

# FIN _____________________ OBIS _____________________ ####

#### WCMC ####

# MPAS ####


WDPA <- fread("~/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/WDPA_July2017_MEX-csv/WDPA_July2017_MEX-csv.csv") %>% 
  filter(MARINE >= 1)

# Tipo de informacion existente en la base de datos:
# Tipo de area natural protegida (e.g Designacion) (DESIG) YA
# Categoria IUN (IUCN_CAT) YA
# (Rep_M_Area)
# (GIS_M_Area)
#(Rep_Area)
#(GIS_Area)
# Ano de creacion (Status_YR) (INCLUIR EN TODAS)
# Plan de Manejo (MANG_PLAN) YA
# Localidad (SUB_LOC) * voy a tener de desglosar eso (INCLUIR EN TODAS)
# Tipo de file (TYPE) (INCLUIR EN TODAS)

# For global numbers

Glob <- WDPA %>% 
  group_by(NAME) %>% 
  summarise(n())

View(Glob)

# For single areas

WDPA_I <- WDPA %>% 
  mutate(Title_I = paste("Nivel de proteccion del AMP ", NAME, sep="")) %>% 
  mutate(Key_I = paste("AMP; Area Natural Protegida; CONANP ",DESIG, sep=";")) %>% 
  mutate(Title_II = paste("Cateogria IUCN del AMP ", NAME, sep="")) %>% 
  mutate(Key_II = paste("AMP; Area Natural Protegida; CONANP; IUCN ",IUCN_CAT, DESIG, sep=";")) %>% 
  mutate(Title_III = paste("Plan de Manejo del AMP ", NAME, sep="")) %>% 
  mutate(Key_III = paste("AMP; Area Natural Protegida; CONANP; Poan de Manejo", sep=";")) %>% 
  mutate(Title_VI = paste("Area del AMP ", NAME, sep="")) %>% 
  mutate(Key_VI = paste("AMP; Area Natural Protegida; CONANP; tamano;, area", sep=";")) %>% 
  select(TYPE,
         NAME,
         STATUS_YR,
         MANG_PLAN,
         30:37,
         SUB_LOC)

write.csv(WDPA_I,"WDPA_I.csv")


# FIN WDPA, maps ###

## Cold Corals ####

path_cold_corals <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/Cold_Corals/01_Data")
#The File
fnam_cold_corals <- "WCMC001_ColdCorals_pt_v3.shp"
#Load it!
cold_corals <- readOGR(dsn = path_cold_corals,
                     layer =file_path_sans_ext(fnam_cold_corals))

#### Checkpoint (WORKS)

# leaflet(cold_corals) %>%
#   addTiles(
#     urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
#     attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
#   ) %>%
#   setView(lng = -90, lat = 1.3, zoom = 2) %>% 
#   addMarkers(
#     lng = ~START_LONG,
#     lat= ~START_LATI
  # )

##_____________All_es_gut______________##


#Fortify function to get a data.frame
fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f = fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

#### Mexico's EEZ

EEZ_Mex <- read.csv("./eez_Mex.csv")


# Get the points in polygon  
#Note <- 1 is present| 0 is ausent
Points_Cold_Corals <-data.frame(
  point.in.polygon(
    cold_corals$START_LONG,
    cold_corals$START_LATI,
    EEZ_Mex$long,
    EEZ_Mex$lat
    )
  )


# Fortify the shapefile data:
# Note: because is a SpatioalPointsDataFrame you have to tell R that you want to convert the Data
cold_corals_fort <- fortify(cold_corals@data) 

colnames(Points_Cold_Corals) <- "EEZ"

Cold_Corals_Mx <- cold_corals_fort %>% 
  bind_cols(Points_Cold_Corals) %>% 
  filter(EEZ == 1)  %>% 
  select(-EEZ)


Cold_Corals_Final <- Cold_Corals_Mx %>% 
  separate(START_DATE,
           c("year", "month", "day"), sep = "/") %>% 
  group_by(TAXON,
           REGION,
           PLACE_NAME,
           DATA_OWNER,
           ORDER_
           ) %>% 
  summarise(
    Numero = n(),
    Inicio = min(year,na.rm=T),
    Fin = max(year,na.rm=T),
    DP = length(unique(year,na.rm=T)),
    Mean_Lat = mean(START_LATI),
    Mean_Long = mean(START_LONG)
    ) %>% 
  mutate(Title = paste("Presence of",TAXON,"in the",REGION)) %>% 
  mutate(Key = paste("Corales; Agua Fria; Distribucion Mundial; Ocurrencia; presencia; ausencia; mar profundo; benticos",ORDER_, sep="; "))

# write.csv(
#   Cold_Corals_Final,
#   "Cold_Corals_Final.csv"
#   )

#fin Cold Corals

#### 

## Salt Marshes ####

path_marsh <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/Salt_Marsh/01_Data")
#The File
fnam_marsh <- "14_001_WCMC027_Saltmarsh_pt_v4.shp"
#Load it!
salt_marsh <- readOGR(dsn = path_marsh,
                       layer =file_path_sans_ext(fnam_marsh))

#Fortify function to get a data.frame
fortify.shape <- function(x){
  x@data$id <- rownames(x@data)
  x.f = fortify(x, region = "id")
  x.join <- inner_join(x.f, x@data, by = "id")
}

# Para este es mas facil simplemente extraer los datos para Mexico
Salt_Mx <- salt_marsh[salt_marsh@data$COUNTRY == "Mexico",]

# Fortify the shapefile data:
# Note: because is a SpatioalPointsDataFrame you have to tell R that you want to convert the Data
Salt_Marsh_fort <- fortify(Salt_Mx@data) 

Salt_Marsh_Final <- Salt_Marsh_fort %>% 
  mutate(Title = paste("Ocurrencia y Area reportada de marismas en la",REGION)) %>% 
  mutate(Key = paste("RAMSAR; Marismas; Costero",HABITAT, sep="; "))

write.csv(
  Salt_Marsh_Final,
  "Salt_Marsh_Final.csv"
  )

#fin Sal Marsh

#### Seagrass ####
path_Seagrass <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/Seagrass/01_Data")
#The File
fnam_Seagrass <- "WCMC_013_014_SeagrassesPt_v4.shp"
#Load it!
Seagrass <- readOGR(dsn = path_Seagrass,
                      layer =file_path_sans_ext(fnam_Seagrass))

Seagrass_fort <- fortify(Seagrass@data) 

Seagrass_Mx <- Seagrass_fort %>% 
  filter(PARENT_ISO == "MEX") %>% 
  mutate(Title = paste("Localizacion de", BIO_CLASS, "en Baja California")) %>% 
  mutate(TitleI = paste("Localizacion de", BIO_CLASS, "en Baja California Sur")) %>% 
  mutate(Titleq = paste("Localizacion de", BIO_CLASS, "en Sinaloa")) %>% 
  mutate(Titleqq = paste("Localizacion de", BIO_CLASS, "en Sonora")) %>% 
  mutate(Titleqqq = paste("Localizacion de", BIO_CLASS, "en Tamaulipas")) %>%
  mutate(Titleqqqq = paste("Localizacion de", BIO_CLASS, "en Veracruz")) %>%
  mutate(Titleqqqf = paste("Localizacion de", BIO_CLASS, "en Tabasco")) %>%
  mutate(Titleqqsq = paste("Localizacion de", BIO_CLASS, "en Campeche")) %>%
  mutate(Titleqqqs = paste("Localizacion de", BIO_CLASS, "en Yucatan")) %>% 
  mutate(Titleqqqff = paste("Localizacion de", BIO_CLASS, "en Quintana Roo"))


write.csv(Seagrass_Mx,"Seagrass_Mx_fin.csv")


## FIN SEAGRASS #

#### BioDiv ####


path_Reefs <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/DownloadPack-14_001_WCMC019_PatternsBiodiversity2010_v1/01_Data")
#The File
fnam_Reefs <- "WCMC-019-PatternsBiodiversity2010-AcrossTaxa.shp"
#Load it!
Reefs <- readOGR(dsn = path_Reefs,
                    layer =file_path_sans_ext(fnam_Reefs))

Reef_fort <- fortify(Reefs) 

 #Pacific #
Mex_Biodiv <-data.frame(point.in.polygon(Reefs$X_COORD,
                                     Reefs$Y_COORD,
                                     EEZ_Mex$long,
                                     EEZ_Mex$lat))


P_Biodiv <- Reef_fort %>% 
  bind_cols(Biodiv_P)





Reef_fort2 <- Reef_fort%>% 
  mutate(Longitude = long/100000) %>% 
  mutate(Latitude = lat/100000)

ggplot() + 
  geom_path(data = filter(Reef_fort2, piece ==1), 
            aes(x = Longitude, y = Latitude, group = group), 
            color = "black",
            size = .5) + 
  coord_map(projection = "mercator")+
  theme_classic() 
  


Reefs_fort <- fortify(Reefs_Mex) 

#### Estuarios ####

path_Estuarios <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/DownloadPack-14_001_UBC003_SAU_Estuaries2003_v2/01_Data")
#The File
fnam_Estuarios <- "14_001_UBC003_SAU_Estuaries2003_v2.shp"
#Load it!
Estuarios <- readOGR(dsn = path_Estuarios,
                    layer =file_path_sans_ext(fnam_Estuarios))

Estuarios_fort <- fortify(Estuarios@data) 

Estu_Mex1 <- Estuarios_fort %>% 
  filter(COUNTRY == "MEX") %>% 
  filter(!is.na(RIVER_SYS)) %>% 
  mutate(Titulo = paste("Descarga del Rio",RIVER_SYS))

Estu_Mex2 <- Estuarios_fort %>% 
  filter(COUNTRY == "MEX") %>% 
  filter(is.na(RIVER_SYS)) %>% 
  mutate(Titulo = paste("Descarga del Rio",LABEL))

Estu_Mex <- Estu_Mex1 %>% 
  bind_rows(Estu_Mex2)

write.csv(Estu_Mex, "Estu_Mex.csv")



  leaflet(Estu_Mex) %>%
  addTiles(
    urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
  ) %>%
  setView(lng = -90, lat = 1.3, zoom = 2) %>%
  addMarkers(
    lng = ~INPUT_LON,
    lat= ~INPUT_LAT,
    popup = ~LABEL
)
  
#### Turismo ####
  
  path_Buceo <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/DownloadPack_14_001_WCMC030_DiveCentres2001_v1_2/01_Data")
  #The File
  fnam_Buceo <- "WCMC-030-DiveCentres2001-ver1-2.shp"
  #Load it!
  Buceo <- readOGR(dsn = path_Buceo,
                       layer =file_path_sans_ext(fnam_Buceo))
  
  Buceo_fort <- fortify(Buceo@data) 
  
  Estu_Mex1 <- Buceo_fort %>% 
    filter(COUNTRY == "MEX") %>% 
    filter(!is.na(RIVER_SYS)) %>% 
    mutate(Titulo = paste("Descarga del Rio",RIVER_SYS))
  
  Estu_Mex2 <- Buceo_fort %>% 
    filter(COUNTRY == "MEX") %>% 
    filter(is.na(RIVER_SYS)) %>% 
    mutate(Titulo = paste("Descarga del Rio",LABEL))
  
  Estu_Mex <- Estu_Mex1 %>% 
    bind_rows(Estu_Mex2)
  
  write.csv(Estu_Mex, "Estu_Mex.csv")
  
  
  
  leaflet(Estu_Mex) %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -90, lat = 1.3, zoom = 2) %>%
    addMarkers(
      lng = ~INPUT_LON,
      lat= ~INPUT_LAT,
      popup = ~LABEL
    ) 
  
  
  #### A ver... ####
  
  path_Buceo <- ("/Users/jpalacios/Documents/Dropbox/Metadata_Mexico/Datasets/WCMC/DownloadPack-WCMC015_SeagrassRichness2003_v1")
  #The File
  fnam_Buceo <- "DownloadPack_14_001_WCMC030_DiveCentres2001_v1_2"
  #Load it!
  Buceo <- readOGR(dsn = path_Buceo,
                   layer =file_path_sans_ext(fnam_Buceo))
  
  Buceo_fort <- fortify(Buceo@data) 
  
  Estu_Mex1 <- Buceo_fort %>% 
    filter(COUNTRY == "MEX") %>% 
    filter(!is.na(RIVER_SYS)) %>% 
    mutate(Titulo = paste("Descarga del Rio",RIVER_SYS))
  
  Estu_Mex2 <- Buceo_fort %>% 
    filter(COUNTRY == "MEX") %>% 
    filter(is.na(RIVER_SYS)) %>% 
    mutate(Titulo = paste("Descarga del Rio",LABEL))
  
  Estu_Mex <- Estu_Mex1 %>% 
    bind_rows(Estu_Mex2)
  
  write.csv(Estu_Mex, "Estu_Mex.csv")
  
  
  
  leaflet(Estu_Mex) %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -90, lat = 1.3, zoom = 2) %>%
    addMarkers(
      lng = ~INPUT_LON,
      lat= ~INPUT_LAT,
      popup = ~LABEL
    ) 
  
  # FIN _____________________ WCMC UNEP _____________________ ####
  
#### _____________________CARTA NACIONAL PESQUERA ####
  
  
Species <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/CartaNac.Pesq/Especies_CNP.csv",
                    col_names = TRUE)
View(Species)

#### Correccion Nombres scientificos ####

# CNP_Correct <- gnr_resolve(names = Species$Cientifico, #Looks for homogenic names 
#                             best_match_only = TRUE) %>% # Returns only the best match 
#   rename(Cientifico = user_supplied_name)
# 
# # Searching for completeley wrong names
# Names_Missing <- Species %>% 
#   anti_join(CNP_Correct,
#             by ="Cientifico")
# 
# View(Names_Missing)
# 
# Species_Rev <- Species %>% 
#   left_join(CNP_Correct,
#             by ="Cientifico") %>% 
#   select(-4,-13,-15,-16) %>% 
#   rename(Cientifico = matched_name) %>% 
#   select(1:3,12,4:11)
# 
# write.csv(Species_Rev,
#           "Especies_CNP.csv")

#### Arreglar nombres ####

# Nombres <- Especies_CNP %>% 
#   filter(Litoral =="Pacifico") %>% 
#   group_by(Scientifico,
#            Animal
#   ) %>% 
#   summarise(n=n())
# 
# Duplicados <- Nombres[duplicated(Nombres$Scientifico),]


# Construir la tabla ####
  
  # Titulos 

# 1. Generalidades
Generalidades <- paste("Especies objetivo y asociadas a la pesca de") # <- General, no por especie
Generalidades_Key <- paste("Nombre Comun; Nombre Cientifico") # <- por especie objetivo
Zona_Captura_Ob. <- paste ("Zona de captura de") # <- Para especies objetivo
Zona_Captura_As. <- paste ("Zona de captura de") # <- Para especies objetivo
Zona_Captura_As_II. <- paste("asociada a la pesca de")
Unidad <- paste("Unidad de Pesca de")

# 2. Indicadores
Indicadores <- paste("Indicadores de la pesca de") # <- Nombre de especie objetivo (Categoria)

# 3. Esfuerzo Pesquero 
Esfuerzo <- paste("Esfuerzo pesquero para") # <- # Nombre de especie objetivo (Categoria)

# 4. Lineamientos y estrrategias de manejo
Estrategia <- paste("Estrategia de manejo para") # Nombre de especie objetivo

# Extra. Pesca Incidental
Incidental <- paste("Pesquerias con captura incidental de")
Asociada <- paste("Pesquerias con captura asociada de")


#### Creacion de la Tabla ####

# 1. Generalidades  
x_Generalidades <- Species %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(CNP ==2010) %>% 
  group_by(Animal) %>% 
  summarise(n()) %>% 
  mutate(General = paste(Generalidades, Animal)) %>% 
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(General,
         Dataset)

write.csv(x_Generalidades,
      "x_Generalidades.csv")



###

x_Zona_Cap_Ob <- Species %>% 
  filter(Litoral == "Pacifico") %>%
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Objetivo") %>% 
  mutate(Zona_Captura_Ob = paste(Zona_Captura_Ob.,Cientifico)) %>% 
  mutate(ZCO_key = paste(Animal,Comun, "Litoral; Area de pesca; Zona de Captura; Longitud; Latitud",
                         sep ="; ")) %>% 
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Zona_Captura_Ob,
         ZCO_key,
         Cientifico,
         Dataset)

write.csv(x_Zona_Cap_Ob,
          "x_Zona_Cap_Ob.csv")

###

x_Zona_Cap_As <- Species %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(CNP ==2010) %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Zona_Captura_As = paste(Zona_Captura_As.,Cientifico,Zona_Captura_As_II.,Animal,"(",Info_Extra,")",
                                 sep=" ")) %>% 
  mutate(ZCA_key = paste(Animal,Comun, "Litoral; Area de pesca; Zona de Captura; Longitud; Latitud",
                         sep ="; ")) %>% 
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Zona_Captura_As,
         ZCA_key,
         Cientifico,
         Dataset
         )

write.csv(x_Zona_Cap_As,
          "x_Zona_Cap_As.csv")

###

x_Unidad <- Species %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(Clasificacion == "Objetivo") %>% 
  filter(CNP ==2010) %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Unidad = paste(Unidad,Cientifico)) %>% 
  mutate(Unidad_Key = paste(Comun,Animal,"Embarcacion; motor; fuera de borda; buceo; hooka; sacos; artes de pesca; trampas; viaje de pesca; palangre; anzuelo",
                            sep ="; ")) %>% 
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Unidad,
         Unidad_Key,
         Cientifico,
         Dataset)

write.csv(x_Unidad,
          "x_Unidad.csv")

## FIN 1 ###

x_Indicadores <- Species %>% 
  filter(Litoral == "Pacifico" & 
           Clasificacion == "Objetivo") %>% 
  filter(CNP ==2010) %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Indicadores = paste(Indicadores,Cientifico)) %>% 
  mutate(Indicadores_Key = paste(Comun, Animal, "CPUE; Captura; Esfuerzo; Produccion; composicion especifica; Biomasa estimada",
                                 sep ="; ")) %>% 
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Indicadores,
         Indicadores_Key,
         Cientifico,
         Dataset)

write.csv(x_Indicadores,
          "x_Indicadores.csv")

## FIN 2 ##

x_Esfuerzo <- Species %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(Clasificacion == "Objetivo") %>% 
  filter(CNP ==2010) %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Esfuerzo = paste(Esfuerzo, Cientifico)) %>% 
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  mutate(Esfuerzo_Key = paste(Comun, Animal, "CPUE; Captura; Esfuerzo; Permisos; Pangas; Veda; Embarcaciones; Flota",
                                 sep ="; ")) %>%
  select(Esfuerzo,
         Esfuerzo_Key,
         Cientifico,
         Dataset)
  
write.csv(x_Esfuerzo,
          "x_Esfuerzo.csv")
  
### FIN 3 ###

x_Estrategia <- Species %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Objetivo") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Estrategia = paste(Estrategia, Cientifico)) %>% 
  mutate(Estrategia_Key = paste(Comun, Animal,"Permisos; Veda; Flota; Zonas de Pesca; Talla minima; Quota; ",
                              sep ="; ")) %>%
  mutate(Dataset = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Estrategia,
         Estrategia_Key,
         Cientifico,
         Dataset)
  

write.csv(x_Estrategia, "x_Estrategia.csv")

### FIN 4 ###


# Incidental y Asociada ####

Spp_Incidental <- Species %>% 
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Incidental") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Titulo_I = paste(Incidental,Cientifico)) %>% 
  mutate(Dataset_I = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Titulo_I,
         Dataset_I,
         Cientifico)

Key_I <- Species %>% 
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Incidental") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico) %>% 
  summarise_each(funs(paste(., collapse="; "))) %>% 
  mutate(Key = paste("Nombre Cientifico; comun; bycatch",Animal)) %>% 
  select(Key)
  

Spp_Asociada <- Species %>% 
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico) %>% 
  summarise(n()) %>% 
  mutate(Titulo_A = paste(Asociada,Cientifico)) %>% 
  select(Cientifico,Titulo_A)
  

Key_A <- Species %>% 
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico) %>% 
  summarise_each(funs(paste(., collapse="; "))) %>% 
  mutate(Key = paste("Nombre Cientifico; comun; bycatch",Animal)) %>% 
  select(Key)


NComunes <- Species %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico,
           Animal) %>% 
  summarise(n()) %>% 
  semi_join(Spp_Asociada,
            by="Cientifico")

write.csv(Spp_Incidental, "Spp_Incidental.csv")

write.csv(Spp_Asociada,"Spp_Asociada.csv")
  

### FIN Insident. Asociad. ###


# Revisar duplicados ####

Especies_CNP <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/CartaNac.Pesq/CNP_II.csv")

Repeated_CNP <-Especies_CNP[duplicated(Especies_CNP$Short_Title), ]#Todas parecen ser de Escama cuando objetivo.

lista <- c("Epinephelus niphobles",
"Paralabrax nebulifer",
"Semicossyphus pulcher",
"Sphyraena ensis",
"Mustelus californicus",
"Mustelus lunulatus",
"Mustelus henlei",
"Paralabrax auroguttatus",
"Micropogonias megalops",
"Cynoscion othonopterus",
"Paralabrax maculatofasciatus",
"Centropomus medius",
"Carcharhinus falciformis")

x <- Repeated_CNP %>% 
  filter(!Subject_name %in% lista)

### OJO: ALL REPEATED ARE FOR SHARKS IN EL ITSMO

####_____ CNP Atlantico ####

Species_Rev <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/CartaNac.Pesq/Especies_CNP.csv",
                                         col_names = TRUE)

# Explorando nombres #

#### Creacion de la Tabla ####

# 1. Generalidades  
x_Generalidades <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>% 
  group_by(Animal) %>% 
  summarise(n()) %>% 
  mutate(General = paste(Generalidades, Animal)) %>% 
  mutate(Dataset = paste("III. Pesquerias Marinas Y Costeras (a. Litoral del Atlantico),",Animal)) %>% 
  select(General,
         Animal,
         Dataset)

write.csv(x_Generalidades,
          "x_Generalidades.csv")



###

x_Zona_Cap_Ob <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>%
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  filter(Clasificacion == "Objetivo") %>% 
  mutate(Zona_Captura_Ob = paste(Zona_Captura_Ob.,Cientifico)) %>% 
  mutate(ZCO_key = paste(Animal,Comun, "Litoral; Area de pesca; Zona de Captura; Longitud; Latitud",
                         sep ="; ")) %>% 
  mutate(Dataset = paste("III. Pesquerias Marinas Y Costeras (b. Litoral del Golfo de México y Mar Caribe),",Animal)) %>% 
  select(Zona_Captura_Ob,
         ZCO_key,
         Cientifico,
         Animal,
         Dataset)

write.csv(x_Zona_Cap_Ob,
          "x_Zona_Cap_Ob.csv")

###

x_Zona_Cap_As <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Zona_Captura_As = paste(Zona_Captura_As.,Cientifico,Zona_Captura_As_II.,Animal,"(",Info_Extra,")",
                                 sep=" ")) %>% 
  mutate(ZCA_key = paste(Animal,Comun, "Litoral; Area de pesca; Zona de Captura; Longitud; Latitud",
                         sep ="; ")) %>% 
  mutate(Dataset = paste("III. Pesquerias Marinas Y Costeras (b. Litoral del Golfo de México y Mar Caribe),",Animal)) %>% 
  select(Zona_Captura_As,
         ZCA_key,
         Cientifico,
         Animal,
         Dataset
  )

write.csv(x_Zona_Cap_As,
          "x_Zona_Cap_As.csv")

###

x_Unidad <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Objetivo") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Unidad = paste(Unidad,Cientifico)) %>% 
  mutate(Unidad_Key = paste(Comun,Animal,"Embarcacion; motor; fuera de borda; buceo; hooka; sacos; artes de pesca; trampas; viaje de pesca; palangre; anzuelo",
                            sep ="; ")) %>% 
  mutate(Dataset = paste("III. Pesquerias Marinas Y Costeras (b. Litoral del Golfo de México y Mar Caribe),",Animal)) %>% 
  select(Unidad,
         Unidad_Key,
         Cientifico,
         Animal,
         Dataset)

write.csv(x_Unidad,
          "x_Unidad.csv")

## FIN 1 ###

x_Indicadores <- Species_Rev %>% 
  filter(Litoral == "Atlantico" & 
           Clasificacion == "Objetivo") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Indicadores = paste(Indicadores,Cientifico)) %>% 
  mutate(Indicadores_Key = paste(Comun, Animal, "CPUE; Captura; Esfuerzo; Produccion; composicion especifica; Biomasa estimada",
                                 sep ="; ")) %>% 
  select(Indicadores,
         Indicadores_Key,
         Cientifico,
         Animal)

write.csv(x_Indicadores,
          "x_Indicadores.csv")

## FIN 2 ##

x_Esfuerzo <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Objetivo") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Esfuerzo = paste(Esfuerzo, Cientifico)) %>% 
  mutate(Esfuerzo_Key = paste(Comun, Animal, "CPUE; Captura; Esfuerzo; Permisos; Pangas; Veda; Embarcaciones; Flota",
                              sep ="; ")) %>%
  select(Esfuerzo,
         Esfuerzo_Key,
         Cientifico,
         Animal)

write.csv(x_Esfuerzo,
          "x_Esfuerzo.csv")

### FIN 3 ###

x_Estrategia <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Objetivo") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Estrategia = paste(Estrategia, Cientifico)) %>% 
  mutate(Estrategia_Key = paste(Comun, Animal,"Permisos; Veda; Flota; Zonas de Pesca; Talla minima; Quota; ",
                                sep ="; ")) %>%
  select(Estrategia,
         Estrategia_Key,
         Cientifico,
         Animal)


write.csv(x_Estrategia, "x_Estrategia.csv")

### FIN 4 ###


# Incidental y Asociada ####

Spp_Incidental <- Species_Rev %>% 
  filter(CNP ==2010) %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Incidental") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  mutate(Titulo_I = paste(Incidental,Cientifico)) %>% 
  mutate(Key_I = paste("Nombre Cientifico; comun; bycatch",Comun,Animal)) %>% 
  mutate(Dataset_I = paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Pacifico),",Animal)) %>% 
  select(Titulo_I,
         Dataset_I,
         Cientifico)
  
Key_I <- Species_Rev %>% 
  filter(CNP ==2010) %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Incidental") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico) %>% 
  summarise_each(funs(paste(., collapse="; "))) %>% 
  mutate(Key = paste("Nombre Cientifico; comun; bycatch",Animal)) %>% 
  select(Key)


Key_A <- Species_Rev %>% 
  filter(CNP ==2010) %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico) %>% 
  summarise_each(funs(paste(., collapse="; "))) %>% 
  mutate(Key = paste("Nombre Cientifico; comun; bycatch",Animal)) %>% 
  select(Key)

Spp_Asociada <- Species_Rev %>% 
  filter(Litoral == "Atlantico") %>% 
  filter(CNP ==2010) %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico) %>% 
  summarise(n()) %>% 
  mutate(Titulo_A = paste(Asociada,Cientifico)) %>% 
  select(Cientifico,Titulo_A)

NComunes <- Species_Rev %>% 
  filter(Clasificacion == "Asociadas") %>% 
  filter(Animal != "Escama") %>% #Se repite en las subfichas
  group_by(Cientifico,
           Animal) %>% 
  summarise(n()) %>% 
  semi_join(Spp_Asociada,
            by="Cientifico")

write.csv(Spp_Incidental, "Spp_Incidental.csv")

write.csv(Spp_Asociada,"Spp_Asociada.csv")

#### ___________ FIN Numeros Resumen ________ ##


eee <- aaa %>% 
  mutate(paste("II. Pesquerias Marinas Y Costeras (a. Litoral del Atlantico),",Pesquerias))



#### Jenny Carolina ####
# Corales
Corales <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/Hector Reyes/Jenny Carolina/Corales.csv")
View(Corales)


x <- data.frame()
y <- data.frame()
for(i in 1:13){
  for(j in 1:18){
  x[i,j] = paste("Estado de salud de",Corales$Especies[j],"en", Corales$Sitios[i])
  y[j,1] = paste(Corales$Especies[j])
  }
}

Corales_Fin <- x %>% 
  bind_cols(y) %>% 
  gather("x","Titulo",1:18) %>% 
  gather("xx","Subject",1:18) %>% 
  mutate(Key = paste("Arrecifes; Sano; Lesionado; Corales"))

# write.csv(Corales_Fin,
#           "Corales_Fin.csv")
  

## ___________________________FIN____________________________________###


##### Gloabl Fishing Cost ciky Lam ####


Cost_Vicky <- read_csv("~/Desktop/Cost_Vicky.csv")

x <- data.frame()
y <- data.frame()
for(i in 1:29){
  for(j in 1:4){
    x[i,j] = paste(Cost_Vicky$Cost[j],"cost for", Cost_Vicky$Art[i], "fishing")
    y[i,1] = paste(Cost_Vicky$Arte[i])
  }
}

Cost_Fin <- x %>% 
  bind_cols(y) %>% 
  gather("x","Titulo",1:4) %>% 
  select(-x) %>% 
  mutate(Key = paste(V1,"Costo; Pesca; Estimado; Dolares US; Fijo; Variable; 2005", sep="; ")) %>% 
  filter(V1 !="NA")

write.csv(Cost_Fin,"Cost_Fin.csv")

#### ________ FIN ###

#### ____________________________GBIF ____________________________#### 

#__________ GOC ####


GOC_Data <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/GOC_Data.csv", 
                       "\t", escape_double = FALSE)

GOC_Data$N_year <- as.numeric(GOC_Data$year)


GBIF_DATA_GoC <- GOC_Data %>% 
  group_by(species) %>% 
    summarise(
      n = length(unique(year)),
      mean_Long = mean(decimallongitude),
      mean_Lat = mean(decimallatitude),
      min_y = min(year, na.rm = T),
      max_y = max(year, na.rm=T)
    ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en el Golfo de California")) %>% 
  mutate(Key = "taxonomia; ecologia; distribucion; macrofauna; mar de cortes; peces; reptiles; mamiferos; aves marinas; microinvertebrados")


write.csv(GBIF_DATA_GoC,
          "GBIF_DATA_GoC.csv")

#__________ Tiburcios ####

Tiburcios_Data <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Tiburones.csv", 
                       "\t", escape_double = FALSE)

Tiburcios_Data$N_year <- as.numeric(Tiburcios_Data$year)

Tiburcios_Data_Final <- Tiburcios_Data %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia (y fecha de) de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; tiburon; checklist; literatura; museos; colecciones; trabajo de campo")) %>% 
  select(
    Key,
    n)


x <- data.table(Tiburcios_Data_Final$locations)



write.csv(Tiburcios_Data_Final,
          "Tiburcios_Data_Final_I.csv")


#__________ Poliqueta ####

Poliqueta_Data <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Poliqueta.csv", 
                             "\t", escape_double = FALSE)

Poliqueta_Data$year <- as.numeric(Poliqueta_Data$year)


Poliqueta_I <- Poliqueta_Data %>%  #Datos con especie
  filter(!is.na(species)) %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Poliqueta; Nereididae; checklist; literatura; museos; colecciones; trabajo de campo"))

Poliqueta_II <- Poliqueta_Data %>%  #Datos con especie
  filter(is.na(species)) %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",genus," (Genero) en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Poliqueta; Nereididae; checklist; literatura; museos; colecciones; trabajo de campo")) %>% 
  rename(species = genus) %>% 
  bind_rows(Poliqueta_I)

write.csv(Poliqueta_II,
          "Poliqueta_II.csv")

#__________ Algas ####

Algas <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Algas.csv", 
                             "\t", escape_double = FALSE)

Algas$year <- as.numeric(Algas$year)

Algas_Final <- Algas %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Rodophilas; Coralines; checklist; literatura; museos; colecciones; trabajo de campo"))

write.csv(Algas_Final,
          "Algas_Final.csv")


#________ Macro Algas Gdpe ####


Macro_Algas <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Macro_Gdp.csv", 
                    "\t", escape_double = FALSE)

Macro_Algas$year <- as.numeric(Macro_Algas$year)

Macro_Algas_Final <- Macro_Algas %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Rodophilas; Coralines; checklist; literatura; museos; colecciones; trabajo de campo"))

write.csv(Macro_Algas_Final,
          "Macro_Algas_Final.csv")


#________ Equinodermos ####


Equinodermos <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Equinodermos.csv", 
                          "\t", escape_double = FALSE)

# Es posible que sea la misma base que la de la UNAM...
UNAM <- Template %>% 
  filter(Dataset_Title == "Coleccion Nacional de Equinodermos Mexicanos Dra. Maria Elena Caso Munoz") %>% 
  rename(species = Subject_name)


Equinos <- Equinodermos %>% 
  group_by(species) %>% 
  summarise(n=n())


Ausentes <- Equinos %>% # 361 especies en la nueva que no estan en el Template
  anti_join(UNAM,
            by="species")

Presentes <- Equinos %>% # 292 especies en la nueva que tmb estan en el Template
  semi_join(UNAM,
            by="species")


Equinodermos$year <- as.numeric(Equinodermos$year)

Equinodermos_Final_Data <- Equinodermos %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Equinodermos; checklist; coleccion; trabajo de campo"))

write.csv(Equinodermos_Final_Data,
          "Equinodermos_Final_Data.csv")

#____________ Flora playas y Dunas ####


Flora <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Flora_Dunas.csv", 
                                    "\t", escape_double = FALSE)


Flora$year <- as.numeric(Flora$year)

Flora_Final <- Flora %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; dunas; playas; costa; checklist; literatura; museos; colecciones; trabajo de campo"))

write.csv(Flora_Final,
          "Flora_Final.csv")

#______ Crustaceos GoM ####

Crustaceos <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Crustaceos_GoM.csv", 
                    "\t", escape_double = FALSE)


Crustaceos$year <- as.numeric(Crustaceos$year)

Crustaceos_Data_Final <- Crustaceos %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; dunas; playas; costa; checklist; literatura; museos; colecciones; trabajo de campo"))

write.csv(Crustaceos_Data_Final,
          "Crustaceos_Data_Final.csv")



#____________ Macroalgas_N playas y Dunas ####


Macroalgas_N <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Macroalgas_N.csv", 
                    "\t", escape_double = FALSE)


Macroalgas_N$year <- as.numeric(Macroalgas_N$year)

Macroalgas_N_Data_Final <- Macroalgas_N %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; dunas; playas; costa; checklist; literatura; museos; colecciones; trabajo de campo"))

write.csv(Macroalgas_N_Data_Final,
          "Macroalgas_N_Data_Final.csv")

#______ Crustaceos GoM ####

Crustaceos <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Crustaceos_GoM.csv", 
                         "\t", escape_double = FALSE)


Crustaceos$year <- as.numeric(Crustaceos$year)

Crustaceos_Data_Final <- Crustaceos %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; dunas; playas; costa; checklist; literatura; museos; colecciones; trabajo de campo"))

write.csv(Crustaceos_Data_Final,
          "Crustaceos_Data_Final.csv")

# ____________ Ictoplancton ###



Ictoplancton <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Ictoplancton.csv", 
                         "\t", escape_double = FALSE)


Ictoplancton$year <- as.numeric(Ictoplancton$year)

Ictoplancton_Data_S <- Ictoplancton %>%  #Datos con especie
  filter(taxonrank == "SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude),
    mean_Lat = mean(decimallatitude),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Celestun; Ictioplancton; plankton; costa; checklist; zzoplancton"))

Ictoplancton_Data_Final <- Ictoplancton %>%  #Datos con especie
  filter(taxonrank != "SPECIES") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",genus,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; distribucion; Celestun; Ictioplancton; plankton; costa; checklist; zzoplancton")) %>% 
  rename(species = genus) %>% 
  bind_rows(Ictoplancton_Data_S)



write.csv(Ictoplancton_Data_Final,
          "Ictoplancton_Data_Final.csv")


# ___________ Tortugas ####

Tortugas <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Tortugas.csv", 
                           "\t", escape_double = FALSE)


Tortugas$year <- as.numeric(Tortugas$year)

Tortugas_Data_Final <- Tortugas %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; playas; anidacion; tortugas marinas"))


write.csv(Tortugas_Data_Final,
          "Tortugas_Data_Final.csv")


# ___________ Rhizophora_Gen ####

Rhizophora_Gen <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Rhizophora_Gen.csv", 
                       "\t", escape_double = FALSE)


Rhizophora_Gen$year <- as.numeric(Rhizophora_Gen$year)

Rhizophora_Gen_Data_Final <- Rhizophora_Gen %>%  #Datos con especie
  group_by(species,
           locality) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Estudio Genetico de",species,"en",locality)) %>% 
  mutate(Key = paste("taxonomia; ecologia; playas; anidacion; Rhizophora"))


write.csv(Rhizophora_Gen_Data_Final,
          "Rhizophora_Gen_Data_Final.csv")


# ___________ Diatomeas_G ####

Diatomeas_G <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Diatomeas_G.csv", 
                             "\t", escape_double = FALSE)

#### Mexico's EEZ

EEZ_Mex <- read.csv("./Data/eez_Mex.csv")

# Get Mexico's Pacific EEZ
eez_Mex_P <- EEZ_Mex %>%
  filter(piece == 1)

# Get Mexico's Atlantic EEZ (2)
eez_Mex_A <- EEZ_Mex %>%
  filter(piece == 2)
#________________________________________________________#


Diatomeas_G$year <- as.numeric(Diatomeas_G$year)

Diatomeas_G_Data_Final <- Diatomeas_G %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(" Observacion y frecuencia de",species,"en el G. de Mexico")) %>% 
  mutate(Key = paste("taxonomia; ecologia; playas; frecuencia; Presencia"))


write.csv(Diatomeas_G_Data_Final,
          "Diatomeas_G_Data_Final.csv")


# ___________ Codigo_Barras ####

Codigo_Barras <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Codigo_Barras.csv", 
                          "\t", escape_double = FALSE)
EEZ_Mex <- read.csv("./Data/eez_Mex.csv")


# Get the points in polygon  
#Note <- 1 is present| 0 is ausent
Codigo_Barras_Mex <-data.frame(point.in.polygon(Codigo_Barras$decimallongitude,
                                     Codigo_Barras$decimallatitude,
                                     EEZ_Mex$long,
                                     EEZ_Mex$lat))

colnames(Codigo_Barras_Mex) <- "EEZ"



Codigo_Barras$year <- as.numeric(Codigo_Barras$year)

Codigo_Barras_Data_Final <- Codigo_Barras %>% 
  bind_cols(Codigo_Barras_Mex) %>%  #Datos con especie
  filter(EEZ == 1) %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Codigo de barras (iBOL) para",species)) %>% 
  mutate(Key = paste("taxonomia; genetica; iBOL; International Barcode of Life; DNA; bases; MEXBOL"))


write.csv(Codigo_Barras_Data_Final,
          "Codigo_Barras_Data_Final.csv")


# ___________ Ictioplancton_Campeche ####

Ictioplancton_Campeche <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Ictioplancton_Campeche.csv", 
                             "\t", escape_double = FALSE)


Ictioplancton_Campeche$year <- as.numeric(Ictioplancton_Campeche$year)

Ictioplancton_Campeche_Data_Sp <- Ictioplancton_Campeche %>%  #Datos con especie
  filter(taxonrank == "SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Presencia de",species,"en la bahia de Campeche")) %>% 
  mutate(Key = paste("taxonomia; ecologia; presencia; ictioplancton; plancton; zooplancton; Terminos; Centla"))

Ictioplancton_Campeche_Data_G <- Ictioplancton_Campeche %>%  #Datos con especie
  filter(taxonrank == "GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Presencia de",genus," (Gen.) en la bahia de Campeche")) %>% 
  mutate(Key = paste("taxonomia; ecologia; presencia; ictioplancton; plancton; zooplancton; Terminos; Centla")) %>% 
  rename(species = genus)

Ictioplancton_Campeche_Data_F <- Ictioplancton_Campeche %>%  #Datos con especie
  filter(taxonrank == "FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Presencia de",family," (Fam.) en la bahia de Campeche")) %>% 
  mutate(Key = paste("taxonomia; ecologia; presencia; ictioplancton; plancton; zooplancton; Terminos; Centla")) %>% 
  rename(species = family) %>% 
  bind_rows(Ictioplancton_Campeche_Data_Sp,
            Ictioplancton_Campeche_Data_G)


write.csv(Ictioplancton_Campeche_Data_F,
          "Ictioplancton_Campeche_Data_Final.csv")



# ___________ Macroinver_TJ ####

Macroinver_TJ <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Macroinver_TJ.csv", 
                       "\t", escape_double = FALSE)


Macroinver_TJ_Data_Final <- Macroinver_TJ %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Ensenada-Tijuana")) %>% 
  mutate(Key = paste("taxonomia; ecologia; Macroinvertebrados; benthos; bentico; Annelida; Mollusca; Echinodermata; Crustacea; Plataforma Continental; RMP"))


write.csv(Macroinver_TJ_Data_Final,
          "Macroinver_TJ_Data_Final.csv")



# ___________ Aves_Playeras_Tam ####

Aves_Playeras_Tam <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Aves_Playeras_Tam.csv", 
                            "\t", escape_double = FALSE)


Aves_Playeras_Tam_Data_Final <- Aves_Playeras_Tam %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en Ensenada-Tijuana")) %>% 
  mutate(Key = paste("taxonomia; ecologia; Invertebrados; aves; playeras; Laguna Madre; LMT"))


write.csv(Aves_Playeras_Tam_Data_Final,
          "Aves_Playeras_Tam_Data_Final.csv")



# ___________ Crustaceos_Macro_GoM ####

Crustaceos_Macro_GoM <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Crustaceos_Macro_GoM.csv", 
                                "\t", escape_double = FALSE)
View(Crustaceos_Macro_GoM)


Crustaceos_Macro_GoM_Data_Final <- Crustaceos_Macro_GoM %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Ocurrencia de",species,"en la plataforma continental y talud del GoM")) %>% 
  mutate(Key = paste("taxonomia; ecologia; crustaceos; talud; macrobentos; talud; plataforma"))


write.csv(Crustaceos_Macro_GoM_Data_Final,
          "Crustaceos_Macro_GoM_Data_Final.csv")


# ___________ Parasitos_Noroeste ####

Parasitos_Noroeste <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Parasitos_Noroeste.csv", 
                                   "\t", escape_double = FALSE)
View(Parasitos_Noroeste)

Parasitos_Noroeste_Data_S <- Parasitos_Noroeste %>%  #Datos con especie
  filter(taxonrank == "SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Identificacion de",species,"como parasito de peces")) %>% 
  mutate(Key = paste("taxonomia; ecologia; peces; parasitos; Bahia Mazatlan; Bahia de Banderas; SNIB"))

Parasitos_Noroeste_Data_G <- Parasitos_Noroeste %>%  #Datos con especie
  filter(taxonrank == "GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Identificacion de",genus," (Gen.)como parasito de peces")) %>% 
  mutate(Key = paste("taxonomia; ecologia; peces; parasitos; Bahia Mazatlan; Bahia de Banderas; SNIB")) %>% 
  rename(species = genus)

Parasitos_Noroeste_Data_F <- Parasitos_Noroeste %>%  #Datos con especie
  filter(taxonrank == "FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Identificacion de",family,"(Fam.) como parasito de peces")) %>% 
  mutate(Key = paste("taxonomia; ecologia; peces; parasitos; Bahia Mazatlan; Bahia de Banderas; SNIB")) %>% 
  rename(species = family) %>% 
  bind_rows(Parasitos_Noroeste_Data_G,
            Parasitos_Noroeste_Data_S)

write.csv(Parasitos_Noroeste_Data_F,
          "Parasitos_Noroeste_Data_Final.csv")


# ___________ Macroalgas_Invasoras ####

Macroalgas_Invasoras <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Macroalgas_Invasoras.csv", 
                                   "\t", escape_double = FALSE)
View(Macroalgas_Invasoras)


Macroalgas_Invasoras_Data_Final <- Macroalgas_Invasoras %>%  #Datos con especie
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Estado actual de",species,"en el Pacifico Norte")) %>% 
  mutate(Key = paste("taxonomia; ecologia; macroalgas; introducidas; invasoras; costa"))


write.csv(Macroalgas_Invasoras_Data_Final,
          "Macroalgas_Invasoras_Data_Final.csv")


# ___________ Ictiofauna_Banderas ####

Ictiofauna_Banderas <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Ictiofauna_Banderas.csv", 
                                   "\t", escape_double = FALSE)
View(Ictiofauna_Banderas)


Ictiofauna_Banderas_Data_S <- Ictiofauna_Banderas %>%  #Datos con especie
  filter(taxonrank =="SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Taxonomia y Sistematica de",species)) %>% 
  mutate(Key = paste("taxonomia; ecologia; ictiofauna; peces"))

Ictiofauna_Banderas_Data_G <- Ictiofauna_Banderas %>%  #Datos con especie
  filter(taxonrank =="GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Taxonomia y Sistematica de",genus, "(Gen.)")) %>% 
  mutate(Key = paste("taxonomia; ecologia; ictiofauna; peces")) %>% 
  rename(species = genus)

Ictiofauna_Banderas_Data_F <- Ictiofauna_Banderas %>%  #Datos con especie
  filter(taxonrank =="FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Taxonomia y Sistematica de",family, "(Fam.)")) %>% 
  mutate(Key = paste("taxonomia; ecologia; ictiofauna; peces")) %>% 
  rename(species = family) %>% 
  bind_rows(Ictiofauna_Banderas_Data_S,
            Ictiofauna_Banderas_Data_G)


write.csv(Ictiofauna_Banderas_Data_F,
          "Ictiofauna_Banderas_Data_Final.csv")



# ___________ Aves_Playeras_II ####

Aves_Playeras_II <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Aves_Playeras_II.csv", 
                                  "\t", escape_double = FALSE)
View(Aves_Playeras_II)


Aves_Playeras_II_Data_S <- Aves_Playeras_II %>%  #Datos con especie
  filter(taxonrank =="SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Datos de observacion de",species, "en la Laguna Madre")) %>% 
  mutate(Key = paste("taxonomia; ecologia; invertebrados; aves; playeras"))

Aves_Playeras_II_Data_G <- Aves_Playeras_II %>%  #Datos con especie
  filter(taxonrank =="GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Datos de observacion de",genus, "(Gen.) en la Laguna Madre")) %>% 
  mutate(Key = paste("taxonomia; ecologia; invertebrados; aves; playeras")) %>% 
  rename(species = genus)

Aves_Playeras_II_Data_F <- Aves_Playeras_II %>%  #Datos con especie
  filter(taxonrank =="FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Datos de observacion de",family, "(Fam.) en la Laguna Madre")) %>% 
  mutate(Key = paste("taxonomia; ecologia; invertebrados; aves; playeras")) %>% 
  rename(species = family) %>% 
  bind_rows(Aves_Playeras_II_Data_S,
            Aves_Playeras_II_Data_G)


write.csv(Aves_Playeras_II_Data_F,
          "Aves_Playeras_II_Data_Final.csv")

# ___________ Aves_Playeras_III ####

Aves_Playeras_III <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Aves_Playeras_III.csv", 
                               "\t", escape_double = FALSE)
View(Aves_Playeras_III)


Aves_Playeras_III_Data_S <- Aves_Playeras_III %>%  #Datos con especie
  filter(taxonrank =="SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Datos de observacion de",species, "en la Laguna Madre")) %>% 
  mutate(Key = paste("taxonomia; ecologia; invertebrados; aves; playeras"))

Aves_Playeras_III_Data_G <- Aves_Playeras_III %>%  #Datos con especie
  filter(taxonrank =="GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Datos de observacion de",genus, "(Gen.) en la Laguna Madre")) %>% 
  mutate(Key = paste("taxonomia; ecologia; invertebrados; aves; playeras")) %>% 
  rename(species = genus)

Aves_Playeras_III_Data_F <- Aves_Playeras_III %>%  #Datos con especie
  filter(taxonrank =="FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Datos de observacion de",family, "(Fam.), en la Laguna Madre")) %>% 
  mutate(Key = paste("taxonomia; ecologia; invertebrados; aves; playeras")) %>% 
  rename(species = family) %>% 
  bind_rows(Aves_Playeras_III_Data_S,
            Aves_Playeras_III_Data_G)


write.csv(Aves_Playeras_III_Data_F,
          "Aves_Playeras_III_Data_Final.csv")


# ___________ Larvas_Atun ####

Larvas_Atun <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Larvas_Atun.csv", 
                                   "\t", escape_double = FALSE)
View(Larvas_Atun)


Larvas_AtunS <- Larvas_Atun %>%  #Datos con especie
  filter(taxonrank =="SPECIES") %>% 
  group_by(species) %>% 
  summarise(
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T),
    n = length(unique(year))
  ) %>% 
  mutate(Titulo = paste("Distribucion espacio-temporal de larvas de",species, "en el Golfo de Tehuantepec")) %>% 
  mutate(Key = paste("larvas; distribucion; atun; tunidos"))

Larvas_AtunG <- Larvas_Atun %>%  #Datos con especie
  filter(taxonrank =="GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Distribucion espacio-temporal de larvas de",genus, "(Gen.) en el Golfo de Tehuantepec")) %>% 
  mutate(Key = paste("larvas; distribucion; atun; tunidos")) %>% 
  rename(species = genus)

Larvas_AtunF <- Larvas_Atun %>%  #Datos con especie
  filter(taxonrank =="FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste("Distribucion espacio-temporal de larvas de",family, "(Fam.), en el Golfo de Tehuantepec")) %>% 
  mutate(Key = paste("larvas; distribucion; atun; tunidos")) %>% 
  rename(species = family) %>% 
  bind_rows(Larvas_AtunS,
            Larvas_AtunG)


write.csv(Larvas_AtunF,
          "Larvas_AtunFinal.csv")


#___________ Algas GoC #####

Algas_GoC <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Algas_GoC.csv", 
                          "\t", escape_double = FALSE)
View(Algas_GoC)

Keywords <- "Flores; algas marinas; herbario; CMMEX; Rhodophyta; Phaeophyta; Chlorophyta"
Inicio <- "Descripcion floristica de"
Fin <- "en el Alto Golfo de California"

Algas_GoCS <- Algas_GoC %>%  #Datos con especie
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

Algas_GoCG <- Algas_GoC %>%  #Datos con especie
  filter(taxonrank =="GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, genus, "(Gen.)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = genus) %>% 
  bind_rows(Algas_GoCS,
            Algas_GoCG)


Algas_GoCF <- Algas_GoC %>%  #Datos con especie
  filter(taxonrank =="FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, family, "(Fam.)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = family) %>% 
  bind_rows(Algas_GoCS,
            Algas_GoCG)


write.csv(Algas_GoCS,
          "Algas_GoCFinal.csv")


#___________ Fauna Batial #####

Fauna_Batial <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Fauna_Batial.csv", 
                        "\t", escape_double = FALSE)
View(Fauna_Batial)

Keywords <- "Campana; oceanografica; SIGSBEE; DGoMB-JSSD; Sigsbee; Planicie abisal; batial; abisopelagica"
Inicio <- "Presencia de"
Fin <- "en el fondo marino del GoM"

Fauna_BatialS <- Fauna_Batial %>%  #Datos con especie
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

Fauna_BatialG <- Fauna_Batial %>%  #Datos con especie
  filter(taxonrank =="GENUS") %>% 
  group_by(genus) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, genus, "(Gen.)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = genus)

Fauna_BatialC <- Fauna_Batial %>%  #Datos con especie
  filter(taxonrank =="CLASS") %>% 
  group_by(class) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, class, "(Class)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = class)

Fauna_BatialO <- Fauna_Batial %>%  #Datos con especie
  filter(taxonrank =="ORDER") %>% 
  group_by(order) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, order, "(Ord.)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = order)

Fauna_BatialK <- Fauna_Batial %>%  #Datos con especie
  filter(taxonrank =="KINGDOM") %>% 
  group_by(kingdom) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, kingdom, "(King.)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = kingdom)


Fauna_BatialF <- Fauna_Batial %>%  #Datos con especie
  filter(taxonrank =="FAMILY") %>% 
  group_by(family) %>% 
  summarise(
    n = length(unique(year)),
    mean_Long = mean(decimallongitude, na.rm =T),
    mean_Lat = mean(decimallatitude, na.rm =T),
    min_y = min(year, na.rm = T),
    max_y = max(year, na.rm=T)
  ) %>% 
  mutate(Titulo = paste(Inicio, family, "(Fam.)",Fin)) %>% 
  mutate(Key = paste(Keywords)) %>% 
  rename(species = family) %>% 
  bind_rows(Fauna_BatialS,Fauna_BatialG,Fauna_BatialC,Fauna_BatialO,Fauna_BatialK)


write.csv(Fauna_BatialF,
          "Fauna_Batial_Final_Data.csv")

# ______________ Aves_Playeras_IV ####

Aves_Playeras_IV <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Aves_Playeras_IV.csv", 
                                "\t", escape_double = FALSE)
View(Aves_Playeras_IV)

Inicio <- "Ocurrencia de"
Fin <- "en la laguna Madre, Tamaulipas"
Keywords <- "taxonomia; ecologia; Invertebrados; aves; playeras; Laguna Madre; LMT"

Aves_Playeras_IV_Final <- Aves_Playeras_IV %>%  #Datos con especie
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


write.csv(Aves_Playeras_IV_Final,
          "Aves_Playeras_IV_Data_Final.csv")

# ______________ Gen_Robalo ####

Gen_Robalo <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Gen_Robalo.csv", 
                               "\t", escape_double = FALSE)
View(Gen_Robalo)

Inicio <- "Genetica y taxonomia de"
Fin <- ""
Keywords <- "taxonomia; ecologia; robalo; genetica; DNA"

Gen_Robalo_Final <- Gen_Robalo %>%  #Datos con especie
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


write.csv(Gen_Robalo_Final,
          "Gen_Robalo_Final_Data.csv")



# ______________ Rodolitos ####

Rodolitos <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Rodolitos.csv", 
                         "\t", escape_double = FALSE)
View(Rodolitos)

Inicio <- "Morfologia funcional de"
Fin <- "en el Golfo de California"
Keywords <- "Rodolitos; taxonomia; morfologia; mantos"

RodolitosS <- Rodolitos %>%  #Datos con especie
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

RodolitosG <- Rodolitos %>%  #Datos con especie
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
            
RodolitosF <- Rodolitos %>%  #Datos con especie
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
  bind_rows(RodolitosS,
            RodolitosG)


write.csv(RodolitosF,
          "Rodolitos_Final_Data.csv")

# ______________ Decapodos ####

Decapodos <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Decapodos.csv", 
                        "\t", escape_double = FALSE)
View(Decapodos)

Inicio <- "Presencia de"
Fin <- "en el Pacifico tropical"
Keywords <- "decapodos; crustaceos; pocillopora; corales"

DecapodosS <- Decapodos %>%  #Datos con especie
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

DecapodosG <- Decapodos %>%  #Datos con especie
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

DecapodosF <- Decapodos %>%  #Datos con especie
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
  bind_rows(DecapodosS,
            DecapodosG)


write.csv(DecapodosF,
          "Decapodos_Final_Data.csv")


# ______________ Larvas_GoC ####

Larvas_GoC <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Larvas_GoC.csv", 
                        "\t", escape_double = FALSE)

unique(Larvas_GoC$taxonrank)

Inicio <- "Inventario de larvas de"
Fin <- "en el Pacifico tropical"
Keywords <- "Larvas; peces"

Larvas_GoCS <- Larvas_GoC %>%  #Datos con especie
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


write.csv(Larvas_GoCS,
          "Larvas_GoC_Final_Data.csv")


# ______________ Codigo barras Huevos_ ####

CB_Huevos <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Codigo_Barra_Huevos.csv", 
                         "\t", escape_double = FALSE)
unique(CB_Huevos$taxonrank)

Inicio <- "Codigo de barras para huevos y larvas de"
Fin <- ""
Keywords <- "Codigo de barras; huevos; larvas; costeros; oceanicos; peces; sistema arrecifal mesoamericano; caribe"

CB_HuevosS <- CB_Huevos %>%  #Datos con especie
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

CB_HuevosG <- CB_Huevos %>%  #Datos con especie
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

CB_HuevosF <- CB_Huevos %>%  #Datos con especie
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
  bind_rows(CB_HuevosS,
            CB_HuevosG)


write.csv(CB_HuevosF,
          "CB_Huevos_Final_Data.csv")

# ______________ Escribano ####

Escribano <- read_delim("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Fitoplancton_Ts.csv", 
                        "\t", escape_double = FALSE)
unique(Escribano$taxonrank)


Inicio <- "Caracterizacion y evaluacion de la pesqueria de"
Fin <- "en Quintana Roo"
Keywords <- "Pesqueria; escribanos; migratorios; picudas; pesca deportiva"



GBIF_Create(Escribano,Inicio,Fin,Keywords,"Escribano")

# ______________ Tortuga_Laud ####


Inicio <- "Distribucion de la poblacion de"
Fin <- "en el Pacifico mexicano (1996-1997)"
Keywords <- "Tortuga; laud; anidacion; temporada"

Author <- "Sarti Martinez., et al"
Institution <- "GBIF-UNAM"
Area <- "Pacific"
Region <- "" 
Location <- ""
Dataset_Title <- "Estimacion del tamano de la poblacion anidadora de tortuga laud dermochelys coriacea y su distribucion en el pacífico mexicano durante la temporada de anidacion 1996-1997" 
Reference <- "https://doi.org/10.15468/siicu6"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Tortuga_Laud,Inicio,Fin,Keywords,"Tortuga_Laud_I")


# ______________ Aves_Playeras V ####

## FUNCION ###
Titulo <- "Aves_Playeras_Tam_V"
Inicio <- "Ocurrencia de"
Fin <- "en laguna Madre de Tamaulipas"
Keywords <- "Aves; Pplayera; presencia; registro"

## TEMPLATE ###
Author <- "Gabino Rodriguez., et al"
Institution <- "GBIF-UNAL"
Area <- "Pacific"
Region <- "W. G. of Mexico" 
Location <- "Laguna Madre"
Dataset_Title <- "Invertebrados y aves playeras de la Laguna Madre de Tamaulipas, México" 
Reference <- "https://doi.org/10.15468/gnalq5"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Fitoplancton_Ts ####

## FUNCION ###
Titulo <- "Carey_Lagartos"
Inicio <- "Dinamica poblacional de "
Fin <- "en Todos Santos"
Keywords <- "Fitoplancton; Todos Santos; Ensenada; Contaminacion"

## TEMPLATE ###
Author <- "Orellana Cepeda., et al"
Institution <- "GBIF-UABC"
Area <- "Pacific"
Region <- "G. Of California" 
Location <- "Todos Santos"
Dataset_Title <- "Dinámica poblacional de la tortuga de carey (Eretmochelys imbricata) en su área de forraje. Río Lagartos, Yucatán" 
Reference <- "https://doi.org/10.15468/aswbyx"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Dorada ####

## FUNCION ###
Titulo <- "Dorada"
Inicio <- "Deteccion de reclutas de"
Fin <- ""
Keywords <- "dorada; Sparus; cultico"

## TEMPLATE ###
Author <- "Cruz Aguero., et al"
Institution <- "GBIF-IPN"
Area <- "Pacific"
Region <- "G. Of California" 
Location <- "Bahia de La Paz"
Dataset_Title <- "Deteccion de reclutas de la dorada Sparus aurata como medida del nivel de establecimiento en la Bahia de La Paz, BCS" 
Reference <- "https://doi.org/10.15468/ajyuo9"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Spp_Bentonicas_Caribe ####

## FUNCION ###
Titulo <- "Spp_Bentonicas_Caribe"
Inicio <- "Distribucion de"
Fin <- "en el Caribe"
Keywords <- "hexacorales; octocorales; esponjas; especies miscelaneas; bentos;"

## TEMPLATE ###
Author <- "Chavez Ortiz., et al"
Institution <- "GBIF-IPN"
Area <- "Atlantic"
Region <- "B. Campeche Caribe" 
Location <- "Quintana Roo"
Dataset_Title <- "Distribucion e inventario de algunas especies bentonicas (hexacorales, octocorales, esponjas, y especies miscelaneas) en arrecifes del Caribe mexicano" 
Reference <- "https://doi.org/10.15468/txdpn5"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Decapodos_UNAM ####

## FUNCION ###
Titulo <- "Decapodos_UNAM"
Inicio <- "Registro de"
Fin <- "en coleccion de la FCB, UANL"
Keywords <- "Crustaceos; Decapodos;"

## TEMPLATE ###
Author <- "Rodriguez Almaraz., et al"
Institution <- "GBIF-UANL"
Area <- "National"
Region <- "" 
Location <- ""
Dataset_Title <- "Los crustaceos decapodos marinos: Actualizacion de la coleccion carcinologica de la Facultad de Ciencias Biologicas, UANL" 
Reference <- "https://doi.org/10.15468/w1lujq"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Anfipodos_ColJa ####

## FUNCION ###
Titulo <- "Anfipodos_ColJa"
Inicio <- "Registro de"
Fin <- "en Jalisco y Colima"
Keywords <- "anfipodos; crustaceos; zooplancton; hiperidos; area marina prioritaria"

## TEMPLATE ###
Author <- "Gasca Serrano., et al"
Institution <- "GBIF-ECOSUR"
Area <- "Pacific"
Region <- "Sentral Pacific" 
Location <- "Colima y Jalisco"
Dataset_Title <- "Base de datos y coleccion de anfipodos (Hyperiidea:Crustacea) de regiones marinas prioritarias de Jalisco y Colima en el Pacifico mexicano" 
Reference <- "https://doi.org/10.15468/c2edgd"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Macroalgas_Banderas ####

## FUNCION ###
Titulo <- "Macroalgas_Banderas"
Inicio <- "Registro de"
Fin <- "en Bahia Banderas"
Keywords <- "macroalgas; algas; Chlorophyta; Phaeophyta; Rhodophyta; Cianophyta; FCME; Herbario"

## TEMPLATE ###
Author <- "Gonzalez Gonzalez., et al"
Institution <- "GBIF-UNAM"
Area <- "Pacific"
Region <- "Sentral Pacific" 
Location <- "Bahia Banderas"
Dataset_Title <- "Inventario de macroalgas de Bahia de Banderas: Fase I y Fase II" 
Reference <- "https://doi.org/10.15468/pglvkj"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Fitoplancton_BC ####

## FUNCION ###
Titulo <- "Fitoplancton_BC"
Inicio <- "Area de Surgencia de"
Fin <- ""
Keywords <- "zooplancton; Surgencia; Enscenada; Biodiversidad"

## TEMPLATE ###
Author <- "Orellana Cepeda., et al"
Institution <- "GBIF-UABC"
Area <- "Pacific"
Region <- "Gulf of California" 
Location <- "Ensenada"
Dataset_Title <- "Fitoplancton marino frente a Baja California. 2. Areas de surgencias de la region Ensenadense" 
Reference <- "https://doi.org/10.15468/dcpjjs"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Tortugas_gen ####

## FUNCION ###
Titulo <- "Tortugas_gen"
Inicio <- "Genetica poblacional de"
Fin <- ""
Keywords <- "Tortugas; ADN; genetica; filogeografia; golfina; laud"

## TEMPLATE ###
Author <- "Abreu Grobois., et al"
Institution <- "GBIF-UNAM"
Area <- "Pacific"
Region <- "" 
Location <- ""
Dataset_Title <- "Genetica poblacional y filogeografia de las tortugas marinas golfina (Lepidochelys olivacea) y laud (Dermochelys coriacea) en el Pacifico mexicano" 
Reference <- "https://doi.org/10.15468/dlyav9"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Cetaceos_BA ####

## FUNCION ###
Titulo <- "Cetaceos_BA"
Inicio <- "Distribucion de"
Fin <- "en Bahia de Los Angeles (BCS)"
Keywords <- "Diversidad; Distribucion; abundancia; Cetaceos; Canal Ballenas; Angeles; turismo; mamifero marino"

## TEMPLATE ###
Author <- "Heckel Dziendzielewski., et al"
Institution <- "GBIF-CICESE"
Area <- "Pacific"
Region <- "Gulf of California" 
Location <- "Bahia de los Angeles"
Dataset_Title <- "Diversidad, distribucion y abundancia de cetaceos en Bahia de los Angeles y Canal de Ballenas, Golfo de California: bases cientificas para una nueva area de observacion turistica de mamiferos marinos"
Reference <- "https://doi.org/10.15468/cwwjak"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Kuby ####

Kuby <- Kuby %>% 
  filter(countrycode == "MX")

write.csv(Kuby,
          "Kuby.csv",
          col.names = FALSE)

## FUNCION ###
Titulo <- "Kuby"
Inicio <- "Registro de"
Fin <- "en la coleccion de la U. de Kansas"
Keywords <- "Ichthyologia; Peces; KUBI; Coleccion"

## TEMPLATE ###
Author <- "Bentley., et al"
Institution <- "GBIF-U.Kansas"
Area <- "National"
Region <- "" 
Location <- ""
Dataset_Title <- "KUBI Ichthyology Collection"
Reference <- "https://doi.org/10.15468/mgjasg"
User_Contact <- "Andrew Bentley; abentley@ku.edu" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Bacterias_BC ####

## FUNCION ###
Titulo <- "Bacterias_BC"
Inicio <- "Presencia de"
Fin <- "en Bahia Concepcion (BCS)"
Keywords <- "Bacterias; Fototrofas; Comunidad; Microbios; Bentos"

## TEMPLATE ###
Author <- "Lopez Cortes, et al"
Institution <- "GBIF-CIBNOR"
Area <- "Pacifico"
Region <- "Gulf of California" 
Location <- "Bahia Concepcion"
Dataset_Title <- "Diversidad de bacterias fototrofas en comunidades microbianas bentonicas de Bahia Concepcion, BCS, Mexico"
Reference <- "https://doi.org/10.15468/b4gnla"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx"

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Aves_Paz ####

## FUNCION ###
Titulo <- "Aves_Paz"
Inicio <- "Presencia de"
Fin <- "en la ensenada de La Paz (BCS)"
Keywords <- "Riqueza especifica; Distribucion; Abundancai; Aves"

## TEMPLATE ###
Author <- "Carmona Pina, et al"
Institution <- "GBIF-UABCS"
Area <- "Pacifico"
Region <- "Gulf of California" 
Location <- "Ensenada de La Paz"
Dataset_Title <- "Riqueza especifica, distribucion y abundancia de aves acuaticas en la ensenada de La Paz, Baja California Sur, Mexico"
Reference <- "https://doi.org/10.15468/wricli"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx"

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Flora_Terminos ####

## FUNCION ###
Titulo <- "Flora_Terminos"
Inicio <- "Registro de"
Fin <- "en Laguna de Terminos (CAMP)"
Keywords <- "macroalgas; algas; Chlorophyta; Phaeophyta; Rhodophyta; Cianophyta; vascular; zona inundable; fauna; flora; Laguna"

## TEMPLATE ###
Author <- "Bonilla Barbosa, et al"
Institution <- "GBIF-UAEM"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe" 
Location <- "Laguna de Terminos"
Dataset_Title <- "Flora acuatica vascular y de zonas inundables del area de proteccion de flora y fauna Laguna de Terminos, Campeche, Mexico." 
Reference <- "https://doi.org/10.15468/zp7jgv"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Esponjas ####

## FUNCION ###
Titulo <- "Esponjas"
Inicio <- "Registro de"
Fin <- "Para registro de Cofigo de barras"
Keywords <- "Esponjas; DNA; ADN; Codigo de barras; biodiversidad; Porifera; biotecnologia"

## TEMPLATE ###
Author <- "Cruz-Barraza, et al"
Institution <- "GBIF-UAEM"
Area <- "National"
Region <- "" 
Location <- ""
Dataset_Title <- "Nuevas aportaciones a la biodiversidad de esponjas marinas de Mexico: bases para la elaboracion de codigo de barras de ADN" 
Reference <- "https://doi.org/10.15468/pkhco9"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Flora_Centla ####

## FUNCION ###
Titulo <- "Flora_Centla"
Inicio <- "Registro de"
Fin <- "en los Pantanos de Centla"
Keywords <- "Flora; reserva; biosfera; pantanos; Centla; Humedal; inventario"

## TEMPLATE ###
Author <- "Guadarrama Olivera, et al"
Institution <- "GBIF-UJAT"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe"
Location <- "Pantanos de Centla"
Dataset_Title <- "Flora de la reserva de la biosfera de los Pantanos de Centla, en el estado de Tabasco, Mexico" 
Reference <- "https://doi.org/10.15468/nfizxi"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Manglar_Ver ####

## FUNCION ###
Titulo <- "Manglar_Ver"
Inicio <- "Monitoreo de"
Fin <- "en Veracruz"
Keywords <- "Manglar; Mangle; Caracterizacion; monitoreo; Costa"

## TEMPLATE ###
Author <- "Portillo Guzman, et al"
Institution <- "GBIF-INECOL"
Area <- "Atlantic"
Region <- "W. G. of Mexico"
Location <- "Veracruz"
Dataset_Title <- "Programa regional para la caracterizacion y el monitoreo de ecosistemas de manglar del Golfo de Mexico y Caribe Mexicano: Veracruz. Version 1.3"
Reference <- "https://doi.org/10.15468/p2gmmh"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Manati ####

## FUNCION ###
Titulo <- "Manati"
Inicio <- "Monitoreo de"
Fin <- ""
Keywords <- "Manati; ratio-transmisor; monitoreo; genetico; ADN; DNA; mitocondrial"

## TEMPLATE ###
Author <- "Morales Vela, et al"
Institution <- "GBIF-ECOSUR"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe"
Location <- "Quintana Roo"
Dataset_Title <- "Variacion genetica del manati (Trichechus manatus), en el sureste de Mexico y monitoreo con radio-transmisores en Quintana Roo. Version 1.4"
Reference <- "https://doi.org/10.15468/mncmu1"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Aves_GoC_GoM ####

## FUNCION ###
Titulo <- "Aves_GoC_GoM"
Inicio <- "Distribucion de"
Fin <- "en el Golfo de California"
Keywords <- "Aves; abundancia; marinas; acuaticas; regiones marinas prioritarias; area protegida; SAV"

## TEMPLATE ###
Author <- "Velarde Gonzalez, et al"
Institution <- "GBIF-ECOSUR"
Area <- "Pacific"
Region <- "Gulf of California"
Location <- ""
Dataset_Title <- "Distribucion y abundancia de aves marinas y acuaticas en regiones marinas prioritarias y areas protegidas del Golfo de California y Golfo de Mexico"
Reference <- "https://doi.org/10.15468/tld9yx"

User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Tortugas_gen ####

## FUNCION ###
Titulo <- "Tortugas_gen"
Inicio <- "Genetica poblacional de"
Fin <- ""
Keywords <- "Tortugas; ADN; genetica; filogeografia; golfina; laud"

## TEMPLATE ###
Author <- "Abreu Grobois., et al"
Institution <- "GBIF-UNAM"
Area <- "Pacific"
Region <- "" 
Location <- ""
Dataset_Title <- "Genetica poblacional y filogeografia de las tortugas marinas golfina (Lepidochelys olivacea) y laud (Dermochelys coriacea) en el Pacifico mexicano" 
Reference <- "https://doi.org/10.15468/dlyav9"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Cetaceos_BA ####

## FUNCION ###
Titulo <- "Cetaceos_BA"
Inicio <- "Distribucion de"
Fin <- "en Bahia de Los Angeles (BCS)"
Keywords <- "Diversidad; Distribucion; abundancia; Cetaceos; Canal Ballenas; Angeles; turismo; mamifero marino"

## TEMPLATE ###
Author <- "Heckel Dziendzielewski., et al"
Institution <- "GBIF-CICESE"
Area <- "Pacific"
Region <- "Gulf of California" 
Location <- "Bahia de los Angeles"
Dataset_Title <- "Diversidad, distribucion y abundancia de cetaceos en Bahia de los Angeles y Canal de Ballenas, Golfo de California: bases cientificas para una nueva area de observacion turistica de mamiferos marinos"
Reference <- "https://doi.org/10.15468/cwwjak"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Kuby ####

Kuby <- Kuby %>% 
  filter(countrycode == "MX")

write.csv(Kuby,
          "Kuby.csv",
          col.names = FALSE)

## FUNCION ###
Titulo <- "Kuby"
Inicio <- "Registro de"
Fin <- "en la coleccion de la U. de Kansas"
Keywords <- "Ichthyologia; Peces; KUBI; Coleccion"

## TEMPLATE ###
Author <- "Bentley., et al"
Institution <- "GBIF-U.Kansas"
Area <- "National"
Region <- "" 
Location <- ""
Dataset_Title <- "KUBI Ichthyology Collection"
Reference <- "https://doi.org/10.15468/mgjasg"
User_Contact <- "Andrew Bentley; abentley@ku.edu" 


GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Bacterias_BC ####

## FUNCION ###
Titulo <- "Bacterias_BC"
Inicio <- "Presencia de"
Fin <- "en Bahia Concepcion (BCS)"
Keywords <- "Bacterias; Fototrofas; Comunidad; Microbios; Bentos"

## TEMPLATE ###
Author <- "Lopez Cortes, et al"
Institution <- "GBIF-CIBNOR"
Area <- "Pacifico"
Region <- "Gulf of California" 
Location <- "Bahia Concepcion"
Dataset_Title <- "Diversidad de bacterias fototrofas en comunidades microbianas bentonicas de Bahia Concepcion, BCS, Mexico"
Reference <- "https://doi.org/10.15468/b4gnla"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx"

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Aves_Paz ####

## FUNCION ###
Titulo <- "Aves_Paz"
Inicio <- "Presencia de"
Fin <- "en la ensenada de La Paz (BCS)"
Keywords <- "Riqueza especifica; Distribucion; Abundancai; Aves"

## TEMPLATE ###
Author <- "Carmona Pina, et al"
Institution <- "GBIF-UABCS"
Area <- "Pacifico"
Region <- "Gulf of California" 
Location <- "Ensenada de La Paz"
Dataset_Title <- "Riqueza especifica, distribucion y abundancia de aves acuaticas en la ensenada de La Paz, Baja California Sur, Mexico"
Reference <- "https://doi.org/10.15468/wricli"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx"

GBIF_Create(Inicio,Fin,Keywords,Titulo)

  # ______________ Arrecifes ####

## FUNCION ###
Titulo <- "Arrecifes"
Inicio <- "Caracterizacion de"
Fin <- "en Mahahual"
Keywords <- "Corales; Sistema; Arrecifal; Mesoamericano; pesquerias; Turismos; Flora; Fauna; Mahahual"

## TEMPLATE ###
Author <- "Merediz Alonso, et al"
Institution <- "GBIF-AmigosSianKaan"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe"
Location <- "Mahahual"
Dataset_Title <- "Caracterizacion y monitoreo de la condicion arrecifal en cinco areas naturales protegidas y un area de influencia de Quintana Roo, Mexico: Primera etapa. Version 1.3"
Reference <- "https://doi.org/10.15468/yx7eck"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Aves_SJ ####

## FUNCION ###
Titulo <- "Aves_SJ"
Inicio <- "Abundancia de"
Fin <- "en Isla San Jose (BCS)"
Keywords <- "Aves marinas; Aves terrestres; Abundancia; riqueza especifica"

## TEMPLATE ###
Author <- "Carmona Pina, et al"
Institution <- "GBIF-UABCS"
Area <- "Pacific"
Region <- "G. of California"
Location <- "Isla San Jose"
Dataset_Title <- "Riqueza especifica, distribucion y abundancia de aves terrestres y marinas en Isla San Jose, Golfo de California, Baja California Sur, Mexico"
Reference <- "https://doi.org/10.15468/u0yv2s"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Peces_Tax ####

## FUNCION ###
Titulo <- "Peces_Tax"
Inicio <- "Estudio taxonomico de"
Fin <- ""
Keywords <- "Peces; taxonomia; zoogeografia; Laguna Madre; Costa Tamaulipas; Veracruz; Sonda Campeche; Justo Sierra; Barcos Camaroneros"

## TEMPLATE ###
Author <- "Lozano Vilano, et al"
Institution <- "GBIF-UANL"
Area <- "Atlantic"
Region <- ""
Location <- ""
Dataset_Title <- "Estudio taxonomico y zoogeografico de areas selectas de peces marinos en Laguna Madre y Costas de Tamaulipas, Veracruz y Campeche, Mexico. Version 1.3"
Reference <- "https://doi.org/10.15468/r6mfvp"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Ictiofauna_PC ####

## FUNCION ###
Titulo <- "Ictiofauna_PC"
Inicio <- "Caracterizacion de"
Fin <- "en Punta Carrizal (Col)"
Keywords <- "Peces; taxonomia; estructura; ictiofauna; arrecifes rocosos; arrecifes coralinos; diversidad"

## TEMPLATE ###
Author <- "Chavez Comparan, et al"
Institution <- "GBIF-UCOL"
Area <- "Pacific"
Region <- "Central Pacific"
Location <- "Punta Carrizal"
Dataset_Title <- "Caracterizacion y estructura de la ictiofauna de arrecifes rocosos y coralinos en Punta Carrizal, Colima, Mexico para fines de inventario y conservacion de la diversidad animal"
Reference <- "https://doi.org/10.15468/xqft0m"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Flora_Complejo ####

## FUNCION ###
Titulo <- "Flora_Complejo"
Inicio <- "Presencia de"
Fin <- "en Baja California Sur"
Keywords <- "Flora;Clorophyta; Phaeophyta; Rodophyta; conspicua; Echinodermata; Mollusca; Polychaeta; Complejo Insular Espiritu Santo-Cerralvo-San Jose"

## TEMPLATE ###
Author <- "Herrero Perezrul, et al"
Institution <- "GBIF-IPN"
Area <- "Pacific"
Region <- "G. of Clifornia"
Location <- "Complejo Insular Espiritu Santo-Cerralvo-San Jose"
Dataset_Title <- "Distribución espacio-temporal de aves playeras y su relación con los invertebrados bentónicos en la Reserva de la Biósfera Marismas Nacionales, Nayarit, México"
Reference <- "https://doi.org/10.15468/p0ekvb"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)


# ______________ Aves_Mar_Nac ####

## FUNCION ###
Titulo <- "Aves_Mar_Nac"
Inicio <- "Relacion de"
Fin <- "con invertebrados benticos"
Keywords <- "aves playeras; Reserva de la Biosfera; riqueza; abundancia; distribucion"

## TEMPLATE ###
Author <- "Carmona Pina, et al"
Institution <- "GBIF-CONABIO"
Area <- "Pacific"
Region <- "G. of Clifornia"
Location <- "Reserva de la Biosfera Marismas Nacional"
Dataset_Title <- "Distribucion espacio-temporal de aves playeras y su relación con los invertebrados bentonicos en la Reserva de la Biosfera Marismas Nacionales, Nayarit, Mexico"
Reference <- "https://doi.org/10.15468/cmen4f"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Cangrejo ####

## FUNCION ###
Titulo <- "Cangrejo"
Inicio <- "Presencia de"
Fin <- "en Golfo de Mexico"
Keywords <- "Cangrejo; Anomuro; Branquiuros; Laguna Costera; Litoral"

## TEMPLATE ###
Author <- "Raz-Guzman, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "W. G. of Mexico"
Location <- ""
Dataset_Title <- "Catalogo de cangrejos anomuros y braquiuros de las lagunas costeras de mayor extension en el litoral mexicano del Golfo de Mexico"
Reference <- "https://doi.org/10.15468/kd8uxs"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ SAV ####

## FUNCION ###
Titulo <- "SAV"
Inicio <- "Condicion actual de"
Fin <- "en Sistema Arrecifal Veracruzano"
Keywords <- "Sistema Arrecifal Veracruzano; Corales; Monitoreo; Comunidad; SAV; Pertubacion"

## TEMPLATE ###
Author <- "Horta Puga, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "W. G. of Mexico"
Location <- "Sistema Arrecifal Veracruzano"
Dataset_Title <- "Sistema Arrecifal Veracruzano: condicion actual y programa permanente de monitoreo: Primera Etapa"
Reference <- "https://doi.org/10.15468/ygb95w"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Ictio ####

## FUNCION ###
Titulo <- "Ictio"
Inicio <- "Registro de"
Fin <- ""
Keywords <- "Ictiofauna; Region Marina Prioritaria; Peces Marinos; Coleccion Ictiologica"

## TEMPLATE ###
Author <- "Cruz Aguero, et al"
Institution <- "GBIF-CONABIO"
Area <- "National"
Region <- ""
Location <- ""
Dataset_Title <- "Registros ictiofaunisticos de localidades selectas de 10 regiones marinas prioritarias del Pacifico Mexicano"
Reference <- "https://doi.org/10.15468/mezqik"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Elasmobranquios ####

## FUNCION ###
Titulo <- "Elasmobranquios"
Inicio <- "Registro de"
Fin <- "en el Golfo de Tehuantepec"
Keywords <- "Ictiofauna; peces cartilaginosos; Chondrichthyies; Tiburones; Rayas; quimeras"

## TEMPLATE ###
Author <- "Castillo Geniz, et al"
Institution <- "GBIF-CONABIO"
Area <- "Pacific"
Region <- "South Pacific"
Location <- "Golfo de Tehuantepec"
Dataset_Title <- "Elasmobranquios del Golfo de Tehuantepec, litoral chiapaneco"
Reference <- "https://doi.org/10.15468/omucft"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Poliquetos_UANL ####

## FUNCION ###
Titulo <- "Poliquetos_UANL"
Inicio <- "Registro de"
Fin <- ""
Keywords <- "Poliqueto; Co"

## TEMPLATE ###
Author <- "Leon Gonzalez, et al"
Institution <- "GBIF-CONABIO"
Area <- "National"
Region <- ""
Location <- ""
Dataset_Title <- "Actualizacion de la coleccion poliquetologica de la Universidad Autonoma de Nuevo Leon"
Reference <- "https://doi.org/10.15468/x5e1h1"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Ostracodos ####

## FUNCION ###
Titulo <- "Ostracodos"
Inicio <- "Registro de"
Fin <- ""
Keywords <- "Ostracodos; catalogo; Inventario; Mar Profundo; superficie; Zona Economica Exclusiva"

## TEMPLATE ###
Author <- "Leon Gonzalez, et al"
Institution <- "GBIF-CONABIO"
Area <- "National"
Region <- ""
Location <- ""
Dataset_Title <- "Inventario y catalogo de ostracodos recientes de los mares mexicanos
"
Reference <- "https://doi.org/10.15468/lihjgf"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Poliquetos_UANL ####

## FUNCION ###
Titulo <- "Poliquetos_UANL_I"
Inicio <- "Registro de"
Fin <- "de la coleccion de la FcB de la UANL"
Keywords <- "Poliquetos; Catalogo; Invertebrados; Atropodos"

## TEMPLATE ###
Author <- "Leon Gonzalez, et al"
Institution <- "GBIF-CONABIO"
Area <- "National"
Region <- ""
Location <- ""
Dataset_Title <- "La Coleccion de Poliquetos (Annelida: Polychaeta) de la Facultad de Ciencias Biologicas, Universidad Autonoma de Nuevo Leon"
Reference <- "https://doi.org/10.15468/sn6e04"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Copepodos ####

## FUNCION ###
Titulo <- "Copepodos"
Inicio <- "Registro de"
Fin <- "en la costa oriental de la Pen. Yucatan"
Keywords <- "Copepodos; Crustaceos; pelagico;" 

## TEMPLATE ###
Author <- "Suarez Morales, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe"
Location <- "Caribe"
Dataset_Title <- "Los copepodos (crustacea) pelagicos de la costa oriental de la Peninsula de Yucatan"
Reference <- "https://doi.org/10.15468/oztiwh"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Eufasidos ####

## FUNCION ###
Titulo <- "Eufasidos"
Inicio <- "Registro de"
Fin <- ""
Keywords <- "Eufasido; Malacostraca; Custraceo; Zooplancton" 

## TEMPLATE ###
Author <- "Gasca Serrano, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- ""
Location <- ""
Dataset_Title <- "Base de datos y la coleccion de eufausidos (Euphausiacea: Malacostraca: Crustacea) del Atlantico mexicano"
Reference <- "https://doi.org/10.15468/jvcqjg"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Crustaceos_Inv ####

## FUNCION ###
Titulo <- "Crustaceos_Inv"
Inicio <- "Registro de"
Fin <- "como especie invasora del Sistema Arrecifal Veracruzano"
Keywords <- "Catalogo; crustaceos; anfipodos; invasores; Parque Nacional Sistema Arrecifal Veracruzano; PNSAV; SAV"

## TEMPLATE ###
Author <- "Winfield Aguilar, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "W. G. of Mexico"
Location <- "Sistema Arrecifal Veracruzano"
Dataset_Title <- "Catalogo de las especies de crustaceos anfipodos invasores del Parque Nacional Sistema Arrecifal Veracruzano (PNSAV) y la actualizacion de la base de datos (CONABIO) de los anfipodos en Mexico"
Reference <- "https://doi.org/10.15468/ky1oeo"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Manglar_Yucatan ####

## FUNCION ###
Titulo <- "Manglar_Yuc"
Inicio <- "Monitoreo de"
Fin <- "en Yucatan"
Keywords <- "Manglar; Mangle; Caracterizacion; monitoreo; Costa"

## TEMPLATE ###
Author <- "Herrera SIlveira, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe"
Location <- "Yucatan"
Dataset_Title <- "Programa regional para la caracterizacion y el monitoreo de ecosistemas de manglar del Golfo de Mexico y Caribe Mexicano: Peninsula de Yucatan"
Reference <- "https://doi.org/10.15468/9ecxkr"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Ictiofauna_humed ####

## FUNCION ###
Titulo <- "Ictiofauna_humed"
Inicio <- "Estatus Ecologico de"
Fin <- "en humedales costeros del noroeste de BC"
Keywords <- "Ictiofauna; Humedales; Costeros; Bocanas; marismas; Area Marina Prioritaria; Amenazada A1; Ensenadense"

## TEMPLATE ###
Author <- "Ruiz Campos, et al"
Institution <- "GBIF-CONABIO"
Area <- "Pacific"
Region <- "G. of California"
Location <- "Ensenadense"
Dataset_Title <- "Estatus ecologico y distribucion de la ictiofauna de humedales costeros (bocanas y marismas) en el noroeste de Baja California México (Area marina prioritaria amenazada A1: Ensenadense)"
Reference <- "https://doi.org/10.15468/8e7deh"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Tanaidaceos ####

## FUNCION ###
Titulo <- "Tanaidaceos"
Inicio <- "Presencia de"
Fin <- "en el G. de Mexico y Caribe"
Keywords <- "Tanaidaceos; Crustaceos; Peracarida; Poliquetos"

## TEMPLATE ###
Author <- "Suarez Morales, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "B. of Campeche Caribe"
Location <- ""
Dataset_Title <- "Bases de datos de Tanaidaceos (Crustacea: Peracarida) del Mar Caribe mexicano y Poliquetos pelagicos del Golfo de Mexico y Mar Caribe mexicano"
Reference <- "https://doi.org/10.15468/8bcbag"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)

# ______________ Cangrejo_II ####

## FUNCION ###
Titulo <- "Cangrejo_I"
Inicio <- "Presencia de"
Fin <- "en Golfo de Mexico"
Keywords <- "Cangrejo; Anomuro; Branquiuros; Laguna Costera; Litoral"

## TEMPLATE ###
Author <- "Raz-Guzman, et al"
Institution <- "GBIF-CONABIO"
Area <- "Atlantic"
Region <- "W. G. of Mexico"
Location <- ""
Dataset_Title <- "Catalogo de cangrejos anomuros y braquiuros de las lagunas costeras de mayor extension en el litoral mexicano del Golfo de Mexico"
Reference <- "https://doi.org/10.15468/atuln7"
User_Contact <- "Sonia Alejandra Careaga Olvera; scareaga@conabio.gob.mx" 

GBIF_Create(Inicio,Fin,Keywords,Titulo)


####____________ Merge EVERYTHING ####


#### STILL NOT WORKING ####

# First correcting the MMID column

### STEP DONE ###
for(i in 35:nrow(Data_Guide)){
  Name <- paste(Data_Guide[i,1])
  xx <- read_csv(
    paste("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Final_Data/",
          Name,
          sep=""),
    col_types = cols(X1 = col_skip())
  )%>% 
    mutate(MMID="") %>% 
    select(MMID,everything())
  
  write.csv(x,
            Name,
            row.names = F)
}

####____________________________________

# Now merging everything
xx <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Final_Data/Arrecifes_BC_Final.csv")
Data_Guide <- fread("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Final_Data/Data_Guide.csv") %>% 
  select(Final_List)
x <- NULL

for(i in 1:nrow(Data_Guide)){
  # Name <- paste(Data_Guide[i])
  Liga <- paste("~/Documents/Dropbox/Metadata_Mexico/Datasets/GBIF/Final_Data/",
                Data_Guide$Final_List[i],
                sep="")
  
  x <- fread(Liga)
  x <- x %>% 
    mutate(File = paste(Data_Guide[i])) %>% 
    filter(!is.na(Short_Title))
  
  x <- data.table(x)
    
  # print(Liga)
# }
  
  if(i == 1){
    Data <- copy(x) # <- copies the previouse data.table
  }else{ 
    setkey(Data, # <- sets the data.table as "reference" ?setkey
           Area); setkey(x,
                                Area); 
    list <- list(Data, # <- creates a list to merge the tables
                 x)
    Data <- rbindlist(list, #<- Merges all the data in one single file.
                       use.names = TRUE, # <- This will merge columns by Name
                       fill = FALSE, # <- This allows for the NA's
                       idcol =  NULL)  # Columns id
  }
}

unique(Data$File)

Data <- select(Data, -File)

write.csv(Data,
      "Data.csv",
      row.names = F)
#### __________________________________FIN DE GBIF __________________________________ ####

#### Money realy, money ####

Template_3_4 <- fread("~/Documents/Github/Meta_Data_Mexico/App/Template_4.1.csv")

Money <- Template_3_4 %>% 
  group_by(Compilation_Title) %>% 
  summarise(n())

Money <- Template_3_4 %>% 
  filter(is.na(Research_Fund)) %>% 
  group_by(Compilation_Title) %>% 
  summarise(n())

#### Network analysis ###

# Eventualmente...


Category <- data.table::data.table(Name=c(
  "AAcademic",
  "Aquaculture",
  "Goverment",
  "Inter. Gov (IGO)",
  "International",
  "NGO",
  "Unknown",
  "Conservation",
  "Ecology",
  "Fisheries",
  "Oceanography",
  "Sociology",
  "Turism",
  "Other",
  "NGO Funding",
  "Private Funding",
  "Goverment Fu",
  "ACA_F",
  "Inter. Gov (IGO) Funding",
  "International Funding"
)
) %>% 
  arrange(Name)

# The rest of the needed information
Template <- Template_3_4

#First Research Funding
R_Fund_Org <-Template %>%
  group_by(Research_Fund,Institution_Type) %>%
  summarise(Value =n()) %>%
  rename(Source = Research_Fund,
         Target = Institution_Type)

Inst_Field <-Template %>%
  filter(!is.na(Institution_Type)) %>%
  group_by(Institution_Type,Research_Field) %>%
  summarise(Value = n()) %>%
  rename(Source = Institution_Type,
         Target = Research_Field)

Final_Table <- R_Fund_Org %>%
  bind_rows(Inst_Field) 

Final_Table <- data.frame(ID = seq(1:nrow(Final_Table))) %>% 
  bind_cols(Final_Table)

Final_Table_N <- Final_Table %>% 
  gather("Category","Character",2:3) %>% 
  arrange(Character)

Final_Table_N$Character <- as.integer(as.factor(Final_Table_N$Character))
Final_Table_N$Character <- as.numeric(as.integer(Final_Table_N$Character)-1)

Source <- Final_Table_N %>% 
  filter(Category == "Source") %>% 
  select(-Category) %>% 
  rename(Source = Character)

ATarget <- Final_Table_N %>% 
  filter(Category == "Target") %>% 
  select(-Category,-Value) %>% 
  rename(Target = Character) %>% 
  left_join(Source,
            by ="ID")


sankeyNetwork(Links = ATarget, #Dataset with Source, Target and value
              Nodes = Category, #Dataset withe the Names
              Source = "Source", #Source column in Links dataset
              Target = "Target", #Target column in Links dataset
              Value = "Value", # The amount to plot from the Links dataset
              NodeID = "Name", #What's showing when mouse over Node
              units = "Records", #Units to show
              fontSize = 12,
              nodeWidth = 30)



#### ____________ DATOS.GOV _______________ ####

## SCT- Infraestructura arino Portuaria ##
Dinoflagelados_Marinos <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/DatosAbiertosGov/CIBNOR/Dinoflagelados_Marinos.csv") %>% 
  separate(`FECHA DE AISLAMIENTO`, c("day", "month", "year"), sep = "/")


Dino <- Dinoflagelados_Marinos %>%
  filter(LOCALIDAD != "CUBA") %>% 
  group_by(ESPECIE,LOCALIDAD) %>% 
  summarise(Ymin = min(year, na.rm=t),
            Ymax = max(year),
            nYear = length(unique(year))) %>% 
  mutate(Titulo = paste("Presencia de",ESPECIE, "en",LOCALIDAD)) %>% 
  mutate(Key = paste(LOCALIDAD, "Dinoflagelados; Coleccion; semestral; Noroeste"))
  

Titulos <- data.frame(
  Titul= c(
"Temperatura del agua",
"Radiacion solar",
"Direccion viento",
"Magnitud viento",
"Temperatura del aire",
"Humedad relativa",
"Presion atmosferica",
"Precipitacion",
"Nivel del mar 3"
))

Final <- Titulos %>% 
  mutate(Ti = paste(Titul,"en la Bahia de Baja California (2016)")) %>% 
  mutate(Key = paste("Mes; Dia; Hora; Minuto; Segundo; Temperatura; estacio mareografica; Observatorio; Mareas; Costas"))


### CNH ###
Gas <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/DatosAbiertosGov/Gas.csv")

CNH <- Gas %>% 
  group_by(activo,region) %>% 
  summarise(n=n()
            )

### CONANP ####

CONANP <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/DatosAbiertosGov/CONANP_D.csv")

CO <- CONANP %>% 
  mutate(Titulo = paste("Superficie Certificada del", `Nombre del area`)) %>% 
  mutate(Titulo_I = paste("Fecha y plazo de Certificacion de", `Nombre del area`)) %>% 
  select(Titulo,
         Titulo_I)

Areas <- CONANP %>% 
  group_by(Estado) %>% 
  summarise(n()) %>% 
  mutate(x = paste("Areas con Manglar destinadas Voluntariamente a la Conservacion en",Estado))


### Sitios Ramsar
path.ne.coast <- ("/Users/jpalacios/Downloads/Cobertura_Sitios_Ramsar")
file_name <- "Sitios_Ramsar_Geo_ITRF92_2015.shp"

# Loading the shapefile:
data_coast <- readOGR(dsn = path.ne.coast, 
                      layer = file_path_sans_ext(file_name))

x <- fortify(data_coast)
ggplot() + 
  geom_path(data = x, 
            aes(x = long, y = lat, group = group), 
            color = "black",
            size = 0.25) + 
  coord_map(projection = "mercator") 


### ANP ##

RB <- c("Sian Kaan",
"Alto Golfo de California",
"Banco Chinchorro",
"Isla del Golfo de California",
"Ria Celestun",
"Ria Lagartos",
"Arrecife Alacranes",
"Huatulco",
"Laguna madre y Delta de Rio Bravo",
"Pantanos de Centla",
"Sistema Arrecifal Veracruzano",
"Islas Marietas",
"Islas Marias",
"Isla Cozumel",
"Sian Ka'an"
)

ANP_L <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/DatosAbiertosGov/ANP_Loc.csv")
ANPS <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/DatosAbiertosGov/listado_ANPS.csv") %>% 
  filter(Sup_Marina >= 1) %>% 
  filter(!is.na(Programa)) %>% 
  mutate(x = paste("Estudio Previo para la determinacion del ANP",ANP)) %>% 
  select(x,ANP,Programa, Categoria, Municipios) %>% 
  left_join(ANP_L,
            by="ANP") %>% 
  mutate(Key = paste(Categoria,Municipios,
                     sep="; ")
  ) %>% 
  select(Key)

write.csv(ANPS,
          "ANPS_key.csv",
          row.names = FALSE)

unique(Template$Region)


#### Indicadores de desempeño de recursos pesqueros @@@@

Indicadores <- Template %>% 
  filter()


### RAMSAR ##

Mexico_Politico <- read_csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Data/Mexico_Politico.csv")

MP <- Mexico_Politico %>% 
  filter(Region_M != "Freshwater/Terrestrial") %>% 
  rename(Estado = Location)

RAMSAR <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/DatosAbiertosGov/RAMSAR.csv") 

Ram <- RAMSAR%>% 
  filter(!is.na(`Sitio Ramsar`)) %>% 
  mutate(Titulo = paste("Superficie de",`Sitio Ramsar`)) %>% 
  select(Titulo, Estado) %>% 
  semi_join(MP,
            by = "Estado")



#### CONAPESCA ####

tolower(Acciones_Inspeccion_Vigilancia$Estado)

Acciones_Inspeccion_Vigilancia$Estado <- tolower(Acciones_Inspeccion_Vigilancia$Estado)
Mexico_Politico$Location <- tolower(Mexico_Politico$Location)



Inspeccion <- Acciones_Inspeccion_Vigilancia %>% 
group_by(Actividad,
           Region,
           Estado) %>% 
  summarise(n()) %>% 
  mutate(Titulo_1 = paste(Actividad, "en", Region, "de", Estado,
                          sep=" "),
         Key_1 = paste("Inspeccion; Vigilancia; Monitoreo; Pesquerias; IUU")
         ) %>% 
  select(Titulo_1,
         Key_1,
         Estado) %>% 
  rename(Location = Estado) %>% 
  left_join(Mexico_Politico,
            by ="Location")

RO_VP_sistemas_producto$Nombre.Municipio <- tolower(RO_VP_sistemas_producto$Nombre.Municipio)

SPR <- RO_VP_sistemas_producto %>% 
  mutate(Location = tolower(Nombre.Estado)) %>% 
  group_by(Programa,
           Location,
           Pesqueria) %>% 
  summarise(Moni = paste(unique(Nombre.Municipio),
                         collapse = "; "),
            Compo = paste(unique(Componente),
                         collapse = "; ")
            ) %>% 
  mutate(
         T1 = paste("Beneficiarios del programa:", Programa, "en", Location,
         sep = " "),
         T2 = paste("Recurso federal destinado a Beneficiarios del programa", Programa, "en", Location,
                    sep = " "),
         Key1 = paste("Subsidios; Programas; Apoyo; Pescadores; Cadena Productiva",Compo, Moni,
                      sep = "; ")
  ) %>% 
  left_join(Mexico_Politico,
            by="Location") %>% 
  select(T1,
         T2,
         Key1,
         Location, 
         Area_M,
         Region_M,
         Pesqueria)


Arte_Pesca <- ArtePesca_2011 %>% 
  group_by(
    Nombre.Estado,
    Pesqueria
  ) %>% 
  summarise(Moni = paste(unique(Nombre.Municipio),
                         collapse = "; "),
            Loc = paste(unique(Nombre.Municipio),
                        collapse = "; "),
            Compo = paste(unique(Componente),
                          collapse = "; ")
  ) %>% 
  mutate(
    Titulo = paste("Beneficiarios de programa de Modernizacion de Embarcaciones Pesqueras Menores en", Nombre.Estado, "(2011)"),
    Key = paste("Pequena escala; Embarcaciones menores; Subsidio",Moni,Loc,Compo,
                sep="; ")
  ) %>% 
  rename(Location = Nombre.Estado) %>%
  filter(Location != "*") %>% 
  select(
    Titulo,
    Key,
    Location,
    Pesqueria
  )

Motores <- Motor_2008.2016 %>% 
  filter(Nombre.Estado != "*") %>% 
  group_by(
    Nombre.Estado,
    Pesqueria
  ) %>% 
  summarise(Moni = paste(unique(Nombre.Municipio),
                         collapse = "; "),
            # Loc = paste(unique(Nombre.Municipio),
            #             collapse = "; "),
            Compo = paste(unique(Componente),
                          collapse = "; ")
  ) %>% 
  mutate(
    Titulo = paste("Beneficiarios de programa de Sustitucion de Motores en", Nombre.Estado, "(2008-2016)"),
    Key = paste("Pequena escala; Embarcaciones menores; Subsidio; MotorM 4 tiempos; 115 HP; fuera de borda",Moni,Compo,
                sep="; ")
  ) %>% 
  rename(Location = Nombre.Estado) %>% 
  select(
    Titulo,
    Key,
    Location,
    Pesqueria
  )

write.csv(Motores,
      "Motores.csv",
      row.names= FALSE)
  


GPS <- GPS_Data %>% 
  filter(Nombre.Estado != "*") %>% 
  group_by(
    Nombre.Estado,
    Pesqueria
  ) %>% 
  summarise(Moni = paste(unique(Nombre.Municipio),
                         collapse = "; "),
            # Loc = paste(unique(Nombre.Municipio),
            #             collapse = "; "),
            Compo = paste(unique(Componente),
                          collapse = "; ")
  ) %>% 
  mutate(
    Titulo = paste("Beneficiarios de programa de adquisicion de GPS en", Nombre.Estado, "(2011 - 2016)"),
    Key = paste("GPS; Comunicacion Satelital; Embarcaciones menores; Subsidio",Moni,Compo,
                sep="; ")
  ) %>% 
  rename(Location = Nombre.Estado) %>% 
  select(
    Titulo,
    Key,
    Location,
    Pesqueria
  )

write.csv(GPS,
          "GPS.csv",
          row.names= FALSE)


Localidad <- GPS %>% 
  select(Location) %>% 
  left_join(Mexico_Politico,
            by = "Location")


Beneficiarios_emb_mayores$NOMBRE.ESTADO <- tolower(Beneficiarios_emb_mayores$NOMBRE.ESTADO)
Mexico_Politico$Location <- tolower(Mexico_Politico$Location)

Emb_Mayores <- Beneficiarios_emb_mayores %>% 
  group_by(NOMBRE.ESTADO
           ) %>% 
  summarise(Moni = paste(unique(NOMBRE.MUNICIPIO),
                         collapse = "; "),
            # Loc = paste(unique(Nombre.Municipio),
            #             collapse = "; "),
            Compo = paste(unique(COMPONENTE),
                          collapse = "; ")
  ) %>% 
  mutate(
    Titulo = paste("Beneficiarios de programa de modernizacion de flota mayor en", NOMBRE.ESTADO, "(2012 - 2015)"),
    Key = paste("Modernizacion; Subsidio; Embarcaciones mayores; Industrial;",Moni,Compo,
                sep="; ")
  ) %>% 
  rename(Location = NOMBRE.ESTADO) %>% 
  left_join(Mexico_Politico,
            by = "Location") %>% 
  select(
    Titulo,
    Key,
    Location,
    Area_M,
    Region_M
  ) %>% 
  filter(!is.na(Area_M))
  
Produccion <- Produccion_2014 %>% 
  group_by(ENTIDAD,
           NOMBRE_CIENTIFICO,
           ORIGEN
           ) %>% 
  summarise(
    Comun = paste(unique(NOMBRE_COMUN),
                  collapse = "; "),
    Ofi = paste(unique(OFICINA),
                  collapse = "; "),
    Princi = paste(unique(NOMBREPRINCIPAL),
                   collapse = "; ")
  ) %>% 
  mutate(
    T1 = paste(ORIGEN, "(peso vivo) de", NOMBRE_CIENTIFICO, "en", ENTIDAD,
               sep = " "),
    T2 = paste(ORIGEN, "(peso deembarcado) de", NOMBRE_CIENTIFICO, "en", ENTIDAD,
               sep = " "),
    T3 = paste("Valor de",ORIGEN, "de", NOMBRE_CIENTIFICO, "en", ENTIDAD,
               sep = " "),
    K1 = paste(Comun, Ofi, Princi, "Estadistica oficial; Pesca; Produccion; Captura")
  ) %>% 
  select(
    ENTIDAD,
    NOMBRE_CIENTIFICO,
    K1,
    T1,
    T2,
    T3
  ) %>% 
  tidyr::gather("TItulo",
                "Short_Title",
                4:6) 

#

Vigilancia <- invenctivos %>% 
  group_by(ESTADO
           ) %>% 
  summarise(
    Min_Y = min(ANO),
    Max_Y = max(ANO),
    Compo = paste(unique(COMPONENTE),
                  collapse = "; "),
    Pro = paste(unique(PROGRAMA),
                collapse = "; ")
    ) %>% 
  mutate(
    Titulo = paste("Beneficiarios de programas de incentivos de inspeccion y vigilancia en", ESTADO,
    sep = " "),
    TituloB = paste("Monto otorgado de programas de incentivos de inspeccion y vigilancia en", ESTADO,
                   sep = " "),
    Key = paste(Compo, Pro, "Subsidios; Pesca; IIU; Vigilancia; Monitoreo; Inspecicon; Aquicola")
  ) %>% 
  rename(Location = ESTADO) %>% 
  left_join(Mexico_Politico,
            by ="Location")


#   
Bene <- Beneficiarios_obras_estudios %>% 
  group_by(NOMBRE.ESTADO,
           NOMBRE.MUNICIPIO
           ) %>% 
  summarise(
    Pro = paste(unique(PROGRAMA),
                collapse = "; "),
    Con = paste(unique(CONCEPTO),
                collapse = "; ")
  ) %>% 
  mutate(
    T1 = paste("Beneficiarios de programa de obras y estudios en",NOMBRE.MUNICIPIO,",",NOMBRE.ESTADO),
    T2 = paste("Montos Otorgados del programa de obras y estudios en",NOMBRE.MUNICIPIO,",",NOMBRE.ESTADO),
    key = paste(Pro,Con,"Subsidio; CONAPESCA; Pesca; Equipamento; Infraestructura", sep = "; ")
  )

LoLo <- lolo%>% 
  left_join(Mexico_Politico,
            by ="Location")


#### INCAPESCA ####

DICTAMENES <- read_csv("~/Downloads/DICTAMENES.csv", 
                       col_types = cols(Ano_del_dictamen = col_number()))

# Primero los que tienen nombre cientifico
DIC <- DICTAMENES %>% 
  filter(Nombre_Scientifico != "") %>%
  group_by(
    Nombre_Scientifico,
    area
           ) %>% 
  summarise(
    Centro = paste(unique(Nombre_del_Centro_Regional),
    collapse = ";"),
    Nombre_Comun = paste(unique(Nombre_Comun),
                   collapse = ";"),
    Amin = min(Ano_del_dictamen),
    Amax = max(Ano_del_dictamen),
    Nano = length(unique(Ano_del_dictamen))
  ) %>% 
  mutate(
    Titulo = paste("Listado de Dictamenes Tecnicos para",Nombre_Scientifico, "del",area),
    Key = paste("Pesca; Dictamenes; Manejo Pesquero",Nombre_Comun,
                sep = "; ")
  )

WDIC_b <- DICTAMENES %>% 
  filter(is.na(Nombre_Scientifico)) %>%
  group_by(
    Nombre_Comun,
    area
  ) %>% 
  summarise(
    Centro = paste(unique(Nombre_del_Centro_Regional),
                   collapse = ";"),
    Amin = min(Ano_del_dictamen),
    Amax = max(Ano_del_dictamen),
    Nano = length(unique(Ano_del_dictamen))
  ) %>% 
  mutate(
    Titulo = paste("Listado de Dictamenes Tecnicos para",Nombre_Comun, "del",area),
    Key = paste("Pesca; Dictamenes; Manejo Pesquero",Nombre_Comun,
                sep= "; ")
  )




#### CONABIO a CNP ###

### Problemas, el nivel de detalle para las especies no es el mismo. Almejas es un claro ejemplo

#### Hay que hacerlo por litoral, primero.
Template <- read_csv("~/Documents/Github/Meta_Data_Mexico/App/Template_4.1.csv")

Especies_CNP <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/CartaNac.Pesq/Especies_CNP.csv")

#### Pacifico ####

P_CNP <- Especies_CNP %>% 
  filter(Litoral == "Pacifico") %>% 
  select(Animal,
         Cientifico) %>% 
  rename(Subject_name = Animal)

CONAPESCA <- Template %>% 
  filter(Institution == "CONAPESCA") %>% 
  filter(Area == "Pacific") %>% 
  select(MMID,
         Subject_name) %>% 
  filter(!is.na(Subject_name))

Homologado <- CONAPESCA %>% 
  left_join(P_CNP,
            by = "Subject_name")

#### Completar faltantes de acuerdo a CNP ####


NA_Homo <- filter(Homologado,
                  is.na(Cientifico))

# Cmabiar nombres para que empaten los del Anuario

P_CNP$Subject_name <- replace(P_CNP$Subject_name, 
                              P_CNP$Subject_name== "Huachinango_Pargo", "Guachinango"
)

P_CNP$Subject_name <- replace(P_CNP$Subject_name, 
                              P_CNP$Subject_name== "Lisas", "Lisa"
)

P_CNP$Subject_name <- replace(P_CNP$Subject_name, 
                              P_CNP$Subject_name== "Tiburon", "Tiburon;Cazon"
)

Sardina <- Especies_CNP %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(str_detect(Comun,"Sardina")
  ) %>% 
  select(Cientifico) %>% 
  mutate(Subject_name = paste("Sardina")) %>% 
  group_by(Cientifico,
           Subject_name) %>% 
  summarise(n=n()) %>% 
  select(-n)
  

Bagre <- Especies_CNP %>% 
  filter(Litoral == "Pacifico") %>% 
  filter(str_detect(Comun,"Bagre")
  ) %>% 
  select(Cientifico) %>% 
  mutate(Subject_name = paste("Bagre")) %>% 
  group_by(Cientifico,
           Subject_name) %>% 
  summarise(n=n()) %>% 
  select(-n)


P_CNP_II <- P_CNP %>% 
  bind_rows(Sardina,
            Bagre)

### Volver a correr con problemas resueltos ##

#Homologados #

Homologado <- CONAPESCA %>% 
  left_join(P_CNP_II,
            by = "Subject_name") %>% 
  select(-Subject_name) %>% 
  filter(!is.na(Cientifico))

# Regresar homologados a Template de CONAPESCA #

New_CONAPESCA <- CONAPESCA %>% 
  left_join(Homologado,
            by = "MMID")

Spp <- New_CONAPESCA %>% 
  filter(!is.na(Cientifico)) %>% 
  select(-Subject_name) %>% 
  rename(Subject_name = Cientifico)

Final <- New_CONAPESCA %>% 
  filter(is.na(Cientifico)) %>% 
  select(-Cientifico) %>% 
  bind_rows(Spp) %>% 
  arrange(MMID)




#### Convertir Template de categoria para especie...

### First we deal with the Subject_Name
CONAPESCA_I <- Template %>% 
  filter(Institution == "CONAPESCA") %>% 
  filter(Area == "Pacific")

Merged_Table <- CONAPESCA_I %>% 
  left_join(Final,
            by = "MMID") %>% 
  rename(Subject_name = Subject_name.y) %>% 
  ### Now we deal with the Shott title...
  mutate(New_name = gsub(New_CONAPESCA$Subject_name,
                         New_CONAPESCA$Cientifico,
                         Merged_Table$Short_Title)) %>% 
  select(1,
         New_name,
         2:6,
         Subject_name,
         -Short_Title,
         -Subject_name.x,
         everything())

#### All dataset test...

#### Not working LOOOOOSER

### First we deal with the Subject_Name
Template_Bis <- Template

Merged_Table_Bis <- Template_Bis %>% 
  left_join(Final,
            by = "MMID") %>% 
  rename(Subject_name = Subject_name.y) %>% 
  ### Now we deal with the Shott title...
  mutate(New_name = gsub(New_CONAPESCA$Subject_name,
                         New_CONAPESCA$Cientifico,
                         Merged_Table_Bis$Short_Title)) %>% 
  select(1,
         New_name,
         2:6,
         Subject_name,
         -Short_Title,
         -Subject_name.x,
         everything())




#### Aquacultura MAC ####
Aquacultura <- read_csv("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis/Data/Aquacultura.csv")
View(Aquacultura)

AQ <- Aquacultura %>% 
  group_by(NOMBRE_OFICINA,
           NOMBRE_PRINCIPAL
           ) %>% 
  summarise(Min_Y = min(ANO_CORTE),
            Max_Y = max(ANO_CORTE),
            DATA.POINTS = length(unique(ANO_CORTE)),
            LOCALIZACION = paste(unique(NOMBRE_ESTADO, collapse="; "))
            ) %>% 
  mutate(ProduccionI = paste("Peso desembarcado de", NOMBRE_PRINCIPAL, "en",NOMBRE_OFICINA)) %>% 
  mutate(ProduccionII = paste("Peso Vivo de", NOMBRE_PRINCIPAL, "en",NOMBRE_OFICINA)) %>% 
  mutate(ProduccionIII = paste("Precio de", NOMBRE_PRINCIPAL, "en",NOMBRE_OFICINA)) %>% 
  mutate(ProduccionIIII = paste("Valor de", NOMBRE_PRINCIPAL, "en",NOMBRE_OFICINA)) %>% 
  mutate(Key_Pro = paste("Produccion; Acuacultura; Cosecha")) %>% 
  mutate(Key_Valor = paste("Produccion; Acuacultura; Cosecha; Valor; Economico")) %>% 
  gather("Uno","dos",7:10)

write.csv(AQ,
          "AQ.csv")

CONAPESCA_I <- CONAPESCA <- Template %>% 
  filter(Institution == "CONAPESCA") %>% 
  filter(Area == "Pacific")


New_Temp <- CONAPESCA_I %>% 
  left_join(New_CONAPESCA,
            by="MMID")

### Correccion de todos los nombres...

Template_Correct <- gnr_resolve(names = Template$Subject_name[1:50000], #Looks for homogenic names
                            best_match_only = TRUE,
                            canonical = TRUE)

TC <- data.table(Template_Correct)



#### Tiburones Angel Silvia Salas ####



Tiburcio <- Angel_tiburones %>% 
  group_by(Presentacion,
           Origen,
           Destino) %>% 
  summarise(n(),
            Sy = min(Ano),
            Ey = max(Ano),
            DP = length(unique(Ano))) %>% 
  mutate(Titulo = paste("Datos crudos de exportacion de", Presentacion,"(",Origen,"-",Destino,")"))

#### dataMares ####

Ecol <- ec_gc_ecological_monitoring %>% 
  group_by(Species,
           Region) %>% 
  summarise(
    Type = paste(unique(`Label (Species)`), collapse = "; "),
    Ymi = min(`Year of Date`),
    Ymax = max(`Year of Date`)
  ) %>% 
  mutate(
    T1 = paste("Abundancia de", Species, "en", Region),
    T2 = paste("Talla de", Species, "en", Region),
    K = paste(Type, "Monitoreo; Colecta; Talla; Auundancia",
              sep ="; ")
  ) %>% 
  select(-Type) %>% 
  tidyr::gather(
    "Titulo",
    "Title",
    5:6
  )

write.csv(Ecol,
          "Ecol.csv",
          row.names =FALSE)

# em_gc_healthindex

Helath <- em_gc_healthindex %>% 
  group_by(
    Region
  ) %>% 
  summarise(
    Ymi = min(Year),
    Ymax = max(Year),
    Keys = paste(unique(Arrecife),
                 collapse = "; "),
    Keys2 = paste(unique(Exploitation),
                 collapse = "; "),
    Keys3 = paste(unique(Habitat),
                 collapse = "; ")
    ) %>% 
  mutate(
    Key = paste(Keys,Keys2,Keys3,
                sep="; "),
    T1 = paste("Indice de salud de arrecifes rocosos del",Region,
                sep=" "), 
    T2 = paste("Abundancia relativa de erizos en arrecifes rocosos de",Region,
                  sep=" "),
    T3 = paste("Abundancia relativa de Estrellas en arrecifes rocosos de",Region,
                  sep=" "),
    T4 = paste("Abundancia relativa de peces en arrecifes rocosos de",Region,
                sep=" "),
    T5 = paste("Biomasa promedio de peces en arrecifes rocosos de",Region,
                sep=" "),
    T6 = paste("Biomasa promedio en arrecifes rocosos de",Region,
                sep=" ")
    ) %>% 
  select(-4:-6) %>% 
  tidyr::gather("Titulo",
                "Texto",
                5:10)
  
# em_gc_snappers_densities.xlsx

Densidades <- em_gc_snappers_densities %>% 
  group_by(
    Species,
    Region
  ) %>% 
  summarise(
    Sites = paste(unique(Site),
                  collapse = "; "),
    miny = min(Year),
    maxy = max(Year)
  ) %>% 
  mutate(
    Titulo = paste("Densidad de", Species, "en manglares de", Region),
    TituloI = paste("Tallas de", Species, "en manglares de", Region),
    TituloII = paste("Abundancia de", Species, "en manglares de", Region),
    Key = paste(Sites, "Densidad; Red Snapper; Pargo; Manglar")
  ) %>% 
  tidyr::gather("Titulo",
                "Texto",
                6:8) %>% 
  select(-Sites,
         -Titulo)
write.csv(Densidades,
          "Densidades.csv",
          row.names = FALSE)

# em_gc_snappers_distances

Pargo <- em_gc_snappers_distances %>% 
  group_by(Mangles.Sitio,
           Especie) %>% 
  summarise(
    Sitios = paste(unique(Sitio),
                   collapse = "; "
    ),
    miny = min(ANO),
    maxy = max(ANO)
  ) %>% 
  mutate(
    Titulo = paste("Densidad de", Especie, "en manglares de", Mangles.Sitio),
    TituloI = paste("Tallas de", Especie, "en manglares de", Mangles.Sitio),
    TituloII = paste("Abundancia de", Especie, "en manglares de", Mangles.Sitio),
    Key = paste(Sitios, "Densidad; Red Snapper; Pargo amarillo; Manglar")
  ) %>% 
  tidyr::gather("Titulo",
                "Texto",
                6:8)

write.csv(Pargo,
          "Pargo.csv",
          row.names = FALSE)

Inver <- em_hu_monitoreo_arrecifal_INVERTEBRADOS %>% 
  group_by(Especies
           ) %>% 
  summarise(
    miny = min(Anio),
    maxy = max(Anio)
  ) %>% 
  mutate(Titulo = paste("Talla de", Especies, "en Parque Nacional Bahia de Huatulco"),
         TituloI = paste("Abundancia de", Especies, "en Parque Nacional Bahia de Huatulco"),
         TituloII = paste("Densidad de", Especies, "en Parque Nacional Bahia de Huatulco"))

Peces <- read_excel("~/Documents/Dropbox/Metadata_Mexico/Datasets/dataMares/em_hu_monitoreo_arrecifal_PECES.xlsx")
View(Peces)

Peces_S <- Peces %>% 
  group_by(Especies
  ) %>% 
  summarise(
    miny = min(Anio),
    maxy = max(Anio)
  ) %>% 
  mutate(Titulo = paste("Talla de", Especies, "en Parque Nacional Bahia de Huatulco"),
         TituloI = paste("Abundancia de", Especies, "en Parque Nacional Bahia de Huatulco"),
         TituloII = paste("Biomasa de", Especies, "en Parque Nacional Bahia de Huatulco"))

Div_Fun <- read_excel("~/Documents/Dropbox/Metadata_Mexico/Datasets/dataMares/em_po_diversidad_funcional.xlsx")

Div <- Div_Fun %>% 
  group_by(Especie,
           `Sitio (Sitios)`) %>% 
  summarise(
    Nombre = paste(`Nombre espanol`,`Nombre ingles`, unique(Ecosistema),"Diversidad funcional",
                   sep  = "; ",
                   collapse = "; ")
  ) %>% 
  mutate(
    T1 = paste("Talla maxima de", Especie, "en",`Sitio (Sitios)`,", Ensenada (BC)"),
    T1 = paste("Numero de individuos de", Especie, "en",`Sitio (Sitios)`,", Ensenada (BC)")
  )

# em_po_fish_communities

Peces_G <- read_excel("~/Documents/Dropbox/Metadata_Mexico/Datasets/dataMares/em_po_fish_communities.xlsx")

Peces_Goc <- Peces_G %>% 
  filter(`ID Country` != "Ecuador") %>% 
  group_by(
    `ID Species`,
    `ID Region`
  ) %>% 
  summarise(
    Key= paste(unique(IDReefs2),"Monitoreo; Peces; Abundancia; Talla; Islas",
                sep= "; ",
                collapse = "; ")
  ) %>% 
  mutate(
    Title1 = paste("Abundancia de",`ID Species`,"en",`ID Region`),
    Title2 = paste("Tallas de",`ID Species`,"en",`ID Region`)
  )

# em_po_kelp

Kelp_Data <- read_excel("~/Documents/Dropbox/Metadata_Mexico/Datasets/dataMares/em_po_kelp.xlsx")

Kelp <- Kelp_Data %>%
  filter(Especie != "Especie X") %>% 
  group_by(Especie,
           Localidad) %>% 
  summarise(n()) %>% 
  mutate(
    Titulo = paste("Abundancia de",Especie,"en",Localidad)
      )

# dataMares_metadata





