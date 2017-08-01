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
setwd("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis")

#### _________________________________ ####

#### DATA ####

#### Template

Template <- read_csv("~/Documents/Github/Meta_Data_Mexico/App_Eng/Template.csv")
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

COBI <- read.csv("Data/Cobi_Imp.csv")

C_F <- COBI %>% 
  mutate(AbundanciaA = paste("Abundancia de", Especies, "en Punta Herrero, Sian Kaan")) %>% 
  mutate(AbundanciaB = paste("Abundancia de", Especies, "en Maria Elena, Sian Kaan")) %>% 
  mutate(AbundanciaC = paste("Abundancia de", Especies, "en Puerto Morelos")) %>% 
  mutate(AbundanciaD = paste("Abundancia de", Especies, "en Banco Chinchorro")) %>% 
  mutate(Key = paste(Comun,"Abundancia; Ausencia; Presencia; Quinta Roo; Yucatan; Monitoreo, Peces, Invertebrado")) %>% 
  mutate(TallaI = paste("Talla de", Especies, "en Punta Herrero, Sian Kaan")) %>% 
  mutate(TallaII = paste("Talla de", Especies, "en Maria Elena, Sian Kaan")) %>% 
  mutate(TallaIII = paste("Talla de", Especies, "en Puerto Morelos")) %>% 
  mutate(TallaIIII = paste("Talla de", Especies, "en Banco Chinchorro")) %>% 
  mutate(Key = paste(Comun,"Tallas; Quinta Roo; Yucatan; Monitoreo, Peces, Invertebrado"))
  

write.csv(C_F, "Cobi_Out.csv")

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
  
#### CARTA NACIONAL PESQUERA ####
  
  
Species <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/CartaNac.Pesq/Species_Names.csv",
                    col_names = TRUE)
View(Species)
  
  # Titulos 
Z_Captura <- "Zona de Captura de"
Unidad <- "Unidad de Pesca para"
Captura <- "Captura total de"
Proporcion <- "Proporcion de especies"
Poblaicon <- "Poblacion Estimada de"
Esfuerzo <- "Numero de Embercaciones/personas que pescan"
Medidas <- "Medida de Manejo para"
Punto_Ref <- "Puntos de Referencia para"
Estatus <- "Estatus de"

mutate(T1 = paste(Z_Catpura,Especie, sep=" ")) %>% 
  mutate(T2 = paste(Unidad,Especie, sep=" "))

  
  
  
Nombres <- Species %>% 
  mutate(Scientific_Name = paste(Sci_A,
                       Sci_B)) %>% 
  mutate(Commun_Name = paste(Com_A,
                             Com_B))



write.csv(Nombres, "Final_Names.csv")


  
  
  
  
  
  
  
Especies_CNP <- read_csv("~/Documents/Dropbox/Metadata_Mexico/Datasets/CartaNac.Pesq/Especies_CNP.csv")

#### Arreglar nombres ####

Nombres <- Especies_CNP %>% 
  filter(Litoral =="Pacifico") %>% 
  group_by(Scientifico,
           Animal
           ) %>% 
  summarise(n=n())

Duplicados <- Nombres[duplicated(Nombres$Scientifico),]
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  