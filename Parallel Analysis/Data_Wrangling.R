####
# Data wrangling script 
#The data used is added in the MMd as "Reconstructed" data, I added "Industria, artisanal, etc to keywords
####


#### NOTE Allways run this first ####
library(xlsx)
library(dplyr)
library(tidyr)
library(data.table)
library(leaflet)

setwd("~/Documents/Github/Meta_Data_Mexico/Parallel Analysis")

#________________________________________________________#

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
# GOb.mx info

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
