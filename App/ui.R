library(shiny)

shinyUI(navbarPage(
  theme = "fondo.css",
  "Metadata of Marine Research in Mexico",
  #### HOME ####
  tabPanel("Home",
           fluidRow(
             column(
               12,
               align = "center",
               h1("Project Outline") #,
               # img(
               #   src = "Portada.jpg",
               #   height = 400,
               #   width = 800
               # )
             ),
             column(
               10,
               align = "justified",
               offset = 1,
               p(h3("Wellcome!")),
               p(
                 "Research and management of marine resources increasingly depends on various biological, ecological, social, and economic data. The availability of data is often perceived as a gap in advancing research and policy discussion. However, in many cases, this is largely a result of the lack of knowledge about the availability of these data. In Mexico, numerous information covering the seas and coasts can be found in academic institutions, government, and NGOs located (physically) both inside and outside the country. While diverse barriers often compromise the exchange of information among stakeholders, having publicly accessible description on existing data is a huge step towards increasing collaboration and innovative research." 
               ),
               p(
                 "A meta-database of available ecological, social and economic data in Mexico will help facilitate efficient use of existing information and stimulate collaboration. A meta-database is a documentation of the sources of information instead of a database of the actual data. For example, a meta-database record might represent the sources (e.g., the institution hosting the data, a URL where the data could be assessed), types (e.g., fish abundance, temperature), location (e.g., reef in Caribbean) and time-frame (e.g., specific years, seasons, or a time-series). Such meta-database can help reveal trends in the availability of different types of data, identify existing data gaps, and facilitate researchers and managers to use all the available information for their studies or for decision making. Hence, this database can be an important instrument in the context of increasing collaboration among different disciplines. A researcher could identify data of interest within the meta-database, contact the right person or institute directly, and collaborate on new research. This way, the meta-database fosters collaboration and eases the process of informing best policies relevant to any community or region (shown in the diagram below). Such meta-database has been developed for Canada (Cisneros-Montemayor et al. 2016) and such experiences could be adapted for Mexico."
               ),
               p(
                 "The meta-database can also be constantly updated when new datasets become available. As new projects materialize in Mexico, new information will be created and then potentially incorporated as a new entry in the metadata. This process will keep the database updated and constantly expanding. There is no requirement that corresponding datasets, which may be restricted or otherwise not be publicly available, be initially shared. This facilitates the prompt incorporation of any new information into the meta-database. Finally, besides being a useful resource for collaboration and analysis, the metadata will reflect the state of marine research in Mexico."
               ),
               column(
                 12,
                 align = "center",
               img(
                 src = "flow_chart.png",
                 height = 150,
                 width = 600
               ))
               
             )
           )),
  #### Input Data ####
  tabPanel(
    "Input Data",
    fluidPage(
      column(12,align="center",
             titlePanel("Main Pannel for Uploading Data to the Meta-Dataset")),
      fluidRow(
        sidebarLayout(
          sidebarPanel(
            width = 12,
            h3("Instructions"),
            p("In here we should have a very brieff text informing the instructions on how to fill the blanks"),
            selectInput(
              "User_Contact",
              "User Contact:",
              choices = c(
                "",
                "Punta Arena",
                "Lado de los Palos",
                "Boca de la Islita",
                "Punta de Monte",
                "Enscenada Santa Cruz",
                "Oporito"
              )
            )
          ),
          mainPanel("ja")
        ),
        column(
          width = 4,
          p(h3("Factores de Pesca")),
          #Mean and sd Harvestable Biomass#
          numericInput(
            inputId = "mean",
            label = "Biomasa Capturable (promedio)",
            value = 1000,
            min = 0,
            max = 100
          ),
          numericInput(
            inputId = "sd",
            label = "Biomasa Capturable (desviación estandar)",
            value = 100,
            min = 0,
            max = 100
          ),
          numericInput(
            inputId = "FM",
            label = "Mortalidad por Pesca",
            value = 0.2,
            min = 0,
            max = 1
          ),
          #Conversion Factor
          numericInput(
            inputId = "Con_Fac",
            label = "Factor de Conversión (Pesca<->Venta)",
            value = .25,
            min = .1,
            max = 1
          ),
          #CPUE
          numericInput(
            inputId = "CPUE",
            label = "Captura por Unidad de Esfuerzo (CPUE)",
            value = 25,
            min = 1,
            max = NA
          ),
          #Precio de venta
          numericInput(
            inputId = "Price",
            label = "Precio de venta por Kilo",
            value = 200,
            min = 1,
            max = NA
          )
        ),
        #Close first column
        #### SECOND COLUMN, costos ####
        column(
          width = 6,
          p(h3("Costos de Pesca")),
          p(h6("Seleccione uno")),
          tabsetPanel(
            id = "inTabset",
            #### C Unidad de Esfuerzo ####
            tabPanel(
              "Costos Unidad de Esfuerzo",
              numericInput(
                inputId = "Costo_F_G_1",
                label = "Costo 1",
                value = 150,
                min = 0,
                max = NA
              ),
              numericInput(
                inputId = "Costo_F_G_2",
                label = "Costo 2",
                value = 70,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Costo_F_1",
                label = "Costo 3",
                value = 0,
                min = 0,
                max = NA
              ),
              numericInput(
                inputId = "Costo_F_2",
                label = "Costo 4",
                value = 0,
                min = 0,
                max = NA
              ),
              numericInput(
                inputId = "Costo_F_3",
                label = "Costo 5",
                value = 0,
                min = 0,
                max = NA
              )
            ),
            #### C Unidad de Captura ####
            tabPanel(
              "Costos por unidad de Captura",
              p(h5("Costos por Unidad de Captura")),
              numericInput(
                inputId = "Cost_UE1",
                label = "Precio Gasolina",
                value = 14,
                min = 0,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UE2",
                label = "Distancia",
                value = 0,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UE3",
                label = "Consumo del Vehículo",
                value = 9,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UE4",
                label = "Capacidad del Vehículo (Kg)",
                value = 1000,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UC1",
                label = " Precio del Hielo",
                value = 20,
                min = 0,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UC2",
                label = "Capacidad de carga Hielo",
                value = 25,
                min = NA,
                max = NA
              )
            ),
            #### Costos Fijos ####
            tabPanel(
              "Costos Fijos",
              numericInput(
                inputId = "Cost_UC2",
                label = "Permiso de Pesca",
                value = 750,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UC3",
                label = "Permiso de Barco",
                value = 78,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UC4",
                label = "Mantenimiento del Equipo",
                value = 2500,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UC5",
                label = "Mantenimiento del Equipo",
                value = 2500,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Cost_UC6",
                label = "Mantenimiento del Equipo",
                value = 2500,
                min = NA,
                max = NA
              ),
              numericInput(
                inputId = "Boat_U",
                label = "Uso del barco en la pesca (%)",
                value = .20,
                min = 0,
                max = 1,
                step = .1
              )
            )
          )
        )
      )
))
)
)