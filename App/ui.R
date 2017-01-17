#### NOTA ####

# This version of the app is modified to be publish online. Some things are commented like;

# Data to download
#Input data map

library(shiny)
library(leaflet)
library(DT)
library(markdown)

shinyUI(
  navbarPage(
    "Metadata de Investigación Marina en México",
    #### Inicio ####
    tabPanel("Inicio",
             fluidRow(
               column(
                 12,
                 align = "center",
                 h1("Hacia la Creación de una Base de Metadatos de Investigación Marina en México")
               ),
               column(
                 10,
                 align = "justified",
                 offset = 1,
                 p(h3("¡Bienvenidos!")),
                 p(
                   "La investigación y manejo de los recursos marinos es cada vez más dependiente de diversos indicadores biológicos, ecológicos, económicos y sociales. En México, dichos indicadores pueden existir pero no siempre están disponibles al público o su existencia no es conocida. El estar al tanto de qué datos existen para diversos temas o regiones es un enorme paso para incrementar la colaboración e investigaciónnovedosa en México. Una base de metadatos compuesta por información relacionada a la ecología, sociología y economía, enfocada a los sistemas marinos en México, facilitará el uso eficiente de la información existente al mismo tiempo que estimulará la colaboración entre distintos sectores interesados en el desarrollo marino del país" 
                 ),
                 p(h3(
                   "Objetivos del proyecto"
                 )),
                 p("El objetivo principal de este proyecto es crear una base de metadatos que contenga información sobre datos ecológicos, económicos, oceanográficos y sociales, referentes al ambiente marino. Asímismo, se pretende identificar tendencias en la disponibilidad de datos marinos en México e identificar oportunidades de mejora de información marina en méxico. Por último, se pretende poyar el acceso a la información mediante un portal de consulta de datos."
                 ),
               p(strong("Cambio Climático"),
                 "La disponibilidad de datos es clave para la investigación en materia de cambio climático. Es por esto que este proyecto pretende contribuir de manera substancial a la investigación que se realiza en México sobre los impactos del cambio climático en los recursos marinos y apoyar políticas públicas diseñadas para mejor manejar los recursos marinos del país."
               )
               ),
                 column(
                   12,
                   align = "center",
                   img(
                     src = "flow_chart.png",
                     height = 150,
                     width = 700
                   )
                   ),
               column(
                 10,
                 align = "justified",
                 offset = 1,
                   p(h3("Individuos e Instituciones Participantes")),
                 p(strong(a(href="www.andres.com","Andrés Cisneros-Montemayor;")),
                   "Institute for the Oceans and Fisheries, University of British Columbia"
                 ),
                 p(strong(a(href="www.paco.com","Francisco Arreguín-Sánchez;")),
                   "Centro Interdisciplinario de Ciencias Marinas, Instituto Politécnico Nacional"
                 ),
                 p(strong(a(href="www.mipagina.com","Juliano Palacios-Abrantes")),
                 "Institute for the Oceans and Fisheries, University of British Columbia"
                 ),
                 p(strong(a(href="www.laura.com","Laura Rodriguez;")),
                 "Environmental Defense Fund, México"
                 ),
                 p(strong(a(href="www.mac.com","Miguel Ángel Cisneros-Mata;")),
                 "Centro Regional de Investigación Pesqueras, Guaymas, Instituto Nacional de Pesca y Acuacultura"
                 ),
                 p(strong(a(href="www.william.com","William Cheung;")),
                 "Institute for the Oceans and Fisheries, University of British Columbia"
               )
               ),
               br(),
               column(
                 10,
                 align = "center",
                 offset = 1,
                 a(href="www.ubc.ca",
                   img(
                     src = "ubc_logo.gif",
                     height = 80,
                     width = 70
                   )),
                 a(href="www.ipn.mx",
                   img(
                     src = "ipn_logo.JPG",
                     height = 80,
                     width = 70
                   )),
                 a(href="www.inapesca.gob.mx",
                   img(
                     src = "inapesca_logo.JPG",
                     height = 100,
                     width = 300
                   )),
                 a(href="www.edf.com.mx",
                   img(
                     src = "edf_logo.JPG",
                     height = 100,
                     width = 300
                   ))
               )
             )
    ),
    #.########################## ##### 
    #### INPUT DATA ####
    
    #### IMPORTANT INFORMATION FOR WHEN EDDITING THIS PART ####
    #here is a code to add rows and columns to datatables, could be useful.
    #https://yihui.shinyapps.io/DT-proxy/
    #That's it, be happy, like a hippo.
    # tabPanel(
    #   "Agregar Datos",
    #   fluidPage(
    #     column(12,
    #            align="center",
    #            titlePanel("Compártenos información sobre tus datos")),
    #     fluidRow(
    #       column(12,align="justified",
    #              h3("Instructions"),
    #              p("In here we should have a very brieff text informing the instructions on how to fill the blanks"
    #              )
    #       ),
    #       #### first Column####
    #       ######Contact Info ####
    #       column(
    #         width = 4,
    #         p(h3("Contact Information")),
    #         #Mean and sd Harvestable Biomass#
    #         textInput("User_Name",
    #                   "Name",
    #                   "",
    #                   width = '100%'),
    #         textInput("User_Email",
    #                   "Email",
    #                   "",
    #                   width = '100%'),
    #         ###### Data Rpository ####
    #         p(h3("Repository Information")),
    #         tabsetPanel(
    #           id = "Data_Input",
    #           ###### Reference ####
    #           tabPanel(
    #             "Reference",
    #             textInput("Reference_",
    #                       "Please provide a path", 
    #                       "")
    #           ),
    #           ###### Data Collection ####
    #           tabPanel(
    #             "Compilation",
    #             textInput("Compilation_Title",
    #                       "Name of Data Compilation", 
    #                       "")
    #           ),
    #           ###### Help #
    #           tabPanel(
    #             "Help",
    #             strong("Reference"),"The digital path of where can the data be found. If no digital repository exists, please provide the name of the publication where data can be found",
    #             p(strong("Compilation"),"The name of the collection where data exists."),
    #             br(),
    #             p(em("For example:"),"The dataset could be",em("Shark Catch for Mexico Between 2000-2014"), "and the Collection where the data was published could be the:",em("FAO Yearbook of Fishery and Aquaculture Statistics, 2014"))
    #           )
    #         ),
    #         fileInput('Data_Upload',
    #                   h4('Upload Data'),
    #                   accept=c('text/csv',
    #                            'text/comma-separated-values,text/plain','.csv'))
    #       ),
    #       #### Second column ####
    #       column(
    #         width = 4,
    #         p(h3("General Data Information")),
    #         tabsetPanel(
    #           id = "General_Info",
    #           #######Dataset_Title ####
    #           tabPanel(
    #             "Data Title",
    #             textInput("Dataset_Title",
    #                       "Title of Original Dataset", 
    #                       "")
    #           ),
    #           #######Shot_Title ####
    #           tabPanel(
    #             "Short Title",
    #             textInput("Short_Title",
    #                       "Capture Title", 
    #                       "")
    #           ),
    #           #######Keywords ####
    #           tabPanel(
    #             "Keywords",
    #             textInput("Keyword_1",
    #                       "Keyword 1", 
    #                       "",
    #                       width = '50%'),
    #             textInput("Keyword_2",
    #                       "Keyword 2", 
    #                       "",
    #                       width = '50%'),
    #             textInput("Keyword_3",
    #                       "Keyword 3", 
    #                       "",
    #                       width = '50%'),
    #             textInput("Keyword_4",
    #                       "Keyword 4",
    #                       "",
    #                       width = '50%')
    #           ),
    #           #######Author ####
    #           tabPanel(
    #             "Author",
    #             textInput("Author",
    #                       "Author of the Dataset", 
    #                       ""),
    #             textInput("Institution",
    #                       "Institution", 
    #                       "")
    #           ),
    #           #######Geneal_Info_Help ####
    #           tabPanel(
    #             "Help",
    #             p(
    #               strong(
    #                 "Data Title"
    #               ),
    #               "Original name of dataset.", em("Note"), ": the data specifically, not the source or repository where it was published"
    #             ),
    #             p(
    #               strong(
    #                 "Short Title"
    #               ),
    #               "Please prove a short title that better describes the dataset. Please keep it consize. There is a limit of 8 words"
    #             ),
    #             p(h5("Example")),
    #             p(
    #               "Shark Survey for Mexican Pacific Coast between 1980-2000"
    #             ),
    #             p(
    #               strong(
    #                 "Keywords"
    #               ),
    #               "Limited to 8 words that describe the subject or dataset and are useful for searches"
    #             ),
    #             p(
    #               strong(
    #                 "Author"
    #               ),
    #               "Author (Principal) of the dataset for referencing the data. Can be multiple authors and institutions."
    #             )
    #           )
    #         ),
    #         ######Dataset_Available####
    #         tabsetPanel(
    #           id = "inTabset",
    #           tabPanel(
    #             "Is the Data Set Available?",
    #             selectInput("Dataset_Available", 
    #                         label= "Choose Only One",
    #                         choices = list("Public Access" = 1, 
    #                                        "Restricted Access (Public)" = 2,
    #                                        "Private" = 3),
    #                         selected = 0,
    #                         width = '50%')
    #           ),
    #           tabPanel(
    #             "Help",
    #             p(
    #               "Is the data available for use?"
    #             ),
    #             p(h5("Example")),
    #             p(
    #               "Could be: NGO, Gov, Academia, IGO, Industry,  Private. (Multiple)"
    #             )
    #           )
    #         ),
    #         tabsetPanel(
    #           id = "Institution_type",
    #           tabPanel(
    #             "Institution Collecting Data",
    #             checkboxGroupInput("Institution_Type",
    #                                label = "Chose One or Multiple", 
    #                                choices = list("NGO" = 1,
    #                                               "Gob." = 2, 
    #                                               "Academy" = 3,
    #                                               "IGO", 
    #                                               "Industry" = 5 ), 
    #                                selected = 0)
    #           ),
    #           tabPanel(
    #             "Help",
    #             p(
    #               "The type of institution that generated the dataset (corresponds to the Institution field. Can be multiple selection)"
    #             ),
    #             p(h5("Example")),
    #             p(
    #               "Could be: NGO, Gov, Academia, IGO, Industry,  Private. (Multiple)"
    #             )
    #           )
    #         )
    #       ),
    #       #Close second column
    #       #### Third COLUMN, Spatial####
    #       column(
    #         p(h3("Spatial Information")),
    #         width = 3,
    #         tabsetPanel(
    #           id ="Spatial_Information",
    #           ###### Area ####
    #           tabPanel(
    #             "Area",
    #             selectInput("Area", 
    #                         label= "Choose Only One",
    #                         choices = list("National" = 1, 
    #                                        "Atlantic" = 2,
    #                                        "Pacific" = 3,
    #                                        "FreshWater/Terrestrial" = 4),
    #                         selected = 0,
    #                         width = '100%')
    #           ),
    #           ###### Region ####
    #           tabPanel(
    #             "Region",
    #             checkboxGroupInput("Region", 
    #                                label= "Choose all tha Apply",
    #                                choices = list("Gulf of California" = 1, 
    #                                               "Nortwest Pacific" = 2,
    #                                               "South Pacific" = 3,
    #                                               "Gulf of Tehuantepec" = 4,
    #                                               "Yucatan Peninsula / Mar Caribe" = 5,
    #                                               "West Gulf of Mexico" = 6,
    #                                               "FreshWater/Terrestrial" = 7),
    #                                selected = 0,
    #                                width = '100%'
    #             )
    #           ),
    #           ###### Location ####
    #           tabPanel(
    #             "Location",
    #             textInput("Location",
    #                       "Location",
    #                       "",
    #                       width = '100%'),
    #             actionButton(
    #               "Location_Map",
    #               "Map"
    #             )
    #           ),
    #           ###### Spatial_Help ####
    #           tabPanel(
    #             "Help",
    #             p(strong("Area:"),"A broad category of where the data was generated, cannot be lefted in blank"),
    #             p(strong("Region:"),"A more specific location category, can be left blank"),
    #             p(strong("Location:"),"A very specific localization category. Can be anything from an specific city or beach to coordinates."),
    #             p(strong("Map:"),"Brings out a map so you can geo-reference the data")
    #           )
    #         )
    #       )
    #     )
    #   ),
    #   #### Input Data Map ####
    #   mainPanel(em(
    #     column(
    #       width = 12,
    #       align ="center",
    #       leafletOutput("Location_Map"),
    #       column(width = 6,
    #              numericInput("Map_Long",
    #                           "Longitude",
    #                           value=-103,
    #                           min =-122.1836,
    #                           max = -84.6419,
    #                           step = 1,
    #                           width = "30%"
    #              )
    #       ),
    #       column(width=6,
    #              numericInput("Map_Lat",
    #                           "Latitude",
    #                           value=24,
    #                           min =-12.1031,
    #                           max = -32.627,
    #                           step = 1,
    #                           width = "30%"
    #              )
    #       )
    #     )
    #   )
    #   )
    # ),
    #.########################## ##### 
    #### METADATA ####
    tabPanel("Metadata",
             h3(
               "Metadata de Investigación Marina en México"
             ),
             p("A continuación le presentamos la base de metadatos que se ha recopilado desde Noviembre de 2016, aquí podrá consultar los datos que se han recopilado así como descargar la lista de referencias que se han consultado hasta la fecha."),
             p(
               "Si usted es autor o responsable de alguna información presente en esta tabla y encuentra un error, favor de comunicarse con nosotros."
             ),
             p(strong("Nota:"),"La información aquí presente no es terminal ya que el proyecto aún no termina y la información aún está siendo colectada"),
             column(
               width=12,
               align = "center",
               tabsetPanel(
                 id ="Data_Explorer",
                 tabPanel(
                   p(h4("Base de Metadatos")),
                   dataTableOutput('Metadata')
                   ),
                 tabPanel(
                   p(h4("Referencias")),
                   column(
                     width = 12,
                     align = "center",
                     radioButtons('format',
                                  'Seleccione el Formato',
                                  c('PDF',
                                    'HTML',
                                    'Word'),
                                  inline = TRUE),
                     downloadButton('downloadReport',
                                    "Descargar Lista de Referencias")
                   ),
                 column(
                     width = 12,
                     align = "left",
                   htmlOutput("Reference")
                 )
                 )
                 )
               )
             ),
    #### PRELIMINARY RESULTS ####
    tabPanel("Resultados Preeliminares",
             #Wellcome / Instructions####
             p(h3(
               "Resultados Preeliminares de la Información Adquirida"
               )),
             p(
               "A continuación se muestran datos preeliminares de la investigación en marina en México, dichos resultados están directamente relacionados con la información que se va recaudando por lo que cambian a menudo. Así mismo, estos resultados son parciales y no representan el total de la investigación marina en México."
               ),
             column(
               width=4,
               align = "center",
               p(h3("Registros Capturados")),
               p(h4(textOutput("Number_Entries")))
             ),
             column(
               width=4,
               align = "center",
               p(h3("Número de Datos")),
               p(h4(textOutput("Number_Data_Points")))
             ),
             column(
               width=4,
               align = "center",
               p(h3("Fuentes Consultadas")),
               p(h4(textOutput("Sources")))
             ),
             br(),
             br(),
             column(
               width = 12,
               align = "center",
               p(h3("Resultados preeliminares (Cuantitativos y Cualitativos)"))
             ),
             #.########################## ##### 
             #### PRELIMINARY RESULTS ####
             #### Quantitative Results####
             column(
               width = 6,
               align= "center",
               p(h3("Resultados Cuantitativos")),
               p(h4("Número de datos por Unidad de Espacio")),
               selectInput("Plot_Option", 
                           label= "Seleccione la Opción Deseada:",
                           choices = list("Area" = 1, 
                                          "Región" = 2,
                                          "Localidad" =3
                           )
               ),
               plotOutput("Number_spp"),
               sliderInput("Num_Data_Range",
                           "Seleccion el Número a Mostrar (Localidad)",
                           value=10,
                           min = 1,
                           max = 50),
               #### SE_Component ####
               p(h4("Componente Social Económico")),
               plotOutput("SE_Component")
             ),
             #### Qualitative Results####
             ####Keywords Word Cloud####
             column(
               width = 6,
               align= "center",
               p(h3("Resultados Cualitativos")),
               p(h4("Palabras Claves Más Repetidas")),
               textInput("Keyword_Remove1",
                         "Quite Cualquier Palabra",
                         "",
                         width = '50%'),
               textInput("Keyword_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Keywords_Plot"),
               p(em("Nota: Es posible que no todas las palabras estén representadas")),
               #### Subject_Name Word Cloud ####
               p(h4("Categorías Más Repetidas")),
               textInput("Subject_Remove",
                         "Quite Cualquier Palabra",
                         "",
                         width = '50%'),
               textInput("Subject_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Subject_name_Plot"),
               p(em("Nota: Es posible que no todas las palabras estén representadas"))
             ),
             #### Experimental Analysis ####
             column(width =12,
                    align = "center",
                    selectInput("SE_E_Plot_Option", 
                                label= "Seleccione la Categoría Deseada",
                                choices = list("Area" = 1, 
                                               "Región" = 2,
                                               "Localidad" =3
                                ),
                                width = "20%"
                    ),
             plotOutput("SE_Component_Area"),
             selectInput("Research_Field_Plot_Option", 
                         label= "Seleccione la Opción Deseada:",
                         choices = list("Area" = 1, 
                                        "Region" = 2,
                                        "Location" =3
                         ),
                         width = "20%"
             ),
             plotOutput("Research_Field_Plot")
             )
    ),
    #.########################## ##### 
    #### PARTICIPATION ####
    tabPanel(strong("Como Participar"),
             h3("Colabora en el Desarollo de La Investigación Marina en México"),
            p(" Estamos buscando cualquier fuente de información que contenga datos sobre
             temas marinos en México. ¡No importa la fuente, pueden ser tus datos de
             la tesis, de captura o algún reporte!"),
            p("El proyecto se encuentra en una etapa de colaboración que consta en poblar la base de datos por lo que invitamos a cualquier persona interesada a colaborar con el proyecto a compartir información sobre sus datos."),
            p("Es importante mencionar que",strong("no estamos copilando datos, si no que información sobre los mismos.")
              ),
            "Cuanta más gente esté involucrada, mejor podremos reflejar el estado de la investigación marina en México",
            br(),
            column(12,
                   align="center",
              p(h3(
                "Tres Maneras de Colaborar"
                   ))
              ),
            column(4,
                   align="justified",
                   h3(img(
                     src = "Colaborar.png",
                     height = 60,
                     width = 60,
                     "1. Comparte"
                   )),
                   p(
                     "Para que el proyecto mejor refleje la investigación marina en México es necesario tener la mayor cantidad de información posible. Recuerda que no estamos colectando datos crudos, estamos colectando información sobre los datos"
                     )
                   ),
            column(4,
                   align="justified",
                   h3(img(
                     src = "Busca.png",
                     height =60,
                     width = 60,
                     "2. Informa"
                   )),
                   p(
                     "Estamos buscando cualquier fuente de información que contenga datos sobre
                     temas marinos en México. No importa la fuente, pueden ser datos de tesis, publicaciones, monitoreos, literatura gris o algún reporte oficial"
                   )
            ),
            column(4,
                   align="justified",
                   h3(img(
                     src = "Comparte.png",
                     height = 60,
                     width = 60,
                     "3. Habla"
                   )),
            p(
              "Creemos que en México existe mucha información relevante al ambiente marino, sin embargo un pequeó grupo de personas no son capaces de capturar toda la información, cuanta más gente esté involucrada, más información podemos colectar y mejor podremos reflejar el estado actual de la investigación marina en México así como descubrir campos con falta de información."
            )
            )
            ),
    #.########################## ##### 
    #### CONTACT ####
    hr(),
    br(),
    column(12,
           align = "center",
           h5("Información de Contacto")
           ),
    column(3,
           align = "center",
           p(strong("Autor Corresponsal:"),"Juliano Palacios j.palacios@oceans.ubc.ca")
    ),
    column(3,
           align = "center",
           p(strong("Teléfono:"), "+1 (778) 835 4298"),
           strong("Skype:"), "jepa_88"
           ),
    column(5,
           align ="justified",
           p(strong("Dirección:"), "Changing Oceans Research Unit, The Institute for Oceans and Fisheries, University of British Columbia. 2202 Main Mall, Vancouver, Canadá, BC V6T 1Z4")
           ),
    column(1,
           align ="centered",
           a(img(
             src = "RG_Logo.png",
             height = 40,
             width = 40
           ), href = "https://www.researchgate.net/profile/Juliano_Palacios_Abrantes2")
  )
)
)
