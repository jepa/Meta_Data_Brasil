#________________________________________________________________________________________________________________________________________
##### MUY IMPORTANTE ####

#Para modificar la información de la página informativa TIENES que estar en la "branch" Internet, de github. si no, vas a modificar la versión pensada para cuándo los datos estén listos.

# This version of the app is modified to be publish online. Some things are commented like;

# Data to download
# Input data map
# Resutls map

#________________________________________________________________________________________________________________________________________


library(shiny)
library(leaflet)
library(DT)
library(markdown)

shinyUI(
  ### Navigation bar
  navbarPage(
    #Web page title
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
                   "La investigación y manejo de los recursos marinos es cada vez más dependiente de diversos indicadores biológicos, ecológicos, económicos y sociales. En México, dichos indicadores pueden existir pero no siempre están disponibles al público o su existencia no es conocida. El estar al tanto de qué datos existen para diversos temas o regiones es un enorme paso para incrementar la colaboración e investigación novedosa en México. Una base de metadatos compuesta por información relacionada a la ecología, sociología y economía, enfocada a los sistemas marinos en México, facilitará el uso eficiente de la información existente al mismo tiempo que estimulará la colaboración entre distintos sectores interesados en el desarrollo marino del país" 
                 ),
                 p(h3(
                   "Objetivos del proyecto"
                 )),
                 p("El objetivo principal de este proyecto es crear una base de metadatos que contenga información sobre datos ecológicos, económicos, oceanográficos y sociales, referentes al ambiente marino. Así mismo, se pretende identificar tendencias en la disponibilidad de datos marinos en México e identificar oportunidades de mejora de información marina en México. Por último, se pretende poyar el acceso a la información mediante un portal de consulta de datos."
                 ),
               p(strong("Cambio Climático"),
                 "La disponibilidad de datos es clave para la investigación en materia de cambio climático. Es por esto que este proyecto pretende contribuir de manera substancial a la investigación que se realiza en México sobre los impactos del cambio climático en los recursos marinos y apoyar políticas públicas diseñadas para mejor manejar los recursos marinos del país."
               )
               ),
                 column(
                   12,
                   align = "center",
                   # Flowchart Image
                   img(
                     src = "flow_chart.png",
                     height = 150,
                     width = 700
                   )
                   ),
               # Core Group Infomation
               column(
                 10,
                 align = "justified",
                 offset = 1,
                   p(h3("Individuos e Instituciones Participantes")),
                 p(strong(a(href="http://oceans.ubc.ca/andres-cisneros-montemayor/","Andrés Cisneros-Montemayor;")),
                   "Institute for the Oceans and Fisheries, University of British Columbia"
                 ),
                 p(strong(a(href="http://www.informatica.sip.ipn.mx/semanainnovacion2013/docs/dr_FranciscoArreguinSanchez.pdf","Francisco Arreguín-Sánchez;")),
                   "Centro Interdisciplinario de Ciencias Marinas, Instituto Politécnico Nacional"
                 ),
                 p(strong(a(href="https://jepa.shinyapps.io/jpalacios/","Juliano Palacios-Abrantes")),
                 "Institute for the Oceans and Fisheries, University of British Columbia"
                 ),
                 p(strong(a(href="http://mexico.edf.org/personas/laura-f-rodriguez","Laura Rodriguez;")),
                 "Environmental Defense Fund, México"
                 ),
                 p(strong(a(href="https://www.researchgate.net/profile/Miguel_Cisneros-Mata","Miguel Ángel Cisneros-Mata;")),
                 "Centro Regional de Investigación Pesqueras, Guaymas, Instituto Nacional de Pesca y Acuacultura"
                 ),
                 p(strong(a(href="http://oceans.ubc.ca/william-cheung/","William Cheung;")),
                 "Institute for the Oceans and Fisheries, University of British Columbia"
               )
               ),
               br(),
               column(
                 10,
                 align = "center",
                 offset = 1,
                 a(href="http://www.ubc.ca/",
                   img(
                     src = "ubc_logo.gif",
                     height = 80,
                     width = 70
                   )),
                 a(href="http://www.ipn.mx",
                   img(
                     src = "ipn_logo.JPG",
                     height = 80,
                     width = 70
                   )),
                 a(href="http://www.inapesca.gob.mx",
                   img(
                     src = "inapesca_logo.JPG",
                     height = 100,
                     width = 300
                   )),
                 a(href="http://mexico.edf.org/",
                   img(
                     src = "edf_logo.JPG",
                     height = 100,
                     width = 300
                   ))
               )
             ),
             br(),
             #Partners Information
             column(
               10,
               align = "justified",
               offset = 1,
               p(h3("Partners")
               )
             ),
             column(
               10,
               align = "center",
               offset = 1,
               a(href="http://datamares.ucsd.edu/en/about",
                 img(
                   src = "dataMares_Logo.png",
                   height = 60,
                   width = 200
                 )),
               a(href="http://monitoreonoroeste.mx/index.php",
                 img(
                   src = "Monitoreo_Logo.png",
                   height = 60,
                   width = 200
                 ))#,
               # a(href="www.inapesca.gob.mx",
               #   img(
               #     src = "inapesca_logo.JPG",
               #     height = 100,
               #     width = 300
               #   )),
               # a(href="www.edf.com.mx",
               #   img(
               #     src = "edf_logo.JPG",
               #     height = 100,
               #     width = 300
               #   ))
             ),
             hr(),
             br()
    ), # Close first page
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
               "A continuación se muestran datos preliminares de la investigación en marina en México, dichos resultados están directamente relacionados con la información que se va recaudando por lo que cambian a menudo. Así mismo, estos resultados son parciales y no representan el total de la investigación marina en México."
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
               p(h3("Resultados preliminares (Cuantitativos y Cualitativos)"))
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
                     ),
                   downloadButton('downloadTemplate',
                                  "Descargar Formato")
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
                   ),
                   a(img(
                     src = "Twitter_Logo.png",
                     height = 40,
                     width = 40
                   ), href = "https://twitter.com/julianop_a"
                   ),
                   "Correo Electrónico"
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
    column(4,
           align ="justified",
           p(strong("Dirección:"), "Changing Oceans Research Unit, The Institute for Oceans and Fisheries, University of British Columbia. 2202 Main Mall, Vancouver, Canadá, BC V6T 1Z4")
           ),
    column(2,
           align ="center",
           p(strong("Research Gate:")),
             p(a(img(
             src = "RG_Logo.png",
             height = 40,
             width = 40
           ), href = "https://www.researchgate.net/profile/Juliano_Palacios_Abrantes2")
  )
    )
)
)
