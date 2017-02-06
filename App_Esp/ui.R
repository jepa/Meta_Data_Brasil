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
library(dygraphs)

shinyUI(
  ### Navigation bar
  navbarPage(id = "MMM_Nav_Bar",
             #Web page title
             "Meta-database of Marine Research in Mexico",
             #### HOME ####
             tabPanel("Home",
                      fluidRow(
                        column(
                          12,
                          align = "center",
                          h1("Hacia la creación de una base de metadatos de investigación marina en México"),
                          img(
                            src = "Portada.jpg",
                            height = 500,
                            width = 800
                          )
                        ),
                        column(
                          8,
                          align = "justified",
                          offset = 2,
                          p(h3("Bienvenidos")),
                          p(
                            "El proyecto de meta-base de datos en investigación marina tiene como objetivo crear una meta-base de datos de todos los datos de investigación marina desarrollados en México. Una base de metadas compende información sobre los datos colectados, en lugar de una base de datos de los datos reales. Desde octubre de 2016 hemos recolectado más de 30,000 datos, de 12 repositorios on-line y colaborado con más de 17 profesionales de diferentes disciplinas. Sin embargo, aún estamos buscando cualquier información disponible sobre investigación marina en México, independientemente de la fuente. Los datos pueden provenir de distintas fuentes como: datos de la tesis, literatura gris, o capturados por pescadores o voluntarios"
                          )
                        ),
                        column(
                          8,
                          align = "center",
                          offset = 2,
                          p(strong(h4(
                            "¡Cuantas más personas participen, mejor podremos reflejar el estado de la investigación marina en México!"
                          ))),
                          p(actionButton("Collaborate_But", "¿Cómo puedo colaborar?"))
                        ),
                        #Partners Information####
                        column(
                          8,
                          align = "justified",
                          offset = 2,
                          p(h3("Red de Metadatos")
                          )
                        ),
                        column(
                          8,
                          align = "center",
                          offset = 2,
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
                          #     height = 80,
                          #     width = 300
                          #   )),
                          # a(href="www.edf.com.mx",
                          #   img(
                          #     src = "edf_logo.JPG",
                          #     height = 80,
                          #     width = 300
                          #   ))
                        )
                      )
             ),
             #### PROJECT DESCRIPTION ####
             tabPanel("El Proyecto",
                      fluidRow(
                        column(
                          8,
                          align = "justified",
                          offset = 2,
                          p(h3("El Proyecto")),
                          p(
                   "La investigación y manejo de los recursos marinos es cada vez más dependiente de diversos indicadores biológicos, ecológicos, económicos y sociales. En México, dichos indicadores pueden existir pero no siempre están disponibles al público o su existencia no es conocida. El estar al tanto de qué datos existen para diversos temas o regiones es un enorme paso para incrementar la colaboración e investigación novedosa en México. Una base de metadatos compuesta por información relacionada a la ecología, sociología y economía, enfocada a los sistemas marinos en México, facilitará el uso eficiente de la información existente al mismo tiempo que estimulará la colaboración entre distintos sectores interesados en el desarrollo marino del país. Así mismo, tiene el potencial de facilitar el proceso de mejoras en materia de políticas publicas. Previamente se han desarrollado bases de datos similares (por ejemplo, para Canadá) y esas experiencias pueden adaptarse fácilmente a México",
                   a("(Cisneros-Montemayor et al. 2016).",
                     href="http://www.nrcresearchpress.com/doi/pdf/8.1139/cjfas-2015-0573")
                          ),
                 p(h3(
                   "Objetivos del proyecto"
                 )),
                 p("El objetivo principal de este proyecto es crear una base de metadatos que contenga información sobre datos ecológicos, económicos, oceanográficos y sociales, referentes al ambiente marino. Así mismo, se pretende identificar tendencias en la disponibilidad de datos marinos en México e identificar oportunidades de mejora de información marina en México. Por último, se pretende poyar el acceso a la información mediante un portal de consulta de metadatos."
                 ),
                 p(
                 "La disponibilidad de datos es clave no sólo para comprender mejor los ambientes marinos y costeros de México, sino que también para identificar las brechas de conocimiento apoyando así, a la prioritización de la investigación en México. Por ejemplo, la disponobilidad de información facilitará el manejo de recursos marinos y las políticas de conservación para los ecosistemas marinos y los recursos pesqueros vulnerables al cambio climático"
               )
               ),
                 column(
                   8,
                   align = "center",
                   offset = 2,
                   # Flowchart Image
                   img(
                     src = "flow_chart.png",
                     height = 150,
                     width = 700
                   )
                   ),
               # Core Group Infomation
               column(
                 8,
                 align = "justified",
                 offset = 2,
                 p(h3(
                   "Investigadores Principales"
                 )
                 ),
                 p(strong(a(href="http://oceans.ubc.ca/andres-cisneros-montemayor/","Andrés Cisneros-Montemayor;")),
                   "Institute for the Oceans and Fisheries, University of British Columbia"
                 ),
                 p(strong(a(href="http://www.informatica.sip.ipn.mx/semanainnovacion2013/docs/dr_FranciscoArreguinSanchez.pdf","Francisco Arreguín-Sánchez;")),
                   "Centro Interdisciplinario de Ciencias Marinas, Instituto Politécnico Nacional"
                 ),
                 p(strong(a(href="https://jepa.shinyapps.io/jpalacios/","Juliano Palacios-Abrantes;")),
                   "Institute for the Oceans and Fisheries, University of British Columbia"
                 ),
                 p(strong(a(href="http://mexico.edf.org/personas/laura-f-rodriguez","Laura Rodriguez;")),
                   "Environmental Defense Fund, México"
                 ),
                 p(strong(a(href="https://www.researchgate.net/profile/Miguel_Cisneros-Mata","Miguel Ángel Cisneros-Mata;")),
                   "Centro Regional de Investigaciones Pesqueras, Guaymas. Instituto Nacional de Pesca."
                 ),
                 p(strong(a(href="http://oceans.ubc.ca/william-cheung/","William Cheung;")),
                   "Institute for the Oceans and Fisheries, University of British Columbia"
                 )
               ),
               br(),
               column(
                 8,
                 align = "center",
                 offset = 2,
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
               8,
               align = "justified",
               offset = 2,
               p(h3("Partners")
               )
             ),
             column(
               8,
               align = "center",
               offset = 2,
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
               width=8,
               offset = 2,
               align = "justified",
               tabsetPanel(
                 id ="Data_Explorer",
                 tabPanel(
                   p(h4("Base de Metadata")),
                   dataTableOutput('Metadata')
                 ),
                 tabPanel(
                   p(h4("Metadata Resumida")),
                   dataTableOutput('Metadata_Summary')
                 ),
                 tabPanel(
                   p(h4("Glosario Metadata")),
                   dataTableOutput('Metadata_Key')
                 )
               )
             )
    ), #Close second page
    #### PRELIMINARY RESULTS ####
   tabPanel("Resultados Preeliminares",
            #Wellcome / Instructions####
            column(
              width=8,
              offset = 2,
              align = "justified",
              p(h3(
                "Resultados Preeliminares"
              )),
             p(
               "A continuación se muestran datos preliminares de la investigación en marina en México, dichos resultados están directamente relacionados con la información que se va recaudando por lo que cambian a menudo. Así mismo, estos resultados son parciales y no representan el total de la investigación marina en México."
               )
             ),
             column(
               width=3,
               align = "center",
               offset = 1,
               p(h3("Registros Capturados")),
               p(h4(textOutput("Number_Entries")))
             ),
             column(
               width=3,
               align = "center",
               p(h3("Número de Datos")),
               p(h4(textOutput("Number_Data_Points")))
             ),
             column(
               width=3,
               align = "center",
               p(h3("Repositorios Consultados")),
               p(h4(textOutput("Sources")))
             ),
             br(),
             br(),
             column(
               width = 8,
               align = "center",
               offset = 2,
               #### Time Series ####
               p(h3(
                 "Serie de Tiempo de Incorporación de Información"
               )),
               dygraphOutput("TFgraph"), #Timeframe graph
               p(h3("Resultados Preeliminares "))
             ),
             #### Quantitative Results####
             column(
               width = 4,
               offset = 2,
               align= "center",
               p(h2("Resultados Cuantitativos")),
               br(),
               p(h3("Datos por Area")),
               selectInput("Plot_Option", 
                           label= "Seleccione una opción:",
                           choices = list("Area" = 1, 
                                          "Región" = 2,
                                          "Localidad" =3
                           )
               ),
               plotOutput("Number_spp"),
               sliderInput("Num_Data_Range",
                           "Selecciona el Número de Localidades",
                           value=10,
                           min = 1,
                           max = 50)
             ),
             #### Qualitative Results####
             ####Keywords Word Cloud####
             column(
               width = 4,
               align= "center",
               p(h2("Resultados Cualitativos")),
               br(),
               p(h3("Palabras Clave Frecuentes")),
               textInput("Keyword_Remove1",
                         "Excluya Cualquier Palabra",
                         "",
                         width = '50%'),
               textInput("Keyword_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Keywords_Plot"),
               p(em("Nota: Es posible que no todas las palabras se encuentren en la gráfica"))
               
             ),
             column(
               width = 8,
               align= "center",
               offset = 2,
               p(h3(
                 "Reconstruction of Historic Data from Metadata"
               )),
               dygraphOutput("TSgraph")
            )
            ),
    #.########################## ##### 
    #### PARTICIPATION ####
   tabPanel(id="Collaboration",
            value="Collaborate",
      strong("Como Participar"),
      column(8,
             align="justified",
             offset = 2,
             h3("Colabora en el Desarollo de La Investigación Marina en México"),
            p("Estamos buscando cualquier fuente de información que contenga datos sobre
             temas marinos en México. ¡No importa la fuente, pueden ser tus datos de
             la tesis, de captura o algún reporte!"),
            p("El proyecto se encuentra en una etapa de colaboración que consta en poblar la base de datos por lo que invitamos a cualquier persona interesada a colaborar con el proyecto a compartir información sobre sus datos."),
            p("Es importante mencionar que",strong("no estamos copilando datos, si no que información sobre los mismos.")
              ),
            "Cuanta más gente esté involucrada, mejor podremos reflejar el estado de la investigación marina en México"
            ),
            br(),
            column(8,
                   align="justified",
                   offset = 2,
                   p(h3(
                "Tres Maneras de Colaborar"
                   ))
              ),
            column(3,
                   align="justified",
                   offset = 2,
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
            column(2,
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
            column(3,
                   align="justified",
                   h3(img(
                     src = "Comparte.png",
                     height = 60,
                     width = 60,
                     "3. Difunde"
                   )),
            p(
              "Creemos que en México existe mucha información relevante al ambiente marino, sin embargo un pequeó grupo de personas no son capaces de capturar toda la información, cuanta más gente esté involucrada, más información podemos colectar y mejor podremos reflejar el estado actual de la investigación marina en México así como descubrir campos con falta de información."
            )
            ),
   #### What do I win? ####
   column(8,
          align = "justified",
          offset = 2,
          p(h3(
            "Beneficios de Colaboración"
          )),
          p(
            "El intercambio de información sobre datos tiene una serie de beneficios tanto para los individuos como para la sociedad en si. Al tener la información sobre sus datos en los metadatos aumentará su alcance ya que otros investigadores podrán entrar en contacto con usted en busca de colaboración. Al mismo tiempo, esto fomentará la colaboración entre investigadores nacionales e internacionales que podría dar lugar a un importante avance para el país", 
            a("(Michener 2006).",
              href="http://www.sciencedirect.com/science/article/pii/S157495410500004X"
            )
          ),
          p(
            "Como partde de una comunidad comprometida con la investigación del medio ambiente marino mexicano, compartir nuestros datos trae importantes beneficios. La creación de sistemas para administrar y compartir datos asegura la preservación, administración y acceso a la información",
            a("(Fridell et al., 2014).",
              href="http://datascience.codata.org/articles/abstract/10.2481/dsj.IFPDA-01/"), "También nos permitirá entender qué información marina existe en México, e identificar campos de investigación prioritarios para el desarollo marino y de recursos naturales del país."
          )
   ),
   #### People Collaborating ####
   column(8,
          align = "justified",
          offset = 2,
          p(h3(
            "Colaboradores"
          )),
          p(
            "Si bien la gran mayoría de la información recopilada hasta ahora es de acceso público en línea (internet), hemos comenzado a recibir información de datos de fuentes no publicadas o simplemente no disponibles en internet" 
          ),
          column(6,
                 align = "center",
                 br(),
                 p(h4(strong(
                   "Instituciones"
                 ))),
                 dataTableOutput("Institutions")
          ),
          column(6,
                 align = "center",
                 p(h4(strong(
                   "Personas"
                 ))),
                 dataTableOutput("People")
          )
   
  )
  ),
  #### DATA POINTS ####
  column(8,
         align = "center",
         absolutePanel(
           top = 50,
           bottom = 20,
           right = 20,
           #width = "25%",
           draggable = TRUE,
           fixed = TRUE,
           wellPanel(
             strong("Data Points collected:"),
             textOutput("Datapoints_Intro")
           )
         )
  ),
    #.########################## ##### 
    #### CONTACT ####
    hr(),
    br(),
    column(8,
           align = "center",
           offset = 2,
           h5("Información de Contacto")
           ),
    column(2,
           align = "center",
           offset = 2,
           p(strong("Autor Corresponsal:"),"Juliano Palacios j.palacios@oceans.ubc.ca")
    ),
    column(2,
           align = "center",
           offset = 2,
           p(strong("Teléfono:"), "+1 (778) 835 4298"),
           strong("Skype:"), "jepa_88"
           ),
    column(3,
           align ="justified",
           p(strong("Dirección:"), "Changing Oceans Research Unit, The Institute for Oceans and Fisheries, University of British Columbia. 2202 Main Mall, Vancouver, Canadá, BC V6T 1Z4")
           ),
    column(1,
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
