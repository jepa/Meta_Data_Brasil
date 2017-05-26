#### Server for Metadata APP ####

# This script is part of the Metadata of Marine Research in Mexico app.
#Started on October, 2016
#Juliano Palacios Abrantes, j.palacios@oceans.ubc.ca
#________________________________________________________________________#

#### NOTES ####
# everything you do here, you have to miror in the "App_Esp". If the spanish webpage is stil on-line.

#_______________________________ END NOTES __________________________________#

####Libraries needed ####

library(shiny)
library(leaflet)
library(DT)
library(markdown)
library(dygraphs)

 
shinyUI(

  ### Navigation bar
  navbarPage("",
             #Web page title
             "Metadatos de Investigación Marina en México",
             #### HOME ####
             tabPanel("Bienvenidos",
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
                            "El proyecto de metadatos de investigación marina en México tiene como objetivo crear una base de metadatos respecto a toda la información de investigación marina desarrollada en México. Una base de metadatos está compuesta por información sobre datos existentes, en lugar de los datos en si. Desde octubre de 2016 hemos recolectado más de 30,000 datos individuales, incluyendo 12 repositorios existentes y colaborado con múltiples invetigadores y representates de diferentes disciplinas e instituciónes. Sin embargo, seguimos buscando cualquier información disponible sobre investigación marina en México, independientemente de la fuente. Los datos pueden haberse originado de distintas maneras, por ejemplo: datos de tesis, literatura gris, o capturados por pescadores o voluntarios"
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
                          p(h3("Red de metadatos")
                          )
                          ),
                          #### DataMares ####
                          column(
                            4,
                            align = "justified",
                            offset = 2,
                            a(href="http://datamares.ucsd.edu/en/about",
                              img(
                                src = "dataMares_Logo.png",
                                height = 60,
                                width = 200
                              )),
                            p(
                              "DataMares es una iniciativa que tiene como objetivo promover el libre accesso a la ciencia proporcionando fácil acceso a información científica de alta calidad. DataMares no solo funciona como un repositorio público de datos marinos si no que además, sirve como método de comunicación entre los datos y el usuario. A traves de sus", a (href =" http://datamares.ucsd.edu/?page_id=153 "," historias ")," dataMares conecta al público con la ciencia a través de la investigación y los datos almacenados."
                            ),
                            p(h5(
                              "¿Necesitas un repositorio para tus datos?"
                            )),
                            p(
                              "Recuerda que la base de metadatos de investigación marina no es repositorio de datos. ¡Visita dataMares caso necesites almacenar datos y generar una historia a partir de ellos!"
                            )
                          ), #Close dataMares
                          # column(
                          #   2,
                          #   align = "justified",
                          #   #offset = 2,
                          #   a(href="http://gomexsi.tamucc.edu/",
                          #     img(
                          #       src = "GoMexSI.png",
                          #       height = 60,
                          #       width = 100
                          #     )),
                          #   p(
                          #     "GoMexSi is a transformational, open source tool to be used by the scientific and public communities to record, archive and analyze species interaction data. The database contains information on all instances of species interactions recorded in the Gulf of Mexico.",
                          #     p(h5(
                          #       "Do you have species interactions data from the GoM?"
                          #     )),
                          #     p(
                          #       "You can contact GoMexSi and store your datasets of species interactions on their free on-line repository. Your data will be available for access in the website!"
                          #     )
                          #   )
                          # ), #Close GoMexi
                          column(
                            4,
                            align = "justified",
                            #offset = 2,
                            a(href="http://monitoreonoroeste.mx/index.php",
                              img(
                                src = "Monitoreo_Logo.png",
                                height = 60,
                                width = 200
                              )),
                            p(
                              "Monitoreo Noroeste.mx es un sitio web público que contiene un inventario (metadatos) de monitoreo realizado en el Golfo de California y el Noroeste Pacífico. El inventario contiene una gama de objetivos que están siendo monitoreados por instituciones de investigación, agencias federales, organizaciones sociales civiles, compañías y grupos comunitarios. Los metadatos que se encuentran en Monitoreo noroeste no están incluidos en nuestros meta-datos, ¡pero no se preocupe! Ambos inventarios están conectados por lo que si su búsqueda resulta en información almacenada en Monitoreo noroeste, usted será inmediatamente redirigido"
                            )
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
                          p(h3("El proyecto")),
                          p(
                            "La investigación y manejo de los recursos marinos depende cada vez más de diversos indicadores biológicos, ecológicos, económicos y sociales. En México, dichos indicadores no siempre están disponibles al público o su existencia no se conoce ampliamente. El estar al tanto de qué datos existen para diversos temas o regiones es un enorme paso para incrementar la colaboración e investigación novedosa en México. Una base de metadatos compuesta por información relacionada a la ecología, sociología y economía, enfocada a los sistemas marinos en México, facilitará el uso eficiente de la información, estimulará la colaboración entre distintos sectores interesados en el desarrollo marino del país y apoyará al proceso de mejoras en materia de políticas publicas. Previamente se han desarrollado bases de datos similares (por ejemplo, para Canadá) y esas experiencias pueden adaptarse al contexto de México",
                            a("(Cisneros-Montemayor et al. 2016).",
                              href="http://www.nrcresearchpress.com/doi/pdf/10.1139/cjfas-2015-0573")
                          ),
                          p(h3(
                            "Objetivos del proyecto"
                          )),
                          p("El objetivo principal de este proyecto es crear una base de metadatos que contenga información sobre datos ecológicos, económicos, oceanográficos y sociales, referentes al ambiente marino. Así mismo, se pretende identificar tendencias en la disponibilidad de datos marinos en México e identificar oportunidades de mejorar la información marina en México. Por último, se pretende fomentar el acceso a la información mediante un portal de consulta de metadatos en línea."
                          ),
                          p(
                            "La disponibilidad de datos es clave no sólo para comprender mejor los ambientes marinos y costeros de México, sino también para identificar las brechas de conocimiento, apoyando así a la prioritización de la investigación en México. Por ejemplo, la disponobilidad de información facilitará el manejo de recursos marinos y las políticas de conservación para los ecosistemas marinos y los recursos pesqueros vulnerables al cambio climático"
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
                        # Core Group Infomation ####
                        column(
                          8,
                          align = "justified",
                          offset = 2,
                          p(h3(
                            "Investigadores principales"
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
                          # Bloqueado hasta nuevo aviso de INAPESCA
                          # a(href="http://www.inapesca.gob.mx",
                          #   img(
                          #     src = "inapesca_logo.JPG",
                          #     height = 100,
                          #     width = 300
                          #   )),
                          a(href="http://mexico.edf.org/",
                            img(
                              src = "edf_logo.jpg",
                              height = 100,
                              width = 250
                            ))
                        )
                      )
             ), # Close second page
             #.########################## ##### 
             #### METADATA ####
             tabPanel("Metadata",
                      h3(
                        "Metadata de Investigación Marina en México"
                      ),
                      p("A continuación presentamos la base de metadatos que se ha recopilado desde Noviembre de 2016, aquí podrá consultar la información que se ha recopilado hasta ahora, así como la referencia para cada uno de los datos."),
                      p(
                        "Si usted es autor o responsable de alguna información presente en esta tabla y/o encuentra un error, favor de comunicarse con nosotros."
                      ),
                      p(strong("Nota:"),"La información aquí presente no es final ya que el proyecto sigue en curso y la información aún está siendo recolectada"),
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
                          "En esta sección se muestran algunos de los resultados previos relacionados con la investigación marina en México. Dichos resultados están directamente relacionados con la información que se va recaudando por lo que cambian constantemente mientras crece la base de metadatos. Asímismo, estos resultados son parciales y no representan el total de la investigación marina en México."
                        ) #,
                        # p(strong("Nota:"),"Debido al tamaño de la base de metadatos esta sección tarda algunos segundos (~20) en cargar.",
                        #   style = "color:red"
                        #   )
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
                        selectInput(inputId = "Discipline", 
                                    label= "Seleccione una opción:",
                                    selected = NULL,
                                    choices = list("Seleccione" = "NOT",
                                                   "Acuacultura" = "Aquaculture",
                                                   "Ecología" = "Ecology",
                                                   "Oceanografía" = "Oceanography",
                                                   "Pesquerías" ="Fisheries",
                                                   "Turismo" = "Tourism",
                                                   "Sociología" = "Sociology",
                                                   "Todas" = "Todas"
                                                   
                                    )
                        ),
                        plotOutput("Keywords_Plot"),
                        p(em("Nota: Es posible que no todas las palabras se encuentren en la gráfica"))
                        
                      ),
                      column(
                        width = 8,
                        align= "center",
                        offset = 2,
                        p(h3(
                          "Histórico de Datos Incluidos en los Metadatos"
                        )),
                        p(actionButton("Tomeseries_But", 
                                       "Generar Histórico de Datos")),
                        dygraphOutput("TSgraph")
                      )
             ),
             #.########################## ##### 
             #### PARTICIPATION ####
             navbarMenu(strong("Como Participar"),
             tabPanel(strong("Como Participar"),
                        id="Collaboration",
                        value="Collaborate",
                        column(8,
                             align="justified",
                             offset = 2,
                             h3("Colabora en el Desarollo de La Investigación Marina en México"),
                             p("Estamos buscando cualquier fuente de información que contenga datos acerca de
                               temas marinos en México. ¡No importa la fuente, pueden ser tus datos de
                               la tesis, de captura o algún reporte!"),
                             p("El proyecto se encuentra en una etapa de colaboración que consiste en hacer crecer la base de datos, por lo que invitamos a cualquier persona interesada a colaborar con el proyecto a compartir información sobre sus datos u ofrecer comentarios para el desarrollo del proyecto."),
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
                               "Para que el proyecto mejor refleje la investigación marina en México es necesario tener la mayor cantidad de información posible. Recuerda que no estamos colectando datos crudos, estamos colectando información acerca de los datos"
                             ),
                             downloadButton('downloadTemp',
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
                             "Correo: j.palacios@oceans.ubc.ca"
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
                               "Creemos que en México existe mucha información respecto al ambiente marino, sin embargo un pequeó grupo de personas no son capaces de capturar toda la información. Cuanta más gente esté involucrada, más información podemos colectar y mejor podremos reflejar el estado actual de la investigación marina en México, así como descubrir campos con falta de información."
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
                               "Como parte de de una comunidad comprometida con la investigación del medio ambiente marino mexicano, compartir nuestros datos trae importantes beneficios. La creación de sistemas para administrar y compartir datos asegura la preservación, administración y acceso a la información",
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
                               "Si bien la gran mayoría de la información recopilada hasta ahora es de acceso público en línea, hemos comenzado a recibir información de datos de fuentes no publicadas o simplemente no disponibles en internet" 
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
  tabPanel("Poster",
           column(10,
                  align = "center",
                  p(h4(strong(
                    "¡Copia y Comparte!"))),
           img(
             src = "Poster.jpg",
             height = 1000,
             width = 600
           )
           )
  )
  ),
  #### IDIOMA ####
  navbarMenu("Idioma",
             tabPanel(a("Inglés",
                        href="https://jepa.shinyapps.io/marmetadatamexeng/")
                      )
  ),
  #### Floating Menu ####
  column(8,
         align = "center",
         absolutePanel(
           top = 100,
           bottom = 20,
           right = 5,
           #width = "25%",
           draggable = TRUE,
           fixed = TRUE,
           wellPanel(
             strong("Datos Colectados:"),
             textOutput("Datapoints_Intro"),
             "Actualizado:",
             textOutput("date")
           )
         )
  ),
  #.########################## #####
  #### CONTACT ####
  column(8,
         align = "center",
         offset = 2,
         hr(),
         br(),
         h5("Información de Contacto")
  ),
  column(2,
         align = "center",
         offset = 2,
         p(strong("Autor Corresponsal:"),"Juliano Palacios j.palacios@oceans.ubc.ca")
  ),
  column(2,
         align = "center",
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
         ), href = "https://www.researchgate.net/project/Metadata-base-of-marine-research-in-Mexico-trends-and-applications")
         )
  )
)
)

