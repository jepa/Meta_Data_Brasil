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
    "Meta-database of Marine Research in Mexico",
    #### Inicio ####
    tabPanel("Inicio",
             fluidRow(
               column(
                 12,
                 align = "center",
                 h1("Towards the Creation of a Meta-database of Marine Research in Mexico")
               ),
               column(
                 10,
                 align = "justified",
                 offset = 1,
                 p(h3("Welcome!")),
                 p(
                   "Research and management of marine resources increasingly depends on various biological, ecological, social, and economic data. The availability of data is often perceived as a gap in advancing research and policy discussion. However, in many cases, this is largely a result of the lack of knowledge about the availability of these data. In Mexico, numerous information covering the seas and coasts can be found in academic institutions, government, and NGOs located (physically) both inside and outside the country. While diverse barriers often compromise the exchange of information among stakeholders, having publicly accessible description on existing data is a huge step towards increasing collaboration and innovative research." 
                 ),
                 p(h3(
                   "Proyect Objectives"
                 )),
                 p("The main objective of this project is to create a meta-database for oceanographic, ecological, economic and social data for marine ecosystems and marine-related sectors of Mexico. Moreover, we aim to identify the major trends in marine data availability in Mexico as well as information and research gaps that should be addressed in the future. Finally, the meta database is expected to be public, self-maintaining and available for consultation"
                 ),
               p(strong("Climate Change"),
                 "Data availability is key to climate change research. That is why this project intends to contribute substantially to the research carried out in Mexico on the impacts of climate change. By improving our understanding that climate change will have on marine resources, we can support policies designed to better manage the country's marine resources."
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
                   p(h3("Participants and Institutions")),
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
             #Partners Information####
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
             )
    ), # Close first page
   #.########################## ##### 
    #### METADATA ####
    tabPanel("Metadata",
             h3(
               "Metadata of Marine research in Mexico"
             ),
             p("In this section you will find the metadata base that has been compiled since November 2016. In addition to surf the database, you can download the list of references that have been consulted to date."),
             p(
               "If you are the author or responsible for any information in this database, and find an error, please contact us."
             ),
             p(strong("Note:"),"The information here presented is partial. The project is not yet complete and the information is still being collected"),
             column(
               width=12,
               align = "center",
               tabsetPanel(
                 id ="Data_Explorer",
                 tabPanel(
                   p(h4("Metadata Base")),
                   dataTableOutput('Metadata')
                   ),
                 tabPanel(
                   p(h4("Reference List")),
                   column(
                     width = 12,
                     align = "center",
                     radioButtons('format',
                                  'Select Format',
                                  c('PDF',
                                    'HTML',
                                    'Word'),
                                  inline = TRUE),
                     downloadButton('downloadReport',
                                    "Download Reference List")
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
    tabPanel("Preliminary Results",
             #Wellcome / Instructions####
             p(h3(
               "Preliminary Results"
               )),
             p(
               "In this section you will be presented with some of the preliminary results. These results are directly related to the information that is being collected. Likewise, these results are partial and do not represent the total of marine research in Mexico."
               ),
             column(
               width=4,
               align = "center",
               p(h3("Registers")),
               p(h4(textOutput("Number_Entries")))
             ),
             column(
               width=4,
               align = "center",
               p(h3("Data Number")),
               p(h4(textOutput("Number_Data_Points")))
             ),
             column(
               width=4,
               align = "center",
               p(h3("References/Repositories")),
               p(h4(textOutput("Sources")))
             ),
             br(),
             br(),
             column(
               width = 12,
               align = "center",
               dygraphOutput("TFgraph"), #Timeframe graph
               p(h3("Preliminary Results (Quantitative & Qualitative)"))
             ),
             #.########################## ##### 
             #### PRELIMINARY RESULTS ####
             #### Quantitative Results####
             column(
               width = 6,
               align= "center",
               p(h2("Quantitative Results")),
               br(),
               p(h3("Data per Area")),
               selectInput("Plot_Option", 
                           label= "Select one option:",
                           choices = list("Area" = 1, 
                                          "Region" = 2,
                                          "Location" =3
                           )
               ),
               plotOutput("Number_spp"),
               sliderInput("Num_Data_Range",
                           "Select the amount to display (Location)",
                           value=10,
                           min = 1,
                           max = 50),
               #### SE_Component ####
               p(h3("Socio-Economic Component")),
               plotOutput("SE_Component")
             ),
             #### Qualitative Results####
             ####Keywords Word Cloud####
             column(
               width = 6,
               align= "center",
               p(h2("Qualitative Results")),
               br(),
               p(h3("Frequent Keywords")),
               textInput("Keyword_Remove1",
                         "Remove Any Word",
                         "",
                         width = '50%'),
               textInput("Keyword_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Keywords_Plot"),
               p(em("Note: It is possible that not all words are represented")),
               #### Subject_Name Word Cloud ####
               p(h3("Frequent Subjects")),
               textInput("Subject_Remove",
                         "Remove Any Word",
                         "",
                         width = '50%'),
               textInput("Subject_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Subject_name_Plot"),
               p(em("Note: It is possible that not all words are represented"))
             ),
             #### Experimental Analysis ####
             column(width =12,
                    align = "center",
                    p(h3("Data Points by Socioeconomic Component and Geogrpahic Location")),
                    selectInput("SE_E_Plot_Option", 
                                label= "Select an Option",
                                choices = list("Area" = 1, 
                                               "Region" = 2,
                                               "Location" =3
                                ),
                                width = "20%"
                    ),
             plotOutput("SE_Component_Area"),
             br(),
             p(h3("Data Points by Research Field and Geogrpahic Location")),
             selectInput("Research_Field_Plot_Option", 
                         label= "Select an Option:",
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
    #### Collaboration ####
    tabPanel(strong("Collaboration"),
             h3("Collaborate in the Development of Marine Research in Mexico"),
            p("We are looking for any source of information that contains data on
             Marine research in Mexico. It does not matter the source. It can be your thesis data, grey literature, or citizen science!"),
            p("The project is on a collaborative stage that consists of populating the database so we invite anyone interested to collaborate with the project to share information about their data."),
            p("It is important to mention that", strong ("we are not gathering hard data, but rather information about them.")
              ),
            "The more people involved, the better we can reflect the state of marine research in Mexico,",
            br(),
            column(12,
                   align="justified",
              p(h3(
                "Three Ways to Collaborate"
                   ))
              ),
            column(4,
                   align="justified",
                   h3(img(
                     src = "Colaborar.png",
                     height = 60,
                     width = 60,
                     "1. Share"
                   )),
                   p(
                     "In order for the project to better reflect marine research in Mexico, it is necessary to have as much information as possible. Remember that we are not collecting raw data, we are collecting information about the data"
                     ),
                   downloadButton('downloadTemplate',
                                  "Download Template")
                   ),
            column(4,
                   align="justified",
                   h3(img(
                     src = "Busca.png",
                     height =60,
                     width = 60,
                     "2. Inform"
                   )),
                   p(
                     "We are looking for any source of information relevant to marine research in Mexico. No matter the source, they can be data of thesis, publications, monitoring, gray literature or official reports"
                   ),
                   a(img(
                     src = "Twitter_Logo.png",
                     height = 40,
                     width = 40
                   ), href = "https://twitter.com/julianop_a"
                   ),
                   "Email"
            ),
            column(4,
                   align="justified",
                   h3(img(
                     src = "Comparte.png",
                     height = 60,
                     width = 60,
                     "3. Communicate"
                   )),
            p(
              "We believe that in Mexico there is a lot of information relevant to the marine environment, however a small group of people are not able to capture all the information. The more people are involved, the more information we can collect and the better we can reflect the current state of marine research in Mexico as well as discover fields with lack of information."
            )
            ),
            column(12,
                   align = "justified",
                   p(h3(
                     "People and Institutions Collaborating"
                     )),
                   p(
                     "While the vast mayority of the information collected until now is publically accessed online (See References), we have started recieving data information from un-published or non-available sources" 
                   ),
                   column(6,
                          align = "center",
                          p(h4(strong(
                            "Institutions"
                          ))),
                          dataTableOutput("Institutions")
            ),
            column(6,
                   align = "center",
                   p(h4(strong(
                     "People"
                   ))),
                   dataTableOutput("People")
            )
            )
            ),
    #.########################## ##### 
    #### CONTACT ####
    column(12,
           align = "center",
           hr(),
           br(),
           h5("Contact Information")
           ),
    column(3,
           align = "center",
           p(strong("Corresponding author:"),"Juliano Palacios j.palacios@oceans.ubc.ca")
    ),
    column(3,
           align = "center",
           p(strong("Phone:"), "+1 (778) 835 4298"),
           strong("Skype:"), "jepa_88"
           ),
    column(4,
           align ="justified",
           p(strong("Address:"), "Changing Oceans Research Unit, The Institute for Oceans and Fisheries, University of British Columbia. 2202 Main Mall, Vancouver, Canadá, BC V6T 1Z4")
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
