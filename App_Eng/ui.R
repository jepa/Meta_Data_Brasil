#### UI for Metadata APP ####

# This script is part of the Metadata of Marine Research in Mexico app.
#Started on October, 2016
#Juliano Palacios Abrantes, j.palacios@oceans.ubc.ca
#________________________________________________________________________#

#### NOTES ####
# Everything you do here, you have to miror in the "App_Esp". If the spanish webpage is stil on-line.

#_______________________________ END NOTES __________________________________#

####Libraries needed ####
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
                 h1("Building a Meta-database of Marine Research in Mexico"),
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
                 p(h3("Welcome!")),
                 p(
                   "The Meta-database of Marine Research project aims to create a meta-database of all marine research data developed in Mexico. A meta-database is a documentation of the sources of information instead of a database of the actual data. Since October of 2016 we have collected more than 28,000 data points, from 12 on-line repositories and collaborated with more than 17 professionals of different disciplines. However, we are looking for any available information regarding marine research in Mexico, regardless of the source. It can be your thesis data, grey literature, or citizen science!"
                 )
                 ),
                 column(
                   8,
                   align = "center",
                   offset = 2,
                   p(strong(h4(
                     "The more people involved, the better we can reflect the state of marine research in Mexico!"
                   ))),
                   p(actionButton("Collaborate_But", "How Can I Collaborate?"))
                 ),
               #Partners Information####
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
    tabPanel("The Project",
             fluidRow(
                 column(
                   8,
                   align = "justified",
                   offset = 2,
                 p(h3("The Project")),
                 p(
                   "Research and management of marine resources increasingly depends on various biological, ecological, social, and economic data. Limited information is often perceived as a barrier in advancing research and policy discussions. However, in many cases, this can be a result of a lack of knowledge about the existence and availability of these data. In Mexico, numerous information regarding the seas and coasts has been produced by academic institutions, government, and NGOs both inside and outside the country. While diverse barriers can compromise the exchange of information among stakeholders, having publicly accessible descriptions of existing data is a huge step towards increasing collaboration and innovative research. A meta-database fosters collaboration and eases the process of informing best policies relevant to any community or region (shown in the diagram below). Similar meta-databases have been previously developed (for example, for Canada) and such experiences can be readily be adapted for Mexico",
                   a("(Cisneros-Montemayor et al. 2016).",
                     href="http://www.nrcresearchpress.com/doi/pdf/10.1139/cjfas-2015-0573")
                 ),
                 p(h3(
                   "Project Objectives"
                 )),
                 p("The main objective of this project is to create a meta-database for oceanographic, ecological, economic, fisheries and social data for marine ecosystems and marine-related sectors of Mexico. Moreover, we aim to identify the major trends in marine data availability in Mexico as well as information and research gaps that can be addressed in the future. Finally, the meta-database will be public and available for consultation, and is expected to be largely self-maintaining"
                 ),
                 p(
                 "Data availability is key not only to better understand Mexico's marine and coastal environments, but to identify knowledge gaps so that research can be prioritized. This will facilitate furnishing management and conservation policies, for example, for marine habitats and fisheries resources vulnerable to climate change."
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
                 p(h3("Lead Researchers")),
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
               # Partners information
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
                 #Blocked until further information (INAPESCA)
                 # a(href="http://www.inapesca.gob.mx",
                 #   img(
                 #     src = "inapesca_logo.JPG",
                 #     height = 80,
                 #     width = 300
                 #   )),
                 a(href="http://mexico.edf.org/",
                   img(
                     src = "edf_logo.JPG",
                     height = 80,
                     width = 200
                   ))
               )
             )
             ),
    #.########################## ##### 
    #### METADATA ####
    tabPanel("Metadata",
             column(
               width=8,
               offset = 2,
               align = "justified",
             h3(
               "Metadata of Marine research in Mexico"
             ),
             p("In this section you will find the meta-database that has been compiled since November 2016. In addition to surfing the database, you can download the list of references that have been consulted to date."),
             p(
               "If you find an error, or an author wishing to make changes to records included in the meta-database, please contact us."
             ),
             p(strong("Note:"),"The information here presented is partial. The project is ongoing and information is still being collected")
             ),
             column(
               width=8,
               offset = 2,
               align = "justified",
             tabsetPanel(
               id ="Data_Explorer",
                 tabPanel(
                   p(h4("Metadata Base")),
                   dataTableOutput('Metadata')
                 ),
                 tabPanel(
                   p(h4("Quick Link")),
                   dataTableOutput('Metadata_Summary')
                 ),
                 tabPanel(
                   p(h4("Metadata Key")),
                   dataTableOutput('Metadata_Key')
              )
             )
             )
    ), #Close second page
    #.########################## ##### 
    #### PRELIMINARY RESULTS ####
    tabPanel("Preliminary Results",
             #Wellcome / Instructions####
             column(
               width=8,
               offset = 2,
               align = "justified",
             p(h3(
               "Preliminary Results"
             )),
             p(
               "In this section you will be presented with some preliminary results. These results are directly related to the information that is being collected and do not represent all marine research in Mexico."
             )#,
             # p(strong("Note:"),"Due to the size of the metadata base this section takes a few seconds to load.",
             #   style = "color:red"
             # )
             ),
             column(
               width=3,
               align = "center",
               offset = 1,
               p(h3("Records")),
               p(h4(textOutput("Number_Entries")))
             ),
             column(
               width=3,
               align = "center",
               p(h3("Data Points")),
               p(h4(textOutput("Number_Data_Points")))
             ),
             column(
               width=3,
               align = "center",
               p(h3("Repositories")),
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
                 "Time Series of Data Information Gathering"
               )),
               dygraphOutput("TFgraph"), #Timeframe graph
               p(h3("Preliminary Results (Quantitative & Qualitative)"))
             ),
             #### Quantitative Results####
             column(
               width = 4,
               offset = 2,
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
                           value=8,
                           min = 1,
                           max = 50)
             ),
             #### Qualitative Results####
             ####Keywords Word Cloud####
             column(
               width = 4,
               align= "center",
               p(h2("Qualitative Results")),
               br(),
               selectInput(inputId = "Discipline", 
                           label= "Select an Option:",
                           selected = NULL,
                           choices = list(" "="",
                                          "Aquaculture" = "Aquaculture",
                                          "Ecology" = "Ecology",
                                          "Oceanography" = "Oceanography",
                                          "Fisheries" ="Fisheries",
                                          "Tourism" = "Tourism",
                                          "Sociology" = "Sociology",
                                          "All" = "Todas"
                                          
                           )
               ),
               p(actionButton("Words_But", 
                              "Plot!")),
               plotOutput("Keywords_Plot"),
               p(em("Note: It is possible that not all words are represented"))
               
             ),
             column(
               width = 8,
               align= "center",
               offset = 2,
               p(h3(
                 "Reconstruction of Historic Data from Metadata"
               )),
               p(actionButton("Tomeseries_But", 
                              "Plot Histric Data")),
               dygraphOutput("TSgraph")
             )
    ),
    #.########################## ##### 
    #### Collaboration ####
    tabPanel(id="Collaboration",
             value="Collaborate",
      strong("Collaboration"),
      column(8,
             align="justified",
             offset = 2,
             h3("Collaborate in the Development of Marine Research in Mexico"),
             p("We are looking for any available information regarding
               marine research in Mexico, regardless of the source. It can be your thesis data, grey literature, or citizen science!"),
             p("The project depends on wide collaboration to populate the database, so we invite anyone interested in collaborating with the project to contact us and share information about their data."),
             p("It is important to mention that", strong ("we are not gathering hard data, but rather information about them.")
             ),
             "The more people involved, the better we can reflect the state of marine research in Mexico"
             ),
             br(),
             column(8,
                    align="justified",
                    offset = 2,
                    p(h3(
                      "Three Ways to Collaborate"
                    ))
             ),
             column(3,
                    align="justified",
                    offset = 2,
                    h3(img(
                      src = "Colaborar.png",
                      height = 60,
                      width = 60,
                      "1. Share"
                    )),
                    p(
                      "In order for the project to better reflect marine research in Mexico, it is necessary to have as much information as possible. Remember that we are not collecting raw data, we are collecting information about what data exists"
                    ),
                    # p("Donwload"),
                    downloadButton('downloadTemp',
                                   "Download Template")
                    # downloadButton('downloadIns',
                    #                "Instructions")
             ),
             column(2,
                    align="justified",
                    h3(img(
                      src = "Busca.png",
                      height =60,
                      width = 60,
                      "2. Inform"
                    )),
                    p(
                      "We are looking for any source of information relevant to marine research in Mexico. No matter the source, they can be data from dissertations, publications, monitoring programs, gray literature or official reports"
                    ),
                    a(img(
                      src = "Twitter_Logo.png",
                      height = 40,
                      width = 40
                    ), href = "https://twitter.com/julianop_a"
                    ),
                    "Email: j.palacios@oceans.ubc.ca"
             ),
             column(3,
                    align="justified",
                    h3(img(
                      src = "Comparte.png",
                      height = 60,
                      width = 60,
                      "3. Communicate"
                    )),
                    p(
                      "We believe that in Mexico there is a lot of existing information relevant to the marine environment, however a small group of people are not able to capture all the information. The more people are involved, the more information we can collect and the better we can reflect the current state of marine research in Mexico, as well as highlight fields with limited information."
                    )
             ),
             #### What do I win? ####
             column(8,
                    align = "justified",
                    offset = 2,
                    p(h3(
                      "Benefits of Collaborating"
                    )),
                    p(
                      "Data information sharing has many benefits to both individuals and society. Having your data information included in the Metadata will increase the visibility of your research. Your data could be potentially be useful for other researchers to answer different questions, and they would contact you directly. This will ultimately foster collaboration among national and international researchers that could result in important advances for the country", 
                      a("(Michener 2006).",
                        href="http://www.sciencedirect.com/science/article/pii/S15749548500004X"
                      )
                    ),
                    p(
                      "As a community committed to research of Mexico's marine environment, sharing our data brings important benefits. Building systems for managing and sharing data ensures preservation, stewardship and access to information",
                      a("(Fridell et al., 2014).",
                        href="http://datascience.codata.org/articles/abstract/8.2481/dsj.IFPDA-01/"), "It will also allow us to understand what information is out there, and identify fields of research that need to be further developed"
                    )
             ),
             #### People Collaborating ####
             column(8,
                    align = "justified",
                    offset = 2,
                    p(h3(
                      "People and Institutions Collaborating"
                    )),
                    p(
                      "While the vast majority  of the information collected so far is publically accessible online, we have started receiving data information from unpublished or otherwise not readily-available sources" 
                    )
                    ),
                    column(4,
                           align = "center",
                           offset = 2,
                           br(),
                           p(h4(strong(
                             "Institutions"
                           ))),
                           dataTableOutput("Institutions")
                    ),
                    column(4,
                           align = "center",
                           p(h4(strong(
                             "People"
                           ))),
                           dataTableOutput("People")
                    )
             
             ), # CLose Collaboration tab
    #### DATA POINTS ####
    navbarMenu("Language",
               tabPanel(a("Spanish",
                          href="https://jepa.shinyapps.io/marmetadatamexesp/")
               )
    ),
  column(8,
           align = "center",
    absolutePanel(
      top = 100,
      bottom = 20,
      right = 20,
      #width = "25%",
      draggable = TRUE,
      fixed = TRUE,
      wellPanel(
        strong("Data Points collected:"),
      textOutput("Datapoints_Intro"),
      "Last updated:",
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
           h5("Contact Information")
           ),
    column(2,
           align = "center",
           offset = 2,
           p(strong("Corresponding author:"),"Juliano Palacios j.palacios@oceans.ubc.ca")
    ),
    column(2,
           align = "center",
           p(strong("Phone:"), "+1 (778) 835 4298"),
           strong("Skype:"), "jepa_88"
    ),
    column(3,
           align ="justified",
           p(strong("Address:"), "Changing Oceans Research Unit, The Institute for Oceans and Fisheries, University of British Columbia. 2202 Main Mall, Vancouver, Canada, BC V6T 1Z4")
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
