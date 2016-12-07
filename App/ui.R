library(shiny)
library(leaflet)
library(DT)

shinyUI(
  navbarPage(
    theme = "fondo.css",
    "Metadata of Marine Research in Mexico",
    #### HOME ####
    tabPanel("Home",
             fluidRow(
               column(
                 12,
                 align = "center",
                 h1("Project Outline")
               ),
               column(
                 10,
                 align = "justified",
                 offset = 1,
                 p(h3("Welcome!")),
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
                   )
                 )
                 
               )
             )
    ),
    #### INPUT DATA ####
    tabPanel(
      "Input Data",
      fluidPage(
        column(12,align="center",
               titlePanel("Main Pannel for Uploading Data to the Meta-Dataset")),
        fluidRow(
          column(12,align="justified",
                 h3("Instructions"),
                 p("In here we should have a very brieff text informing the instructions on how to fill the blanks"
                 )
          ),
          #### first Column####
          ######Contact Info ####
          column(
            width = 4,
            p(h3("Contact Information")),
            #Mean and sd Harvestable Biomass#
            textInput("User_Name",
                      "Name",
                      "",
                      width = '100%'),
            textInput("User_Email",
                      "Email",
                      "",
                      width = '100%'),
            ###### Data Rpository ####
            p(h3("Repository Information")),
            tabsetPanel(
              id = "Data_Input",
              ###### Reference ####
              tabPanel(
                "Reference",
                textInput("Reference_",
                          "Please provide a path", 
                          "")
              ),
              ###### Data Collection ####
              tabPanel(
                "Compilation",
                textInput("Compilation_Title",
                          "Name of Data Compilation", 
                          "")
              ),
              ###### Help #
              tabPanel(
                "Help",
                strong("Reference"),"The digital path of where can the data be found. If no digital repository exists, please provide the name of the publication where data can be found",
                p(strong("Compilation"),"The name of the collection where data exists."),
                br(),
                p(em("For example:"),"The dataset could be",em("Shark Catch for Mexico Between 2000-2014"), "and the Collection where the data was published could be the:",em("FAO Yearbook of Fishery and Aquaculture Statistics, 2014"))
              )
            ),
            fileInput('Data_Upload',
                      h4('Upload Data'),
                      accept=c('text/csv',
                               'text/comma-separated-values,text/plain','.csv'))
          ),
          #### Second column ####
          column(
            width = 4,
            p(h3("General Data Information")),
            tabsetPanel(
              id = "General_Info",
              #######Dataset_Title ####
              tabPanel(
                "Data Title",
                textInput("Dataset_Title",
                          "Title of Original Dataset", 
                          "")
              ),
              #######Shot_Title ####
              tabPanel(
                "Short Title",
                textInput("Short_Title",
                          "Capture Title", 
                          "")
              ),
              #######Keywords ####
              tabPanel(
                "Keywords",
                textInput("Keyword_1",
                          "Keyword 1", 
                          "",
                          width = '50%'),
                textInput("Keyword_2",
                          "Keyword 2", 
                          "",
                          width = '50%'),
                textInput("Keyword_3",
                          "Keyword 3", 
                          "",
                          width = '50%'),
                textInput("Keyword_4",
                          "Keyword 4",
                          "",
                          width = '50%')
              ),
              #######Author ####
              tabPanel(
                "Author",
                textInput("Author",
                          "Author of the Dataset", 
                          ""),
                textInput("Institution",
                          "Institution", 
                          "")
              ),
              #######Generla_Info_Help ####
              tabPanel(
                "Help",
                p(
                  strong(
                    "Data Title"
                  ),
                  "Original name of dataset.", em("Note"), ": the data specifically, not the source or repository where it was published"
                ),
                p(
                  strong(
                    "Short Title"
                  ),
                  "Please prove a short title that better describes the dataset. Please keep it consize. There is a limit of 8 words"
                ),
                p(h5("Example")),
                p(
                  "Shark Survey for Mexican Pacific Coast between 1980-2000"
                ),
                p(
                  strong(
                    "Keywords"
                  ),
                  "Limited to 8 words that describe the subject or dataset and are useful for searches"
                ),
                p(
                  strong(
                    "Author"
                  ),
                  "Author (Principal) of the dataset for referencing the data. Can be multiple authors and institutions."
                )
              )
            ),
            ######Dataset_Available####
            tabsetPanel(
              id = "inTabset",
              tabPanel(
                "Is the Data Set Available?",
                selectInput("Dataset_Available", 
                            label= "Choose Only One",
                            choices = list("Public Access" = 1, 
                                           "Restricted Access (Public)" = 2,
                                           "Private" = 3),
                            selected = 0,
                            width = '50%')
              ),
              tabPanel(
                "Help",
                p(
                  "Is the data available for use?"
                ),
                p(h5("Example")),
                p(
                  "Could be: NGO, Gov, Academia, IGO, Industry,  Private. (Multiple)"
                )
              )
            ),
            tabsetPanel(
              id = "Institution_type",
              tabPanel(
                "Institution Collecting Data",
                checkboxGroupInput("Institution_Type",
                                   label = "Chose One or Multiple", 
                                   choices = list("NGO" = 1,
                                                  "Gob." = 2, 
                                                  "Academy" = 3,
                                                  "IGO", 
                                                  "Industry" = 5 ), 
                                   selected = 0)
              ),
              tabPanel(
                "Help",
                p(
                  "The type of institution that generated the dataset (corresponds to the Institution field. Can be multiple selection)"
                ),
                p(h5("Example")),
                p(
                  "Could be: NGO, Gov, Academia, IGO, Industry,  Private. (Multiple)"
                )
              )
            )
          ),
          #Close second column
          #### Third COLUMN, Spatial####
          column(
            p(h3("Spatial Information")),
            width = 3,
            tabsetPanel(
              id ="Spatial_Information",
              ###### Area ####
              tabPanel(
                "Area",
                selectInput("Area", 
                            label= "Choose Only One",
                            choices = list("National" = 1, 
                                           "Atlantic" = 2,
                                           "Pacific" = 3,
                                           "FreshWater/Terrestrial" = 4),
                            selected = 0,
                            width = '100%')
              ),
              ###### Region ####
              tabPanel(
                "Region",
                checkboxGroupInput("Region", 
                                   label= "Choose all tha Apply",
                                   choices = list("Gulf of California" = 1, 
                                                  "Nortwest Pacific" = 2,
                                                  "South Pacific" = 3,
                                                  "Gulf of Tehuantepec" = 4,
                                                  "Yucatan Peninsula / Mar Caribe" = 5,
                                                  "West Gulf of Mexico" = 6,
                                                  "FreshWater/Terrestrial" = 7),
                                   selected = 0,
                                   width = '100%'
                )
              ),
              ###### Location ####
              tabPanel(
                "Location",
                textInput("Location",
                          "Location",
                          "",
                          width = '100%'),
                actionButton(
                  "Location_Map",
                  "Map"
                )
              ),
              ###### Spatial_Help ####
              tabPanel(
                "Help",
                p(strong("Area:"),"A broad category of where the data was generated, cannot be lefted in blank"),
                p(strong("Region:"),"A more specific location category, can be left blank"),
                p(strong("Location:"),"A very specific localization category. Can be anything from an specific city or beach to coordinates."),
                p(strong("Map:"),"Brings out a map so you can geo-reference the data")
              )
            )
          )
        )
      ),
      mainPanel(em(
        column(
          width = 12,
          align ="center",
          leafletOutput("Location_Map")
          
        )
      )
      )
    ),
    #### PRELIMINAR RESULTS ####
    tabPanel("Metadata",
             p("Hola"),
             column(
               width=12,
               align = "center",
               tabsetPanel(
                 id ="Data_Explorer",
                 tabPanel(
                   p(h3("Metadata Explorer")),
                   column(width=12,
                          align = "center",
                          downloadButton('MMID_Download', 
                                         'Download All Meta-data')
                   ),
                   dataTableOutput('Metadata')
                   ),
                   tabPanel(
                     p(h3("Data Available")),
                     column(
                       width=12,
                       align = "center",
                     numericInput("MMID_Download_Selection",
                                  "Select the MMID to download",
                                  value = 1,
                                  min = 1,
                                  max = "na",
                                  step = 1,
                                  width = '25%'),
                     downloadButton('Data_Download', 
                                    'Download Specific Data')
                     ),
                     dataTableOutput('Available_Data')
                   )
              )
             )
             ),
    tabPanel("Premilinar Results",
             p("Hola"),
             column(
               width=12,
               align = "center",
               tabsetPanel(
                 id = "Map_Raw",
                 tabPanel(
                   p(h3("Map of Data Localization")),
                   leafletOutput("Data_Map")
               )
               )
             ),
             br(),
             br(),
             #### Quantitative Results####
             column(
               width = 6,
               align= "center",
               p(h3("Quantitative Results")),
               p(h4("Total Number of Data Registers")),
               p(h2(textOutput("Number_Entries"))),
               p(h4("Number of Data Enteries per Area")),
               selectInput("Plot_Option", 
                           label= "Choose To Show Plot:",
                           choices = list("By Area" = 1, 
                                          "By Region" = 2,
                                          "By Location" =3
                           )
               ),
               plotOutput("Number_spp") ,
               sliderInput("Num_Data_Range",
                           "Select the Top Categories",
                           value=10,
                           min = 1,
                           max = 50)
             ),
             #### Qualitative Results####
             ####Keywords Word Cloud####
             column(
               width = 6,
               align= "center",
               p(h3("Qualitative Results")),
               textInput("Keyword_Remove1",
                         "Remove any Keyword",
                         "",
                         width = '50%'),
               textInput("Keyword_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Keywords_Plot"),
               p(em("Note: Not all words might be represented in the graph")),
               #### Subject_Name Word Cloud ####
               p(h4("Subject Name Word Cloud")),
               textInput("Subject_Remove",
                         "Remove Any Subject",
                         "",
                         width = '50%'),
               textInput("Subject_Remove2",
                         "",
                         "",
                         width = '50%'),
               plotOutput("Subject_name_Plot"),
               p(em("Note: Not all words might be represented in the graph"))
        
               )
    )
  )
)










