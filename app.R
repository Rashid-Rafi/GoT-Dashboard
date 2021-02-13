# Load packages
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(ggplot2)
library(readr)
library(sf)
library(dplyr)
library(cartography)

# Load data
load("./GoT.RData")

## app.R ##
ui <- dashboardPage(
  dashboardHeader(title = "Game of Thrones"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("World", tabName = "world", icon = icon("globe")),
      menuItem("Characters", tabName = "characters", icon = icon("users")),
      menuItem("Houses", tabName = "houses", icon = icon("home"),
               menuSubItem("Arryn", tabName = "Arryn"),
               menuSubItem("Baratheon", tabName = "Baratheon"),
               menuSubItem("Bolton", tabName = "Bolton"),
               menuSubItem("Frey", tabName = "Frey"),
               menuSubItem("Greyjoy", tabName = "Greyjoy"),
               menuSubItem("Lannister", tabName = "Lannister"),
               menuSubItem("Martell", tabName = "Martell"),
               menuSubItem("Mormont", tabName = "Mormont"),
               menuSubItem("Stark", tabName = "Stark"),
               menuSubItem("Targaryen", tabName = "Targaryen"),
               menuSubItem("Tarly", tabName = "Tarly"),
               menuSubItem("Tully", tabName = "Tully"),
               menuSubItem("Tyrell", tabName = "Tyrell"),
               menuSubItem("Umber", tabName = "Umber")
               ),

      menuItem("Kingdoms", tabName = "kingdoms", icon = icon("map-marker-alt"),
               menuSubItem("Dorne", tabName = "Dorne"),
               menuSubItem("Iron Islands", tabName = "II"),
               menuSubItem("The Crownlands", tabName = "Crownlands"),
               menuSubItem("The North", tabName = "North"),
               menuSubItem("The Reach", tabName = "Reach"),
               menuSubItem("The Riverlands", tabName = "Riverlands"),
               menuSubItem("The Stormlands", tabName = "Stormlands"),
               menuSubItem("The Vale of Arryn", tabName = "Vale"),
               menuSubItem("The Westerlands", tabName = "Westerlands")
               ),
      menuItem("More", tabName = "more", icon = icon("info-circle"),
               menuSubItem("About Game of Thrones", tabName = "overview"),
               menuSubItem("About the project", tabName = "about"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "world",
              fluidRow(
                column(width = 8,
                       box(
                         title = "Spatial repartition of the character in the world of GoT", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                         withSpinner(plotOutput(outputId = "carte", height="600px"))
                       ),
                       box(
                         title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                         withSpinner(plotOutput(outputId = "lignes", height="300px"))
                       )

                ),
                column(width = 4,
                              box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                              checkboxGroupInput(inputId = "ContinentDisplay",
                                                 label = strong("Select continent(s) :"),
                                                 choices = unique(conts$name),
                                                 selected = unique(conts$name)[1]),
                              
                              checkboxGroupInput(inputId = "RegionDisplay",
                                                 label = strong("Select properties :"),
                                                 choices = c("Islands", "Kingdoms","Roads", "Rivers", "Lakes", "Landscape", "Wall"),
                                                 selected = "Islands"),
                              
                              radioButtons(inputId = "show_location",
                                           label = "Display:",
                                           choices = c("Main locations", "Locations of interest", "None"),
                                           selected = "None"),
                              
                              selectInput(inputId = "name", 
                                          label = strong("Select character :"), 
                                          choices = unique(full_char$name), 
                                          selected = unique(full_char$name)[1]),
                              
                              # Select whether to overlay smooth trend line
                              checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                              
                              # Display only if the smoother is checked
                              conditionalPanel(condition = "input.smoother == true",
                                               sliderInput(inputId = "f", label = "Smoother span:",
                                                           min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                           animate = animationOptions(interval = 100)),
                                               HTML("Higher values give more smoothness."))
                              )
                )
              )
      ),
      tabItem(tabName = "characters",
              fluidRow(
                box(
                  title = "Select a character :", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                  selectInput(inputId = "name2", 
                              label = "", 
                              choices = unique(full_char$name), 
                              selected = unique(full_char$name)[1])
                )),
              fluidRow(
                column(4,
                       imageOutput("image")
                ),
                column(8,
                       infoBoxOutput("sex"),
                       infoBoxOutput("house"),
                       infoBoxOutput("killedby"),
                       infoBoxOutput("ScreenTime"),
                       infoBoxOutput("Buddy"),
                       infoBoxOutput("BuddyTime"),
                       infoBoxOutput("Nbscenes"),
                       infoBoxOutput("Nbepisodes"),
                       infoBoxOutput("Nblocations")
                )
              )
      ),
      tabItem(tabName = "Arryn",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("arrynbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("arrynmembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot1", height="400px"))) 
      ),
      tabItem(tabName = "Baratheon",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("baratheonbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("baratheonmembers")
                       )
                )
                
              ),
              br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                plotOutput(outputId = "barplot2", height="400px")))
      ),
      tabItem(tabName = "Bolton",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("boltonbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("boltonmembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot3", height="400px")))  
      ),
      tabItem(tabName = "Frey",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("freybanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("freymembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot4", height="400px")))  
      ),
      tabItem(tabName = "Greyjoy",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("greyjoybanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("greyjoymembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot5", height="400px")))  
      ),
      tabItem(tabName = "Lannister",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("lannisterbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("lannistermembers")
                       )
                )
                
              ),
              br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot6", height="400px")))  
      ),
      tabItem(tabName = "Martell",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("martellbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("martellmembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot7", height="400px")))
      ),
      tabItem(tabName = "Mormont",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("mormontbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("mormontmembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot8", height="400px")))  
      ),
      tabItem(tabName = "Stark",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                         imageOutput("starkbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("starkmembers")
                       )
                )
                
              ),
              
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot9", height="400px")))  
      ),
      tabItem(tabName = "Targaryen",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("targaryenbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("targaryenmembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot10", height="400px")))  
      ),
      tabItem(tabName = "Tarly",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("tarlybanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("tarlymembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot11", height="400px")))  
      ),
      tabItem(tabName = "Tully",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("tullybanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("tullymembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot12", height="400px")))  
      ),
      tabItem(tabName = "Tyrell",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("tyrellbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("tyrellmembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot13", height="400px")))  
      ),
      tabItem(tabName = "Umber",
              fluidRow(
                column(6,
                       box(title = "Banner", width = 7, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("umberbanner")
                       )
                ),
                column(6,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("umbermembers")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Screentime by season", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "barplot14", height="400px")))  
      ),
      tabItem(tabName = "Dorne",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("dornemap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("dorne")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp1", height="400px")))
      ),
      tabItem(tabName = "II",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("ironmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("iron")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp2", height="400px"))) 
      ),
      tabItem(tabName = "Crownlands",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("crownlandsmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("crown")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp3", height="400px")))
              
      ),
      
      tabItem(tabName = "North",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("northmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("north")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp4", height="400px")))
      ),
      tabItem(tabName = "Reach",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("reachmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("reach")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp5", height="400px")))
      ),
      tabItem(tabName = "Riverlands",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("riverlandsmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("riverlands")
                       )
                )
                
              ),
              br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp6", height="400px")))
      ),
      tabItem(tabName = "Stormlands",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("stormlandsmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("stormlands")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp7", height="400px")))
      ),
      tabItem(tabName = "Vale",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("valemap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("vale")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp8", height="400px")))  
      ),
      tabItem(tabName = "Westerlands",
              fluidRow(
                column(8,
                       box(title = "Map", width = 12, height= 6, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           imageOutput("westerlandsmap")
                       )
                ),
                column(4,
                       box(title = "Infos", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           tableOutput("westerlands")
                       )
                )
                
              ),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              fluidRow(box(title = "Barplot of number of scenes by region", width = NULL, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                           plotOutput(outputId = "bp9", height="400px")))  
      ),
      tabItem(tabName = "overview",
              fluidRow(
                column(6,
                       h4(p("About Game of Thrones")),
                       h5(p("Game of Thrones is roughly based on the storylines of the A Song of Ice and Fire book series by George R. R. Martin, set in the fictional Seven Kingdoms of Westeros and the continent of Essos."),
                          p("The series utilizes several simultaneous plot lines. The first story arc follows a dynastic conflict among competing claimants for succession to the Iron Throne of the Seven Kingdoms, with other noble families fighting for independence from the throne."),
                          p("The second covers the exiled scion's actions to reclaim the throne; the third chronicles the threat of the impending winter, as well as the legendary creatures and fierce peoples of the North."),
                          p("Showrunner David Benioff jokingly suggested 'The Sopranos in Middle-earth' as Game of Thrones' tagline, referring to its intrigue-filled plot and dark tone in a fantasy setting of magic and dragons."),
                       br(),
                       h4(p("About the production of the TV show")),
                       h5(p("Game of Thrones is an American fantasy drama television series produced by the HBO cable network. It is based on the novel series A Song of Ice and Fire, written by George R.R. Martin, who serves as a producer, creative consultant and scriptwriter on the television series."),
                          p("David Benioff and D.B. Weiss created the television series and serve as executive producers, showrunners and the main writers."),
                          p("The series consists of eight fully transmitted seasons, comprising seventy-three episodes in total."),
                          p("Production of the series is based in Belfast, Northern Ireland, mainly at the Paint Hall Studios. It is the largest and most expensive television production ever mounted in Northern Ireland. Filming for the series has also been conducted in Malta, Iceland, Croatia, Morocco, Spain, and the USA."),
                          p("David Benioff was sent a collection of the first four novels in the series (A Game of Thrones, A Clash of Kings, A Storm of Swords and A Feast for Crows) by George R.R. Martin's agent."),
                          p("Initially sceptical of the fantasy genre, Benioff became a big fan of the books and invited his friend D.B. Weiss to develop the project with him for a screen adaptation."),
                          p("They initially considered a movie adaption, but realized this would mean losing most of the plot and characters from the books. Instead, they began working on an adaptation for television."),
                          p("They met with George R.R. Martin and spent several hours discussing the project. Martin was impressed with their enthusiasm and that they had already worked out the resolutions to several key mysteries in the books. He agreed with them that the series was a good fit for the cable company HBO, which Martin was already a big fan of."),
                       )
                        )
                ),
                column(6,
                       h4(p("About the author")),
                       h5(p("George Raymond Richard Martin (born George Raymond Martin; September 20, 1948), also known as GRRM, is an American novelist and short story writer, screenwriter, and television producer. 
                                    He writes the series of epic fantasy novels A Song of Ice and Fire, which was adapted into the HBO series Game of Thrones (2011–2019). More info can be found ",
                            a("here.",
                              href = "https://en.wikipedia.org/wiki/George_R._R._Martin"))
                       ),
                       br(),
                       h4(p("About A Song of Ice and Fire")),
                       h5(p("A Song of Ice and Fire is an award-winning series of best-selling books of epic fantasy novels by American author and scriptwriter George R.R. Martin."),
                          p("The series currently comprises five published novels with two more anticipated to bring the series to a conclusion."),
                          p("The fifth book, A Dance with Dragons, was published on 12 July 2011. There are also three prequel novellas set in the same world. Game of Thrones is the television adaptation of the books."),
                ))
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                column(6,
                       h4(p("About the Project")),
                       h5(p("This project is a Shiny Application about Game of Thrones Universe for our course in Data Science in Master IM at Paris 5.")),
                       br(),
                       h5(p("The project began as an attempt to combine our skills in R, a programming language used primarily for analyzing and reporting data, and Shiny, a package in R for building a web-based application.")),
                       br(),
                       h5(p("It has two components. The first is this app, which queries a dataset to return information in the form of map and plots. The second is the lore of Game of Thrones, which we assembled by tying together information from the sources below. We hope you find it interesting and/or useful."))
                       
                ),
                column(6,
                       h4(p("About us")),
                       h5("This project was lead by Arakchou Ayour and Rashid Rafi, students at Université de Paris (Paris 5) for the Data Science.")
                       
                )
              ),
              br(),
              hr(),
              h5("Sources:"),
              h6(
                p("Game of Thrones datasets from ",
                  a("Etienne Côme GitHub",
                    href = "https://github.com/comeetie/got"))),
              h6(
                p("Game of Thrones informations from ",
                  a("Game of Thrones Wiki",
                    href = "https://gameofthrones.fandom.com/wiki/Game_of_Thrones_Wiki"))),
              h5("Built with",
                 img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                 "by",
                 img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                 ".")
              
      )
    )
  )
)

server <- function(input, output) {
  ## Reactive data ##
  data = reactive({
    req(input$name)
    loc_time_mc %>% filter(name %in% input$name)
  })
  data2 = reactive({
    req(input$name)
    mctime %>% filter(name %in% input$name)
  })
  continent = reactive({
    req(input$ContinentDisplay)
    conts %>% filter(name %in% input$ContinentDisplay)
  })
  mc <- reactive({
    req(input$name2)
    mainCharacters %>% filter(name %in% input$name2)
  })
  char <- reactive({
    req(input$name2)
    full_char %>% filter(name %in% input$name2)
  })
  buddy = reactive({
    req(input$name2)
    buddies %>% filter(name.x %in% input$name2) %>% head(1)
  })
  
  nbscenes = reactive({
    req(input$name2)
    df1 %>% filter(name %in% input$name2)
  })
  
  nbeps = reactive({
    req(input$name2)
    df2 %>% filter(name %in% input$name2)
  })
  
  nblocations = reactive({
    req(input$name2)
    df3 %>% filter(name %in% input$name2)
  })

  ## Plots ##
  output$carte <- renderPlot({
    ggplot()+
      geom_sf(data=continent(), fill=colland, col=borderland, size=0.1)+
      {if("Islands" %in% input$RegionDisplay) geom_sf(data=islands, fill=colland, col=borderland)} +
      {if("Kingdoms" %in% input$RegionDisplay) geom_sf(data=kingdoms, fill="grey", col="black")} +
      {if("Kingdoms" %in% input$RegionDisplay) geom_sf_label(data = kingdoms, aes(label=name),size=4,fontface="bold")} +
      {if("Roads" %in% input$RegionDisplay) geom_sf(data=roads, col="grey")} +
      {if("Rivers" %in% input$RegionDisplay) geom_sf(data=rivers,col=colriver)} +
      {if("Lakes" %in% input$RegionDisplay) geom_sf(data=lakes,col=colriver,fill=colriver)} +
      {if("Landscape" %in% input$RegionDisplay) geom_sf(data=landscapes %>% filter(type=="forest"),fill=colforest,col=colforest)} +
      {if("Wall" %in% input$RegionDisplay) geom_sf(data=wall, col="black", size=1)} +
      
      geom_sf(data = data(), aes(size=duration/60), color="#756bb1", alpha = 0.5)+
      {if(input$show_location == "Main locations") geom_sf_text(data = locations %>% filter(size>4,name!='Tolos'),aes(label=name),size=4,fontface="bold")} +
      {if(input$show_location == "Locations of interest") geom_sf_text(data = data() %>% filter(duration>=3600),aes(label=location),color="#000000",vjust="bottom",fontface="bold")} +
      
      scale_color_discrete(guide="none")+
      scale_size_area("Durée (min):", max_size = 12, breaks=c(30,60,120,240))+
      theme_void()+
      coord_sf(expand = 0, ndiscr = 0)+
      theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
            panel.background = element_rect(fill = colriver,color=NA))
  })
  output$lignes <- renderPlot({
    plot(x = data2()$episodeId, 
         y = data2()$time/60, 
         type = "l",
         xlab = "Saisons", 
         ylab = "Temps (min)",
         xaxt='n')
    axis(side = 1, at=season_e,labels = paste("Saison",1:8))
    if(input$smoother){
      smooth_curve <- lowess(x = data2()$episodeId, y = data2()$time/60, f = input$f)
      lines(smooth_curve, col = "#4292c6", lwd = 2)
    }
  })
  
  ## Characters ##
  output$image <- renderImage({
  filename <- normalizePath(file.path('./images/characters',
                                     paste(input$name2, '.jpg', sep='')))
  
  list(src = filename)
  
  }, deleteFile = FALSE)
  
  output$sex <- renderInfoBox({
    infoBox(
      "Sex", char()$sex, icon = icon("venus-mars"),
      color = "light-blue"
    )
  })
  
  output$house <- renderInfoBox({
    infoBox(
      "House", char()$house, icon = icon("home"),
      color = "light-blue"
    )
  })
  
  output$killedby <- renderInfoBox({
    infoBox(
      "Killed By", char()$killedBy, icon = icon("skull"),
      color = "light-blue"
    )
  })
  
  output$ScreenTime <- renderInfoBox({
    infoBox(
      "Total Screentime", paste(round(mc()$screenTimeTotal/3600,2),"h"), icon = icon("clock"),
      color = "light-blue"
    )
  })
  
  output$Buddy <- renderInfoBox({
    infoBox(
      "Buddy", buddy()$name.y, icon = icon("user-friends"),
      color = "light-blue"
    )
  })
  
  output$BuddyTime <- renderInfoBox({
    infoBox(
      "Screentime with buddy", paste(round(buddy()$n/3600,2),"h"), icon = icon("hourglass-end"),
      color = "light-blue"
    )
  })
  
  output$Nbscenes <- renderInfoBox({
    infoBox(
      "Number of scenes played", nbscenes()$nbscenes, icon = icon("hourglass-end"),
      color = "light-blue"
    )
  })
  
  output$Nbepisodes <- renderInfoBox({
    infoBox(
      "Number of episodes played in", nbeps()%>%nrow(), icon = icon("hourglass-end"),
      color = "light-blue"
    )
  })
  
  output$Nblocations <- renderInfoBox({
    infoBox(
      "Number of locations played in", nblocations()%>%nrow(), icon = icon("hourglass-end"),
      color = "light-blue"
    )
  })
  
  ## Houses ##
  
  ## Arryn
  
  output$arrynbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Arryn", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$arrynmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Arryn") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot1 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Arryn")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Arryn")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Baratheon
  
  output$baratheonbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Baratheon", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$baratheonmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Baratheon") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot2 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Baratheon")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Baratheon")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Bolton
  
  output$boltonbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Bolton", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$boltonmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Bolton") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot3 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Bolton")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Bolton")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Frey
  
  output$freybanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Frey", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$freymembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Frey") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot4 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Frey")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Frey")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Greyjoy
  
  output$greyjoybanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Greyjoy", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$greyjoymembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Greyjoy") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot5 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Baratheon")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Baratheon")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Lannister
  
  output$lannisterbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Lannister", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$lannistermembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Lannister") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot6 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Lannister")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Lannister")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  # Martell
  
  output$martellbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Martell", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$martellmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Martell") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot7 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Martell")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Martell")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Mormont
  
  output$mormontbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Mormont", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$mormontmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Mormont") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot8 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Mormont")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Mormont")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Stark
  
  output$starkbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Stark", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$starkmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Stark") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot9 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Stark")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Stark")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Targaryen
  
  output$targaryenbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Targaryen", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$targaryenmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Targaryen") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot10 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Targaryen")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Targaryen")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Tarly
  
  output$tarlybanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Tarly", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$tarlymembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Tarly") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot11 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Tarly")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Tarly")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Tully
  
  output$tullybanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Tully", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$tullymembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Tully") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot12 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Tully")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Tully")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Tyrell
  
  output$tyrellbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Tyrell", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$tyrellmembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Tyrell") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot13 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Tyrell")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Tyrell")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Umber
  
  output$umberbanner <- renderImage({
    filename <- normalizePath(file.path('./images/houses',
                                        paste("Umber", '.png', sep='')))
    
    list(src = filename)
    
  }, deleteFile = FALSE)
  
  output$umbermembers <- renderTable({
    characters %>% left_join(screenTimeTotal) %>% filter(house=="Umber") %>% mutate(Members=name) %>% mutate(screenTime=paste(round(screenTimeTotal/3600,2),"h")) %>% select(Members,sex,killedBy,screenTime)
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$barplot14 <- renderPlot({
    df = characters %>% left_join(screenTimePerSeasons) %>% filter(house=="Umber")
    df2 = characters %>% left_join(screenTimeTotal) %>% filter(house=="Umber")
    ggplot(df)+
      geom_bar(aes(y=name,x=screenTime/60,fill=factor(seasonNum,level=8:1)),stat="identity")+
      scale_fill_brewer("Saison",palette = "Spectral")+theme_bw()+
      geom_text(data=df2,aes(y=name,x=screenTimeTotal/60+5,label=paste(round(screenTimeTotal/60),'min')),hjust = "left")+
      scale_x_continuous("Temps d'apparition (min)",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Kingdoms ##
  
  ## Dorne
  
  output$dornemap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("Dorne", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$dorne <- renderTable({
    locationinfo %>% filter(location=="Dorne")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp1 <- renderPlot({
    df = locationinfo %>% filter(location=="Dorne")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Iron Islands
  
  output$ironmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("Iron Islands", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$iron <- renderTable({
    locationinfo %>% filter(location=="The Iron Islands")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp2 <- renderPlot({
    df = locationinfo %>% filter(location=="The Iron Islands")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## Crownlands
  
  output$crownlandsmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("The Crownlands", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$crown <- renderTable({
    locationinfo %>% filter(location=="The Crownlands")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp3 <- renderPlot({
    df = locationinfo %>% filter(location=="The Crownlands")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## North

  output$northmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("The North", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$north <- renderTable({
    locationinfo %>% filter(location=="The North")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp4 <- renderPlot({
    df = locationinfo %>% filter(location=="The North")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })
  
  ## The Reach

  output$reachmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("The Reach", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$reach <- renderTable({
    locationinfo %>% filter(location=="The Reach")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp5 <- renderPlot({
    df = locationinfo %>% filter(location=="The Reach")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })

  ## The Riverlands
  
  output$riverlandsmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("The Riverlands", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$riverlands <- renderTable({
    locationinfo %>% filter(location=="The Riverlands")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp6 <- renderPlot({
    df = locationinfo %>% filter(location=="The Riverlands")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })

  ## Stormlands
  
  output$stormlandsmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("The Stormlands", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$stormlands <- renderTable({
    locationinfo %>% filter(location=="The Stormlands")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp7 <- renderPlot({
    df = locationinfo %>% filter(location=="The Stormlands")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })

  ## The Vale
  output$valemap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("Vale of Arryn", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$vale <- renderTable({
    locationinfo %>% filter(location=="The Vale")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp8 <- renderPlot({
    df = locationinfo %>% filter(location=="The Vale")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })

  ## Westerlands
  output$westerlandsmap <- renderImage({
    filename <- normalizePath(file.path('./images/kingdoms',
                                        paste("The Westerlands", '.png', sep='')))

    list(src = filename)

  }, deleteFile = FALSE)
  
  output$westerlands <- renderTable({
    locationinfo %>% filter(location=="The Westerlands")
  }, bordered = TRUE, hover = TRUE, spacing = "l")
  
  output$bp9 <- renderPlot({
    df = locationinfo %>% filter(location=="The Westerlands")
    ggplot(df)+
      geom_bar(aes(y=subLocation,x=nbscenes,fill=factor(nbdeaths)),stat="identity")+
      scale_fill_brewer("Deaths",palette = "Spectral")+theme_bw()+
      geom_text(data=df,aes(y=subLocation,x=nbscenes,label=paste(round(nbscenes),'scenes')),hjust = "left")+
      scale_x_continuous("Nombre de scènes",breaks = seq(0,750,by=120),limits = c(0,780),expand = c(0,1))+
      ylab("")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)