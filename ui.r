
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library (leaflet)
library(shiny)
library(ggplot2)
ui <-shinyUI(pageWithSidebar(
  
  headerPanel(h1( a( "US election choropleth map generator", style = "font-weight: 200; line-height: 1.1; color: #000000;")),
  ),
  
  
  sidebarPanel(
    width = 2,
    
    selectInput("chooseColor", "Choose color scheme:", selected = "RdPu", c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples"
                                                         , "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", "Greys", "Greens"
                                                         , "GnBu", "BuPu", "BuGn", "Blues")),
    
    
    selectInput("chooseStates","Choose geography", selected = "United States", c("United States", "Alabama","Arkansas","Arizona","Connecticut"
                                                      ,"Colorado","California","Delaware","DC", "Florida","Georgia","Hawaii","Idaho","Illinois", "Indiana","Iowa"
                                                      ,"Kansas","Kentucky","Louisiana","Maine","Maryland", "Massachusetts", "Michigan", "Minnesota"
                                                      , "Mississippi","Missouri","Montana","Nebraska", "Nevada", "New Hampshire", "New Jersey"
                                                      , "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio","Oklahoma","Oregon","Pennsylvania"
                                                      , "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas" ,"Utah", "Vermont", "Virginia"
                                                      , "Washington", "West Virginia", "Wisconsin", "Wyoming")),

    selectizeInput("whatData","Choose variable to map", list(
                                                    
                                                    "Election Results" = c(
                                                      "2016 Presidential election winner" 
                                                      ,"2012 Presidential election winner"
                                                      ,"2008 Presidential election winner"
                                                      ,"Trump captures"
                                                    ),
                                                      
                                                    Demographics = c(
                                                      "% of population White"
                                                      ,"% of population Black"
                                                      , "% of population Asian"
                                                      , "% of population Native American"
                                                      , "% of population Multiracial"  
                                                      , "% of population Hispanic"  
                                                      , "% of population Male"
                                                      , "Median age"
                                                    ),
                                                    
                                                    "Population Characteristics" = c(
                                                       "Population"
                                                      , "Population density (persons per square mile)"
                                                      , "% Increase in population (2011-2015)"
                                                    ),
                                                    
                                                    "Education, Health, Economy" = c(
                                                       "Bachelor's degree attainment"
                                                      , "Drug overdose rate (age-adjusted)"
                                                      , "Unemployment rate"
                                                      , "Labor force participation rate"
                                                      , "Median household income"
                                                    )
                                                    
                                                    
                                                      # ,"Drug overdose death rate"
                                                      # ,"2016ResultsCongress"
                                                      # , "PVI"
                                                      # ,"RomTrump"
                                                      # ,"blackPop"
                                                      # ,"PopDensity"
                                                    
                                                    
                                                    ),
                   multiple = F
                ),
    
    selectInput("whatFormat","Choose format to download map", c(".png",".pdf",".html")),
    
    checkboxInput("borderYes", "Borders"),
    checkboxInput("labelYes", "Labels"),
    checkboxInput("legendYes", "Legend"),
    downloadButton("downloadMap","download Map"),
    downloadButton("downloadOutput","download csv")
    ),
  
  
  mainPanel(
  
    tabsetPanel(id="tabs",
  
      tabPanel("Instructions", value="int", 
               p(textOutput("text1") 
               ,textOutput("text2")),
               
               p(textOutput("text3"))
               ,textOutput("text4")),
      
      # tabPanel("Graph", value="graph", plotOutput("graphTwo")),
      
      tabPanel("County-level Map", value="state", leafletOutput("countyMap", height = 800, width = 1500)),
      # tabPanel("District-level Map", value="cong", leafletOutput("CongMap")),
      
      # tabPanel("Plot with Drug Rates", value="drugsM",plotOutput("graphThree")),
      tabPanel("Documentation", value="documentation"),
      tabPanel("About Authors", value="about",textOutput("text6"),h1(a(img(src = "ben.jpg", align = "left", width = "50%",length="50%"))),textOutput("text7"))
      
   )

)


))
