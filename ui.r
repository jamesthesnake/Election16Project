
library(dplyr) 
library(rgdal)
library(htmlwidgets)
library(tools)
library(colourpicker)
library (leaflet)
library(shiny)
library(ggplot2)
library(shinyURL)



ui <-shinyUI(pageWithSidebar(
  

  headerPanel(h1( a( "US election choropleth map generator", style = "font-weight: 200; line-height: 1.1; color: #000000;")),
  ),

  
  sidebarPanel(
    width = 2,
    # colourInput("col","select first color","red"),
    # colourInput("col2","select second color","blue"),
    selectInput("chooseColor", "Choose color scheme", selected = "RdPu", c("YlOrRd", "YlOrBr", "YlGnBu", "YlGn", "Reds", "RdPu", "Purples"
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
                                                       "Population (log scale)"
                                                      , "Population density (persons per square mile, log scale)"
                                                      , "% Increase in population (2011-2015)"
                                                    ),
                                                    
                                                    "Education, Health, Economy" = c(
                                                       "Bachelor's degree attainment (%)"
                                                      , "Drug overdose rate (age-adjusted, deaths per 100,000 people)"
                                                      , "Unemployment rate (%)"
                                                      , "Labor force participation rate (%)"
                                                      , "Median household income (USD)"
                                                    )
                                                      
                                                    
                                                    ),
                   multiple = F
                ),
    
    selectInput("whatFormat","Choose format to download map", c(".png",".pdf")),
    
    checkboxInput("borderYes", "Borders"),
    checkboxInput("labelYes", "Labels"),
    checkboxInput("legendYes", "Legend"),
    downloadButton("downloadMap","Download Map"),
    
    # downloadButton("downloadOutput","download csv"),
    shinyURL.ui()
    ),
  
  
  mainPanel(
  
    tabsetPanel(id="tabs",
  
      tabPanel("Instructions", value="int", 
               br(),
               p("Generate maps of the United States by county for a collection of variables."),
               p("Select a variable in the sidebar, specify options, and view output in the Map tab."),
               p("By James Hennessy and Ben Berger")
               ),
      
      # tabPanel("Graph", value="graph", plotOutput("graphTwo")),
      
      tabPanel("County-level Map", value="state", leafletOutput("countyMap", height = 800, width = 1500)
               #,leafletOutput("CongMap", height=800,width=1500)
               ),
      # tabPanel("District-level Map", value="cong", leafletOutput("CongMap")),
      
      # tabPanel("Plot with Drug Rates", value="drugsM",plotOutput("graphThree")),
      tabPanel("Documentation", value="documentation",
              
               
              h2("Election results: Tony McGovern, The Guardian, Townhall.com", style = "font-size: 150%") ,
                                a("https://github.com/tonmcg/County_Level_Election_Results_12-16"),
                                p("Election winner is either Democratic Party or GOP, ie. the 2-party election winner. Trump Captures are counties 
                                  that the GOP lost in 2012, but captured in 2016."),
               
              h2("Demographic data: US 2010 Census", style = "font-size: 150%"),
                                a("https://www.census.gov/2010census/data/"),
                                p("Percent of population white includes only white non-hispanic."),
               
              h2("Median age: US American Community Survey", style = "font-size: 150%"),
                                a("https://www.census.gov/programs-surveys/acs/"),
               
              h2("Population data: US 2010 Census & Gazetteer file", style = "font-size: 150%"), 
                                a("https://www.census.gov/2010census/data/"), 
                                a("http://www.census.gov/geo/maps-data/data/gazetteer.html"),
                                p("Population and population density are given as log-transformed variables. For example,
                                  a county with population value of 4 has an actual population of 10^4 = 10000."),
               
              h2("Age-adjusted drug poisoning mortality: CDC", style = "font-size: 150%"),
                                a("https://blogs.cdc.gov/nchs-data-visualization/drug-poisoning-mortality/"),
               
              h2("Unemployment Rate, Labor Force Participation, Median Household Income: Bureau of Labor Statistics Current Population Survey",
                          style = "font-size: 150%"),
                                a("https://www.bls.gov/cps/data.htm"),
                                p("Labor Force Participation Rate is calculated with 2015 projected population from the 2010 Census file. Notably,
                                  a few counties have greater than 100% labor force participation due to this measure significantly underestimating population growth.")
               
           
               )

   )

)


))
