library(shiny)
library(leaflet)

ui<-fluidPage(title="IMD Climate", theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", 
              tags$head(HTML('<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')),
  fluidRow(HTML("<div  style='background-color: black; height: 80px '> <img src='ah_small_black.gif'> <h2 style='color: white; display: inline-block; margin-left: 20px'> IMClimateR Visualizer </h2> </div>")),
  fluidRow(
    column(1,
      textInput(inputId="ParkCode", placeholder="ParkCode", label="Enter a Park Code"),
      sliderInput(inputId="BoxBuffer", label="Size of Buffer around Park (km)", min=0, max=25,step=1,value=10),
      actionButton(inputId="BoundButton", label="Go", class="btn btn-primary")
    ),
  
    column(8,style="padding: 0",
      leafletOutput("ClimateMap",width = "100%", height = "1000px")
    ),
    column(3,
    #textOutput("Test")
      uiOutput("Header"),
      br(),
      hr(),
      DT::dataTableOutput("WeatherTable")
    )
  )
)
