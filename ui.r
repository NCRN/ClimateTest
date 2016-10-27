library(shiny)
library(leaflet)

ui<-fluidPage(title="IMD Climate Test", theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", 
  fluidRow(column(2,
    textInput(inputId="ParkCode", placeholder="ParkCode", label="Enter a Park Code"),
    sliderInput(inputId="BoxBuffer", label="Size of Buffer around Park (km)", min=0, max=25,step=1,value=10),
    actionButton(inputId="BoundButton", label="Go", class="btn btn-primary")
  ),
  
  column(10,style="padding: 0",
    leafletOutput("ClimateMap",width = "100%", height = "500px")
  )),
  
  fluidRow(
    column(12,
    #textOutput("Test")
    uiOutput("Header"),
    br(),
    DT::dataTableOutput("WeatherTable")
  ))
)
