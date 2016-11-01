library(shiny)
library(leaflet)
library(IMClimateR)
library(DT)



server<-function(input, output, session){
  
#### intialize map ####
  output$ClimateMap<-renderLeaflet({
    leaflet() %>% 
      setView(lng=-115, lat=50, zoom=4)
  })

  NPSAttrib<-HTML("&copy; <a href='http://mapbox.com/about/maps' target='_blank'>Mapbox</a> 
          &copy; <a href='http://openstreetmap.org/copyright' target='_blank'>OpenStreetMap</a> contributors | 
                  <a class='improve-park-tiles' href='http://www.nps.gov/npmap/park-tiles/improve/' 
                  target='_blank'>Improve Park Tiles</a>")
  
  
  observe({#req(input$ParkCode)
    leafletProxy("ClimateMap") %>%

      clearTiles() %>%

      addTiles(group="Map", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.397cfb9a,nps.3cf3d4ab,nps.b0add3e6/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib) %>%
      addTiles(group="Imagery",urlTemplate="//{s}.tiles.mapbox.com/v4/nps.2c589204,nps.25abf75b,nps.7531d30a/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q",attribution=NPSAttrib) %>%
      addTiles(group="Slate", urlTemplate="//{s}.tiles.mapbox.com/v4/nps.9e521899,nps.17f575d9,nps.e091bdaf/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibnBzIiwiYSI6IkdfeS1OY1UifQ.K8Qn5ojTw4RV1GwBlsci-Q", attribution=NPSAttrib) %>%
      addLayersControl(map=., baseGroups=c("Map","Imagery","Slate"), options=layersControlOptions(collapsed=T))
  })
  
  #### Get bouding box for map ####
  ParkBounds<-eventReactive(input$BoundButton,
    getBBox(input$ParkCode, expandBBox=input$BoxBuffer*.011) %>%
    strsplit(", ", fixed=T) %>% 
     `[[`(1) %>% 
    as.numeric()
  )
  
  #### Get relevant stations ####
  ParkStations<- eventReactive(input$BoundButton,
        tryCatch(findStation(unitCode=input$ParkCode, distance=input$BoxBuffer), error=function(e) NA)
  )
  
  #### Get Weather Data from selected station ####
  WeatherData<-eventReactive(input$DataButton,
        getDailyWxObservations(climateStations = SelectedStation()$uid, climateParameters = input$DataType  )
  )
  
  
  #### Zoom the Map ####
   observe({
     req(ParkBounds())
     leafletProxy("ClimateMap") %>%  
       fitBounds(ParkBounds()[1], ParkBounds()[2], ParkBounds()[3], ParkBounds()[4])
   })

#### Add the bounding box to map ####
   observe({
      req(ParkBounds())
      leafletProxy("ClimateMap") %>%  
        clearGroup("BoundingBox") %>% 
        addRectangles(lng1=ParkBounds()[1], lat1=ParkBounds()[2], lng2=ParkBounds()[3], lat2=ParkBounds()[4], fillColor = "transparent",
                    group="BoundingBox")
   })

  #### Add the Stations to the map ####
  observe({
    req(ParkStations())
    leafletProxy("ClimateMap") %>% 
      clearGroup("Stations") %>% 
    addMarkers(lng=ParkStations()$longitude, lat=ParkStations()$latitude, group="Stations",layerId=ParkStations()$uid,
               clusterOptions=markerClusterOptions())
  })
   
 SelectedStation<-eventReactive(input$ClimateMap_marker_click,
              ParkStations()[ParkStations()$uid==input$ClimateMap_marker_click$id,]
 )

   observeEvent(input$ClimateMap_marker_mouseover,
                leafletProxy("ClimateMap") %>% 
                clearPopups() %>% 
                addPopups(lng=input$ClimateMap_marker_mouseover$lng, lat=input$ClimateMap_marker_mouseover$lat+.004,
                          popup=ParkStations()[ParkStations()$uid==input$ClimateMap_marker_mouseover$id,c('name')])
                  )
   
   observeEvent(input$ClimateMap_marker_mouseout,
                leafletProxy("ClimateMap") %>% 
                  clearPopups()
   )

   output$Header<-renderUI({
     tagList(
     h4(SelectedStation()$name),
     p(paste("Unique Id: ", SelectedStation()$uid)),
     p(paste("Station type:"), SelectedStation()$sid1_type),
     selectizeInput(inputId="DataType", label="Choose the type of data you want",
      choices=list("Maximum Temperature"="maxt", "Minimum Temperature"="mint","Average Temperature"="avgt", 
        "Obs Time Temperature"="obst", "Precipitation"="pcpn", "Snow"="snow", "Snow Depth"="snwd")),
     actionButton(inputId="DataButton", label="Get Data", class="btn btn-primary")
     )
   })
   
   
output$WeatherTable<-DT::renderDataTable(
   expr= datatable(WeatherData(),extensions="Buttons",
     options=list(columnDefs=list(list(visible=FALSE, targets=c(1,3:12,15,16))), dom="Bfrtip", 
                  buttons=c("copy","csv","excel","pdf","print"))
             ), server=FALSE
)
   
   
 # output$Test<-renderText("test")
  
  
}