#### --- GLOBAL --- ####

library(shiny)
library(leaflet)
Csites <- readRDS('./data/field.rds')
Csites$part.stream <- with(Csites, paste(partnerid,stream))

#### --- USER INTERFACE --- ####

ui <- navbarPage("CELLDEX",id="nav",
                 tabPanel("Interactive map",
          
                #Generate map                
  leafletOutput("map"),
                          
          
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
              h2("Stream decomp"),
              
              radioButtons("type",label="Show decomposition rate of",
                           choices = list("Cotton","Leaf litter")),
              conditionalPanel("input.type == 'Leaf litter'",
                               # Only prompt for litter type when selecting litter
                               selectInput("leaf",label="Leaf genus",choices=list(
                                 "Oak","Maple"))),
              radioButtons("sites",label="Show sites",
                           choices=list("None","Cotton","Leaf litter","Both"))
              
    ) 
                 ),
  conditionalPanel("false")
  )
                 


#### --- SERVER LOGIC --- ####

server <- function(input, output) {
  
  # Draw the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of sites that are in bounds
  sitesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(Csites[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(Csites,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  observe({
    siteBy <- input$sites
    
    if (siteBy == "Cotton"){
      radset <-  5
      fillset <- "navy"}
    else{radset <- 0
    fillset <- "transparent"
    }
    
    leafletProxy("map", data = Csites) %>%
      clearShapes() %>%
      addCircleMarkers(~longitude, ~latitude, layerId=~part.stream,radius=radset,color=fillset)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
