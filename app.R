#### --- GLOBAL --- ####

#Adapted from SuperZip example https://shiny.rstudio.com/gallery/superzip-example.html
#https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example

library(shiny)
library(leaflet)
Csites <- readRDS('./data/CELLDEX.rds')

#### --- USER INTERFACE --- ####

ui <- navbarPage("CELLDEX",id="nav",
                 tabPanel("Interactive map",
                          sidebarLayout(
                            mainPanel("map",
                            leafletOutput("map",width=600, height=800)),
                            sidebarPanel(
                              h2("Stream decomp"),
                              
                              radioButtons("type",label="Show decomposition rate of",
                                           choices = list("Cotton","Leaf litter")),
                              conditionalPanel("input.type == 'Leaf litter'",
                                               # Only prompt for litter type when selecting litter
                                               selectInput("leaf",label="Leaf genus",choices=list(
                                                 "Oak","Maple"))),
                              radioButtons("sites",label="Show sites",
                                           choices=list("None","Cotton","Leaf litter","Both"),
                                           selected = "Cotton")
                            )
                          )
          ),
                      
  tabPanel("Environmental data",
           h1("Sliders here for environmental data")),
  tabPanel("Substrate data",
           h1("Sliders here for leaf traits"))
  )
                 

#### --- SERVER LOGIC --- ####

server <- function(input, output) {
  
  # Draw the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -81.36, lat = 41.15, zoom = 7)
  })
  
  # A reactive expression that returns the set of sites that are in bounds
  # Was throwing errors so turned it off. App still runs fast without
  #sitesInBounds <- reactive({
    #if (is.null(input$map_bounds))
      #return(Csites[FALSE,])
    #bounds <- input$map_bounds
    #latRng <- range(bounds$north, bounds$south)
    #lngRng <- range(bounds$east, bounds$west)
    
    #subset(Csites,
           #latitude >= latRng[1] & latitude <= latRng[2] &
             #longitude >= lngRng[1] & longitude <= lngRng[2])
  #})
  
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
      addCircleMarkers(~longitude, ~latitude, layerId=~part.str,radius=radset,color=fillset)
  })    
    
    # Generate a popup at the given location
    showCELLDEX <- function(part.str, lat, lng) {
      selectsite <- Csites[Csites$part.str == part.str,]
      content <- "Works"
        #as.character(tagList(
        #tags$strong(HTML(sprintf("%s %s",
        #                         selectsite$part.str, selectsite$biome_short.x
        #))), tags$br(),
        #sprintf("Decay rate (/d): %s", selectsite$k), tags$br(),
      #))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = part.str)
    }
    
    # When map is clicked, show a popup with site info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showCELLDEX(event$id, event$lat, event$lng)
      })
    })
    

  
}

# Run the application 
shinyApp(ui = ui, server = server,options=list(display.mode = "showcase"))

#runGitHub( "CELLDEX_shiny_map", "dmcostello")