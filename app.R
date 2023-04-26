#### --- GLOBAL --- ####

#Adapted from SuperZip example https://shiny.rstudio.com/gallery/superzip-example.html
#https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example

library(shiny)
library(leaflet)
library(leafem)
library(dplyr)
library(gbm)
library(raster)

#Read in data
Csites <- readRDS('./data/CELLDEX.rds')
  #Creates the html strings for map click
Csites <- mutate(Csites, cntnt=paste0('<strong>Site code: </strong>',part.str,
                                      '<br><strong>Biome:</strong> ', biome_short.y,
                                      '<br><strong>Decay rate (1/d):</strong> ', round(k,digits=3))) 
skd<-readRDS('./data/skd.rds')
ln_skd <- readRDS('./data/ln_skd.rds')

#Color pallette
pal <- colorNumeric(
  palette = "YlGn",
  domain = values(skd)
)

pal_ln <- colorNumeric(
  palette = "YlGn",
  domain = values(ln_skd),
  
  reverse=T
)

#Load models
load("FSmodel.rda")  

best.iter2 <- gbm.perf(fgbm,method="cv")

#### --- USER INTERFACE --- ####

ui <- navbarPage("CELLDEX",id="nav",
                 tabPanel("Interactive map",
                          sidebarLayout(
                            mainPanel("",
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
                                           selected = "Cotton",
                                           ),
                              br(),
                              h4("Map center:"),
                              numericInput("lat_in",label = "Latitude",min=-90,max=90,value=41.15),
                              numericInput("lng_in",label = "Longitude",min=-180,max=180,value=-81.36),
                              br(),
                              h4("You clicked:"),
                              textOutput("click_lat"),
                              textOutput("click_lng"),
                              textOutput("kd")
                            )
                          )
          ),
                      
  tabPanel("Environmental data",
           h1("Sliders here for environmental data"),
           plotOutput("test1",width = 300,height=400)),
  tabPanel("Substrate data",
           h1("Sliders here for leaf traits"))
  )
                 

#### --- SERVER LOGIC --- ####

server <- function(input, output, session) {
    
  
  output$map <- renderLeaflet({
    leaflet(Csites) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      #addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
      #                 color = "#1b9e77",
      #                 radius = 3, popup = ~as.character(cntnt),
      #                 stroke = FALSE, fillOpacity = 0.8) %>%
      addGeoRaster(skd,autozoom=F,layerId = 'rkd',
                   colorOptions = colorOptions(palette="YlGn"),resolution = 2^8) %>% 
      addLegend("bottomright", pal = pal, values = values(skd),
                title = "k (1/d)",opacity = 1) %>%
    setView(lng = input$lng_in, lat = input$lat_in, zoom = 6)
  })
  
  observe({proxy <- leafletProxy("map")
  if(input$sites=="Cotton")
  {proxy %>% addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
                              color = "#1b9e77",
                              radius = 3, popup = ~as.character(cntnt),
                              stroke = FALSE, fillOpacity = 0.8)
  }
  if(input$sites=="None")
  {proxy %>% clearMarkers()}
  
  })
  
  output$test1 <- renderPlot({summary(fgbm,n.trees=best.iter2)
  })
  

  output$click_lat <- renderText({paste("Latitude: ",
                                        round(input$map_click$lat,digits=3))})
  output$click_lng <- renderText({paste("Longitude: ",
                                        round(input$map_click$lng,digits=3))})
  
  #updateNumericInput(inputID="lat_in",value=input$map_click$lat)
  
output$kd <- renderText({paste("Predicted cotton kd = ",
                   round(
                     raster::extract(x=skd,
                                     y=data.frame(long=input$map_click$lng,lat=input$map_click$lat))
                   ,digits=3)
)
})

}

# Run the application 
shinyApp(ui = ui, server = server,options=list(display.mode = "showcase"))

#runGitHub( "CELLDEX_shiny_map", "dmcostello")
