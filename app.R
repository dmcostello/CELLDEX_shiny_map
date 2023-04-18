#### --- GLOBAL --- ####

#Adapted from SuperZip example https://shiny.rstudio.com/gallery/superzip-example.html
#https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example

library(shiny)
library(leaflet)
library(leafem)
library(dplyr)
library(gbm)

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
                                           )
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

server <- function(input, output) {
  
  
  output$map <- renderLeaflet({
    leaflet(Csites) %>% 
      addCircles(lng = ~longitude, lat = ~latitude) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
                       color = "#1b9e77",
                       radius = 3, popup = ~as.character(cntnt),
                       stroke = FALSE, fillOpacity = 0.3) %>%
      addGeoRaster(skd,autozoom=F,
                   colorOptions = colorOptions(palette="YlGn"),resolution = 2^8) %>% 
      addLegend("bottomright", pal = pal, values = values(skd),
                title = "k (1/d)",opacity = 1) %>%
    setView(lng = -81.36, lat = 41.15, zoom = 6)
  })
  
  output$test1 <- renderPlot({summary(fgbm,n.trees=best.iter2)
  })

}

# Run the application 
shinyApp(ui = ui, server = server,options=list(display.mode = "showcase"))

#runGitHub( "CELLDEX_shiny_map", "dmcostello")
