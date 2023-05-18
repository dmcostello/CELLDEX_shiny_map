#### --- GLOBAL --- ####


library(shiny)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(dplyr)
library(gbm)
library(raster)
library(stars)

#Read in CELLDEX data
Csites <- readRDS('./data/CELLDEX.rds')
  #Creates the html strings for map click
Csites <- mutate(Csites, cntnt=paste0('<strong>Site code: </strong>',part.str,
                                      '<br><strong>Biome:</strong> ', biome_short.y,
                                      '<br><strong>Decay rate (1/d):</strong> ', round(k,digits=3))) 
skd<-readRDS('./data/skd.rds')
ln_skd <- readRDS('./data/ln_skd.rds')

#Read in the Follstad-Shah 2017 data
FSsites <- readRDS("./data/FSdat.rds")
FSsites <- mutate(FSsites, cntnt=paste0('<strong>Genus: </strong>',Genus,
                                      '<br><strong>Leaf condition:</strong> ', Leaf.condition,
                                      '<br><strong>Decay rate (1/d):</strong> ', round(kd,digits=3))) 

#Read in TRY trait data
traits <- readRDS("./data/traits.rds")

#Color pallette
pal <- colorNumeric(
  palette = "YlGn",
  domain = values(skd),na.color = NA
)

#Load models
load("FSmodel.rda")  

best.iter2 <- gbm.perf(fgbm,method="cv")

#### --- USER INTERFACE --- ####

ui <- navbarPage("CELLDEX",id="nav",
                 tabPanel("Interactive map",
                          fluidPage(title="Click the map to predict decomp",
                            leafletOutput("map",height = 500)),
                            hr(),
                          fluidRow(
                              column(3,
                              h4("Fly to"),
                              numericInput("lat_in",label = "Latitude",min=-90,max=90,value=41.15,),
                              numericInput("lng_in",label = "Longitude",min=-180,max=180,value=-81.36)
                              ),
                              
                              column(6,
                              radioButtons("predk",label="Predict point decomposition rate for",
                                           choices = list("Cotton","Leaf litter"),inline=T),
                              conditionalPanel("input.predk == 'Leaf litter'",
                                               # Only prompt for litter type when selecting litter
                                               column(4,
                                                 selectInput("leaf",label="Leaf genus",choices=as.list(
                                                 traits$Genus))),
                                               column(4,radioButtons("cond",label="Leaf condition",
                                                                     choices=list("senesced","green"))),
                                               column(4,radioButtons("mesh",label="Mesh size",
                                                                     choices=list("coarse","fine"))),
                              )
                              ),
                              column(3,
                                     h4("You clicked:"),
                              textOutput("click_lat"),
                              textOutput("click_lng"),
                              textOutput("click_k")
                            )
                          )
),

                      
  tabPanel("Shape output",
           h1("Create a shape on the map to predict decomp"),
           plotOutput("test1",width = 400,height=400),
           dataTableOutput("shape_summary")),
  tabPanel("Substrate data",
           h1("Sliders here for leaf traits"),
           dataTableOutput("leaftrait"))
  )
                 

#### --- SERVER LOGIC --- ####

server <- function(input, output, session) {
    
  #Fixed map
  output$map <- renderLeaflet({
    leaflet(Csites) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addGeoRaster(skd,autozoom=F,layerId = 'rkd',
                   colorOptions = colorOptions(palette="YlGn"),opacity = 0.65) %>% 
      addLegend("bottomright", pal = pal, values = values(skd),
                title = "Cotton k (1/d)",opacity = 0.65) %>%
    setView(lng = input$lng_in, lat = input$lat_in, zoom = 6) %>% 
      setMaxBounds(~-180, ~-75, ~180, ~75) %>%
    
    addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
                     color = "#1b9e77",radius = 3, 
                     popup = ~as.character(cntnt),stroke = FALSE, fillOpacity = 0.8,
                     group="Cotton (green)") %>%
    
    addCircleMarkers(data = FSsites, lat =  ~Latitude.2, lng =~Longitude.2,
                     color = "firebrick",radius = 3, 
                     popup = ~as.character(cntnt),stroke = FALSE, fillOpacity = 0.8,
                     group="Litter (red)") %>%
    
      addDrawToolbar(targetGroup = "select area",
                     position = "topleft",polylineOptions = F,markerOptions = F,
                     circleMarkerOptions = F,singleFeature = T,
                     editOptions = editToolbarOptions(
                       selectedPathOptions = selectedPathOptions())) %>%
      
      addLayersControl(
      overlayGroups=c("Cotton (green)","Litter (red)","select area"),
      options = layersControlOptions(collapsed=F))
    
  })
  
  user_shape <- reactive(eval(input$map_draw_new_feature$properties$feature_type[[1]]))
  user_coord <- reactive(matrix(unlist(input$map_draw_new_feature$geometry$coordinates[[1]]),byrow = T,ncol=2))
  user_shape_kd <- reactive(raster::extract(x=skd,y=spPolygons(user_coord())))
  
  #Old observers to check shape output
  #observeEvent(input$map_draw_new_feature,{print(user_shape())})
  #observeEvent(input$map_draw_new_feature,{print(user_coord())})


  
  
  #Practice plot from model
  output$test1 <- renderPlot({
    req(input$map_draw_new_feature)
    polykd_den <- density(unlist(user_shape_kd()),na.rm=T)
    with(polykd_den,plot(x,y,type="l",las=1,col="green3",lwd=2,
                         ylab="Relative frequency",xlab="Decomp. rate (1/d)"))
    
    #hist(unlist(user_shape_kd()),xlab="Kd (1/d)",
         #main=paste0("Cotton (cells = ",length(unlist(user_shape_kd())),")"),las=1)
  })
  
  #output$test1 <- renderPlot({summary(fgbm,n.trees=best.iter2)})
  
    #OUTPUT FROM RASTERS
  
  output$click_lat <- renderText({paste("Latitude: ",ifelse(is.null(input$map_click$lat),"N/A",
                                        round(input$map_click$lat,digits=3)))})
  output$click_lng <- renderText({paste("Longitude: ",ifelse(is.null(input$map_click$lat),"N/A",
                                        round(input$map_click$lng,digits=3)))})
  
  #updateNumericInput(inputID="lat_in",value=input$map_click$lat)
  
  output$click_k <- renderText({
    req(input$map_click)
    if(input$predk=="Leaf litter")
      {if(is.nan(raster::extract(x=skd,y=data.frame(long=input$map_click$lng,lat=input$map_click$lat)))){"NA"} else
      
      paste("Predicted",input$leaf,"k (1/d) =",round(digits=3,
      exp(predict(fgbm, n.trees=best.iter2,
              newdata=cbind(traits[traits$Genus==input$leaf,],
                            mesh.size.category=factor(input$mesh),
                            Leaf.condition=factor(input$cond),
                            ln_pred_k=log(raster::extract(x=skd,y=data.frame(long=input$map_click$lng,lat=input$map_click$lat))))
             )))
      )
    } else
    if(input$predk=="Cotton"){
      paste("Predicted cotton k (1/d) = ",
            ifelse(is.null(input$map_click$lat),"N/A",round(digits=3,
          raster::extract(x=skd,y=data.frame(long=input$map_click$lng,lat=input$map_click$lat))
            ))
      )
    }   
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

#options=list(display.mode = "showcase")

#runGitHub( "CELLDEX_shiny_map", "dmcostello")
