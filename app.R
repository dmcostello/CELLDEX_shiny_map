#### --- GLOBAL --- ####

library(shiny)
library(shinyWidgets)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(dplyr)
library(gbm)
library(raster)
library(stars)

# Read in CELLDEX data
Csites <- readRDS('./data/CELLDEX.rds')
  #Creates the html strings for map click
Csites <- mutate(Csites, cntnt=paste0('<strong>CELLDEX </strong>',
                                      '<br><strong>Biome:</strong> ', biome_short,
                                      '<br><strong>Decay rate (1/d):</strong> ', round(k,digits=3))) 
skd<-readRDS('./data/skd.rds')

#Read in the LeRoy 2020 dataset
LITsites <- readRDS("./data/litter.rds")
LITsites <- mutate(LITsites, cntnt=paste0('<strong>Genus: </strong>',Genus,
                                      '<br><strong>Leaf condition:</strong> ', Leaf.condition,
                                      '<br><strong>Mesh:</strong> ', Mesh.size,
                                      '<br><strong>Mean decomp rate (1/d):</strong> ', round(mean_kd,digits=3))) 

#Read in TRY trait data
traits <- readRDS("./data/traits.rds")

#Color pallette
pal <- colorNumeric(
  palette = "YlGn",
  domain = values(skd),na.color = NA
)

#Load models
load("litter_mod.rda")  

best.iter2 <- 49948 #From CELLDEX geospatial

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
           h2("Create a shape on the map to predict decomp for an area"),
           fluidPage(fluidRow(
             column(5,
                    
                    h4("Select 1 litter type"),
                    radioGroupButtons(
                      inputId = "lit_select",
                      label = "",
                      choices = as.list(c("None",traits$Genus)),
                      individual = TRUE,
                      checkIcon = list(
                        yes = tags$i(class = "fa fa-circle", 
                                     style = "color: steelblue"),
                        no = tags$i(class = "fa fa-circle-o", 
                                    style = "color: steelblue"))
                    ),
                    
                    br(),
                    column(6,radioButtons("cond_shape",label="Leaf condition",
                                          choices=list("senesced","green"))),
                    column(6,radioButtons("mesh_shape",label="Mesh size",
                                          choices=list("coarse","fine"))),
                    
                    br(),
                    textOutput("select_warn"),
                    br()
                    ),
             column(7,
                     
                     plotOutput("test1",width = 400,height=400),
                     h4("Cotton summary statistics"),
                     textOutput("shape_mean"),
                     textOutput("shape_sd"),
                     textOutput("shape_n"),
                    br(),
                    h4(textOutput("shape_head")),
                    textOutput("shape_lit_mean"),
                    textOutput("shape_lit_sd")
                    )))),
tabPanel("More info",
         h1("CELLDEX Geospatial Decomposition Predictor"),
         br(),
         p("Welcome to the CELLulose Decomposition EXperiment (CELLDEX) geospatial tool for predicting cotton and litter decomposition in stream ecosystems.
           This Shiny app was developed to allow users to predict cellulose and leaf litter decay rates in streams across the globe.
           The predictive model is build from boosted regression trees (BRT) that predict cotton decay rates from watershed variables provided by HydroSHEDS.
           The model results are validate by natural litter bag decay rates using cotton and genus-level litter traits."),
         br(),
         h3("Getting started"),
         p("Click anywhere on the", strong("Interactive Map"), "to see point estimates of cotton decay rates.
           If you select the Leaf litter radio button, you can pick a litter genus and experimental condition to predict litter bag decay rates."),
         p("Use the tools on the map to select an area of interest. 
           Then click the",strong("Shape output"),"tab to see summary statistics and a kernel density plot of cotton decay in the selected area.
           Select a leaf litter genus to see estimated decay for natural litter in the selected area."),
         br(),
         h3("How to cite this resource"),
         p("Costello DM, JP Schmidt, KA Capps, CJ Patrick, and SD Tiegs. 2023. CELLDEX Geospatial Decomposition Predictor.",
           
           a(href="https://zenodo.org/badge/latestdoi/621003485","DOI: 10.5281/zenodo.8008082")
             
         ),
         br(),
         h3("Other resources"),
         h4("CELLDEX papers"),
         p("Tiegs et al. 2019 Global patterns and drivers of ecosystem functioning in rivers and riparian zones.",
           em("Science Advances"),
           a("pdf",href="https://www.science.org/doi/epdf/10.1126/sciadv.aav0486")),
         p("Costello et al. 2022 Global patterns and controls of nutrient immobilization on decomposing cellulose in riverine ecosystems.",
           em("Global Biogeochemical Cycles"),
           a("pdf",href="https://agupubs.onlinelibrary.wiley.com/doi/am-pdf/10.1029/2021GB007163")),
         
         h4("Cotton strips"),
         p("Tiegs et al. 2013 A standardized cotton-strip assay for measuring organic-matter decomposition in streams.",
           em("Ecological Indicators"),
           a("pdf",href="https://www.researchgate.net/profile/Scott-Tiegs/publication/257593011_A_standardized_cotton-strip_assay_for_measuring_organic-matter_decomposition_in_streams/links/5c0e6e1292851c39ebe26605/A-standardized-cotton-strip-assay-for-measuring-organic-matter-decomposition-in-streams.pdf")),
         
         h4("CELLDEX data repositories"),
         p("CELLDEX Geospatial model",
           a("GitHub",href="https://github.com/dmcostello/CELLDEX_geospatial")),
         p("CELLDEX cotton decomposition data",
           a("GitHub",href="https://github.com/dmcostello/CELLDEX2018")),
         
         h4("Other data sources"),
         p(a("HydroSHEDS",href="https://www.hydrosheds.org")),
         p(a("TRY Plant trait database",href="https://www.try-db.org/TryWeb/Home.php")),
         p("Follstad Shah et al. 2017 Global synthesis of the temperature sensitivity of leaf litter breakdown in streams and rivers.",
           em("Global Change Biology"),
           a("pdf",href="https://onlinelibrary.wiley.com/doi/epdf/10.1111/gcb.13609")),
         p("LeRoy et al. 2020 Plant phylogenetic history explains in‐stream decomposition at a global scale.",
           em("Journal of Ecology"),
           a("pdf",href="https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/1365-2745.13262")),
         
  ))
                 

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
    
    addCircleMarkers(data = LITsites, lat =  ~latitude, lng =~longitude,
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
  
  #Extract coordinated of shape drawn by user. What is extracted depends on the shape
  user_shape <- reactive(eval(input$map_draw_new_feature$properties$feature_type[[1]]))
  
  user_coord <- reactive(
    if(user_shape()=="rectangle"){
    matrix(unlist(input$map_draw_new_feature$geometry$coordinates[[1]]),byrow = T,ncol=2)}
    else  if(user_shape()=="circle"){
      matrix(unlist(input$map_draw_new_feature$geometry$coordinates),byrow = T,ncol=2)}
    else if (user_shape()=="polygon"){
      matrix(unlist(input$map_draw_new_feature$geometry$coordinates[[1]]),byrow = T,ncol=2)}
  )
  
  user_buf <- reactive(unlist(input$map_draw_new_feature$properties$radius))
  
  user_shape_kd <- reactive(
    if(user_shape()=="rectangle"){
    raster::extract(x=skd,y=spPolygons(user_coord()))
  } else
    if(user_shape()=="circle"){
      raster::extract(x=skd,y=user_coord(),buffer=user_buf())
  } else
    if(user_shape()=="polygon"){
      raster::extract(x=skd,y=spPolygons(user_coord()))}
  )
  
  
  #Old observers to check shape output
  #observeEvent(input$map_draw_new_feature,{print(user_shape())})
  #observeEvent(input$map_draw_new_feature,{print(user_coord())})
  #observeEvent(input$map_draw_new_feature,{print(user_buf())})
  
  
  #Generate cotton output from shapes
  output$test1 <- renderPlot({
    req(input$map_draw_new_feature)
    polykd_den <- density(unlist(user_shape_kd()),na.rm=T)
        if(input$lit_select=="None"){
    with(polykd_den,plot(x,y,type="l",las=1,col="green3",lwd=2,
                         ylab="Relative frequency",xlab="Decomp. rate (1/d)"))
    } else
      if(length(input$lit_select!="None")){
        litkd_den <- density(litdat(),na.rm=T)
        xmax <- max(litdat(),unlist(user_shape_kd()),na.rm=T)
        xmin <- min(litdat(),unlist(user_shape_kd()),na.rm=T)
        ymax <- max(litkd_den$y,polykd_den$y)
        with(polykd_den,plot(x,y,type="l",las=1,col="green3",lwd=2,
                             ylab="Relative frequency",xlab="Decomp. rate (1/d)",
                             xlim=c(xmin,xmax),ylim=c(0,ymax)))
        legend("topright",legend=c("Cotton",input$lit_select),col=c("green3","red"),
               lwd=2,text.col=c("green3","red"))
        with(litkd_den,lines(x,y,col="red",lwd=2))
      }
    #hist(unlist(user_shape_kd()),xlab="Kd (1/d)",
         #main=paste0("Cotton (cells = ",length(unlist(user_shape_kd())),")"),las=1)
  })
  
  output$shape_mean <- renderText({
    validate(
      need(input$map_draw_new_feature !="","Please select an area on the map")
    )
    req(input$map_draw_new_feature)
    paste0("Mean decay (1/d) = ",unlist(user_shape_kd()) 
           %>% mean(na.rm=T) 
           %>% round(3))
  })
  output$shape_sd <- renderText({
    req(input$map_draw_new_feature)
    paste0("Standard deviation = ",unlist(user_shape_kd())
           %>% sd(na.rm=T)
           %>% round(3)
    )
  })
  output$shape_n <- renderText({
    req(input$map_draw_new_feature)
    paste0("Cell count = ",sum(!is.nan(unlist(user_shape_kd()))))
  })
  
  #Generate litter output from shape and button input
  

  
  #Warning of more than 1 litter types selected
  warn3 <- eventReactive(input$goshape,{
    ifelse(length(input$lit_select) >1,
           as.character("Please select only 1 litter type"),
           as.character("")
           )
  })
  
  output$select_warn <- renderText({warn3()})
  
  #Reactives to create new data frame
  litdat <- reactive(
    exp(predict(Fgbm,n.trees=best.iter2,newdata=
                           cbind(traits[traits$Genus==input$lit_select,],
                                 Mesh.size=factor(input$mesh_shape),
                                 Leaf.condition=factor(input$cond_shape),
                                 ln_pred_k=log(unlist(user_shape_kd())))
    ))
 )
 
  output$shape_head <- renderText({
    if(input$lit_select!="None"){
      paste(input$lit_select,"summary statistics")
    }
  })
  
  output$shape_lit_mean <- renderText({
    if(input$lit_select!="None"){
      validate(
        need(input$map_draw_new_feature !="","Please select an area on the map")
      )
      req(input$map_draw_new_feature)
    paste0("Mean decay (1/d) = ",litdat() 
           %>% mean(na.rm=T) 
           %>% round(3))
           }
  })
  
  output$shape_lit_sd <- renderText({
    if(input$lit_select!="None"){
      req(input$map_draw_new_feature)
    paste0("Standard deviation = ",litdat()
           %>% sd(na.rm=T)
           %>% round(3))
    }
  })
  
  #observeEvent(input$goshape,{print(max(litdat()))})
  
  
  
    #Point output from rasters
  
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
      exp(predict(Fgbm, n.trees=best.iter2,
              newdata=cbind(traits[traits$Genus==input$leaf,],
                            Mesh.size=factor(input$mesh),
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
