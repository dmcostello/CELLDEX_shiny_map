#Adapted from SuperZip example https://shiny.rstudio.com/gallery/superzip-example.html
#https://github.com/rstudio/shiny-examples/tree/main/063-superzip-example


library(shiny)
library(leafem)
library(leaflet.extras)



# All datafiles are now saved in the CELLDEXgeo BRT code
# See repository at https://github.com/dmcostello/CELLDEX_geospatial
saveRDS(CELLDEX,file="~/Desktop/CELLDEX.rds")
saveRDS(global_kd,file="~/Desktop/skd.rds")

## OLD DATA CREATION CODE ##

#Load in datasets
field <- read.csv("./data/field_clean.csv")
field$part.str <- paste(field$partnerid,field$stream)
skd<-readRDS('./data/skd.rds')
ln_skd <- readRDS('./data/ln_skd.rds')

#Read in TRY trait data
traits <- readRDS("./data/traits.rds")


Csites <- readRDS('./data/CELLDEX.rds')
#Creates the html strings for map click
Csites <- mutate(Csites, cntnt=paste0('<strong>Site code: </strong>',part.str,
                                      '<br><strong>Biome:</strong> ', biome_short.y,
                                      '<br><strong>Decay rate (1/d):</strong> ', round(k,digits=3))) 

k <- read.csv("./data/str_k.csv")
colnames(k)[2] <- "part.str"

str_k <- merge(k,field[,c('biome_short','part.str','latitude','longitude')],by="part.str")

saveRDS(str_k,file="./data/CELLDEX.rds")


#Terminal code for pruning

#git fetch -p
#git branch -d branchname


#TEST MAP
pal <- colorNumeric(
  palette = "YlGn",
  domain = values(skd)
)

 leaflet(elementId = "map") %>% 
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addGeoRaster(skd,autozoom=F,
                 colorOptions = colorOptions(palette="YlGn"),opacity = 0.8,
                 options=leaflet::tileOptions(noWrap=T)) %>% 
    addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
                     color = "#1b9e77",
                     radius = 3, popup = ~as.character(cntnt),
                     stroke = FALSE, fillOpacity = 1) %>%
   addDrawToolbar(targetGroup = "select area",
                  position = "topleft",polylineOptions = F,markerOptions = F,
                  circleMarkerOptions = F,singleFeature = T,
                  editOptions = editToolbarOptions(
                    selectedPathOptions = selectedPathOptions())) %>%
    addLegend("bottomright", pal = pal, values = values(skd),
              title = "k (1/d)",opacity = 1) %>%
     setView(lng = -81.36, lat = 41.15, zoom = 6) %>%
    addMouseCoordinates(epsg = NULL, proj4string = NULL, native.crs = FALSE) %>%
    leafem::addImageQuery(skd,
                          layerId = 'r',
                          type='click',
                          digits=2)
 
 #Example points to extract
 coord<- data.frame(long=c(-84.9),lat=c(42.77))
 raster::extract(x=skd,y=coord)
 
 #Example of rectangle extract
 user_shape = "rectangle"
 user_coord = matrix(c(-87.21814, 40.69151,
                       -87.21814, 43.15974,
                       -79.23580, 43.15974,
                       -79.23580, 40.69151,
                       -87.21814, 40.69151),ncol=2,byrow = T)
 user_buffer = 41000
 
 polykd <- raster::extract(x=skd,y=spPolygons(user_coord))
 (circkd <- raster::extract(x=skd,y=coord,buffer=41000))
 
 
hist(unlist(polykd),las=1,freq=F)

polykd_den <- density(unlist(polykd),na.rm=T)
with(polykd_den,plot(x,y,type="l",las=1,col="green3",lwd=2,
                     ylab="Relative frequency",xlab="Decomp. rate (1/d)"))
  
unlist(polykd) %>% mean(na.rm=T) %>% round(3)
sum(!is.nan(unlist(polykd)))
 
#AGGREGATE TRAITS
 CPdat <- read.csv("./data/AnalysisData_landcat_nocastaneav2.csv")
 traits<-aggregate(CPdat[,13:33],by=list(CPdat$Genus),mean)
 names(traits)[1] <- "Genus"
saveRDS(traits,"traits.rds")
 
saveRDS(CPdat,"FSdat.rds")

#TESTING MODEL PREDICTION
#Load models
load("FSmodel.rda")  

best.iter2 <- gbm.perf(fgbm,method="cv")

#Create a datasheet with the variables
acerdat <- traits[traits$Genus=="Acer",]
conddat <- data.frame(mesh.size.category=1,Leaf.condition=2)
kd <- data.frame(ln_pred_k=-5.5)
acerdat2 <- cbind(acerdat,conddat,kd)
(pred1<-exp(predict(fgbm, newdata=acerdat2, n.trees=best.iter2)))

conddat <- data.frame(mesh.size.category=0,Leaf.condition=0)
kd <- data.frame(ln_pred_k=-5.5)
trait_mean <- t(colMeans(traits[,2:22]))
meandat <- cbind(trait_mean,conddat,kd)
predict(fgbm, newdata=meandat, n.trees=best.iter2)

#Litter polygons
kdpoly <- data.frame(ln_pred_k=log(unlist(polykd)))
acerpoly <- cbind(kdpoly,acerdat,conddat)
(pred2<-exp(predict(fgbm, newdata=acerpoly, n.trees=best.iter2)))

lit <- c("Alnus","Carya")

lit_dat_list <- list(kdpoly,kdpoly)
names(lit_dat_list)<- lit

dat<-data.frame(NULL)

for(i in 1:length(lit)){
  x<- cbind(kdpoly,
    traits[traits$Genus==lit[i],],
    conddat)
  dat <- rbind(dat,x)
}

dat$predk <- exp(predict(fgbm,newdata=dat,n.trees=best.iter2))

#PINE BARK BEETLE
#From Gonzalez-Hernandez 2020 Modelling potential distribution of a pine bark beetle in Mexican 
#temperate forests using forecast data and spatial analysis tools

pine <- read.csv("pine_hucs.csv")
summary(pine)
pine$beetle <- factor(pine$beetle)
  #1 = moderate to high risk of invasion, 0 = low to no risk of invasion
names(pine)[2:3] <- c("Long","Lat")

leaflet(elementId = "map") %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircleMarkers(data = pine[pine$beetle=="0",], lat =  ~Lat, lng =~Long,
                   color = "#1b9e77",
                   radius = 3, stroke = FALSE, fillOpacity = 1) %>%
  addCircleMarkers(data = pine[pine$beetle=="1",], lat =  ~Lat, lng =~Long,
                   color = "orangered",
                   radius = 3, stroke = FALSE, fillOpacity = 1) %>%
  addLegend("topright",colors = c("#1b9e77","orangered"),
            labels=c("No-Low","Moderate-High"),opacity=1,title ="Risk of invasion")

#Predict decay
Mex_cot <- log(raster::extract(x=skd,y=pine[pine$beetle=="1",2:3]))

oakdat <- traits[traits$Genus=="Quercus",]
pinedat <- traits[traits$Genus=="Pinus",]

conddat <- data.frame(mesh.size.category="coarse",Leaf.condition="senesced")

oakdat2 <- cbind(oakdat,conddat,ln_pred_k=Mex_cot)
pinedat2 <- cbind(pinedat,conddat,ln_pred_k=Mex_cot)

Mex_oak<-exp(predict(fgbm, newdata=oakdat2, n.trees=best.iter2))
Mex_pine<-exp(predict(fgbm, newdata=pinedat2, n.trees=best.iter2))

#Calculate empirical density
den_oak <- density(Mex_oak,na.rm=T)
den_pine <- density(Mex_pine,na.rm=T)

#Plotting (Or use color goldenrod2)

with(den_oak,plot(x,y,type="l",lwd=4,col="orangered",lty=3,xlim=c(0.002,0.02),
                  las=1,ylim=c(0,500),yaxt="n",
                  ylab="",xlab="Decomp. rate (1/d)",cex.lab=1.5))
mtext("Relative frequency",side=2,line=1,cex=1.5)
with(den_pine,lines(x,y,lwd=4,col="forestgreen"))
legend("topright",cex=1.2,
       legend=c(substitute(paste(italic("Pinus"))),
                           substitute(paste(italic("Quercus")))),
       lwd=4,lty=c(1,3),
       col=c("forestgreen","orangered"),text.col = c("forestgreen","orangered"))

  
#Run app online
library(rsconnect)
rsconnect::deployApp('~/Library/CloudStorage/OneDrive-KentStateUniversity/Research projects/2020 CELLDEX spatial analysis/CELLDEX map/')
  

## OLD CODE for turing on/off cotton and litter - now within leaflet
# #Turn on and off points where decay was measured
# observe({proxy <- leafletProxy("map")
# if(input$sites=="Cotton")
# {proxy %>% clearMarkers()
#   proxy %>% addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
#                             color = "#1b9e77",
#                             radius = 3, popup = ~as.character(cntnt),
#                             stroke = FALSE, fillOpacity = 0.8)
# } else
# if(input$sites=="None")
# {proxy %>% clearMarkers()
#   } else
# 
# if(input$sites=="Leaf litter")
# {proxy %>% clearMarkers()
#   proxy %>% addCircleMarkers(data = FSsites, lat =  ~Latitude.2, lng =~Longitude.2,
#                             color = "firebrick",
#                             radius = 3, popup = ~as.character(cntnt),
#                             stroke = FALSE, fillOpacity = 0.8)
#   } else
# if(input$sites=="Both")
# {proxy %>% clearMarkers()
#   proxy %>% addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
#                              color = "#1b9e77",
#                              radius = 3, popup = ~as.character(cntnt),
#                              stroke = FALSE, fillOpacity = 0.8)
#   proxy %>% addCircleMarkers(data = FSsites, lat =  ~Latitude.2, lng =~Longitude.2,
#                              color = "firebrick",
#                              radius = 3, popup = ~as.character(cntnt),
#                              stroke = FALSE, fillOpacity = 0.8)
# }
# 
# })
# 
#radioButtons("sites",label="Show sites",
#choices=list("None","Cotton","Leaf litter","Both"),
#selected = "Cotton",


#Old code attempting to plot multiple litter types on one density plot
#UI
dropdown(label="Select litter",tags$label("Choose :"),
         fluidRow(
           column(6,
                  checkboxGroupInput("lit_shape_a", label=NULL,choices=as.list(
                    traits$Genus[1:16]), selected = NULL)),
           column(6,
                  checkboxGroupInput("lit_shape_b", label=NULL,choices=as.list(
                    traits$Genus[17:31]), selected = NULL))
         )
),

actionButton("goshape", label="Calculate Kd", icon = NULL)
),

#SERVER
#Stitch together two columns if litter genera
lit_select <- reactive({
  x <- c(input$lit_shape_a, input$lit_shape_b)
})

#),