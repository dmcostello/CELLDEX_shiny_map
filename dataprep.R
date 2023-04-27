library(shiny)
library(leafem)

#Load in datasets
field <- read.csv("./data/field_clean.csv")
field$part.str <- paste(field$partnerid,field$stream)
skd<-readRDS('./data/skd.rds')
ln_skd <- readRDS('./data/ln_skd.rds')
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

 leaflet(Csites) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addGeoRaster(skd,autozoom=F,
                 colorOptions = colorOptions(palette="YlGn"),opacity = 0.8,
                 options=leaflet::tileOptions(noWrap=T)) %>% 
    addCircleMarkers(data = Csites, lat =  ~latitude, lng =~longitude,
                     color = "#1b9e77",
                     radius = 3, popup = ~as.character(cntnt),
                     stroke = FALSE, fillOpacity = 1) %>%
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
 

 #AGGREGATE TRAITS
 CPdat <- read.csv("./data/AnalysisData_landcat_nocastaneav2.csv")
 traits<-aggregate(CPdat[,13:33],by=list(CPdat$Genus),mean)
 names(traits)[1] <- "Genus"
saveRDS(traits,"traits.rds")
 
saveRDS(CPdat,"FSdat.rds")
