#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tiff)
library(shiny)
library(raster)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(RCurl)
library(urltools)
library(data.table)
library(jsonlite)
library(rgdal)
library(geojsonio)
library(maptools)
library(proj4)
#built in values for now...
DATE_ONE_STR = "August 11, 2016"
DATE_TWO_STR = "August 18, 2016"
DATE_ONE = "8_11_16"
DATE_TWO = "8_18_16"
dateVals<-c(DATE_ONE, DATE_TWO)
dateDisplayVals<-c(DATE_ONE_STR, DATE_TWO_STR)
names(dateVals) <- dateDisplayVals
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = "style.css",
                        
   # Application title
   div(class="img_banner", 
       img(src="banner.png",height=95)
   ),
   fluidRow(
     column(8,  
            leafletOutput("mymap", width=1000,height=750),
            absolutePanel(top = 105, left = 60, height=120, width=300, fixed=TRUE,
                          style = "opacity:0.70;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: white;",
                          selectInput("timeSelect", "Date To Show", dateDisplayVals),
                          tags$head(tags$style(".tab-content {overflow: visible;}")),
                          actionButton("zoomToWestCoast", "Zoom to West Coast", style="display:inline;font-size:0.8em;"),
                          actionButton("zoomToBight", "Zoom to California Bight", style="display:inline-block;font-size:0.8em;")
            )
          ),
     column(4, style="padding-right:20px;",
            inputPanel(style = "overflow-y:scroll;width:280px;overflow-x:hidden; max-height: 400px;opacity:0.70;border-radius: 3px; padding-left: 8px; opacity: 0.92;border: 1px solid; background: white;",
                          h6("West Coast Model"),
                          
                          checkboxInput(inputId="swordFish", "Show Swordfish",value=TRUE),
                          conditionalPanel(
                            condition='input.swordFish',
                            class="sliderPanel",
                            sliderInput("swordFishSlider", "Swordfish Predictive Surface", 0,100,c(50,70),step=1)
                          ),                          
                          
                       
                          checkboxInput("turtle", "Show Sea Turtles", value=FALSE),
                          conditionalPanel(
                            condition='input.turtle',
                            class="sliderPanel",
                            sliderInput("turtleSlider", "Turtle Predictive Surface", 0,100,c(20,30),step=1)
                          ),
                          
                          
                          checkboxInput("lbst", "Show LBST", value=FALSE),
                          conditionalPanel(
                            condition='input.lbst',
                            class="sliderPanel",
                            sliderInput("lbstSlider", "LBST Predictive Surface", 0,100,c(30,40),step=1)
                          ),
                          
                          checkboxInput("blsh", "Show BLSH", value=FALSE),
                          conditionalPanel(
                            condition='input.blsh',
                            class="sliderPanel",
                            sliderInput("blshSlider", "Blsh Predictive Surface", 0,100,c(40,50),step=1)
                          ),
                          
                          checkboxInput("average",  "Average All Selected Rasters", value=TRUE)
                          
            ),
            inputPanel(style = "opacity:0.70;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: white;",
                          h5("High Resolution California Bight Model"),
                          checkboxInput("bight", "Show Snipe Bight Data",value=TRUE),
                          sliderInput("bightSlider", "Snipe Predictive Surface", 0,100,c(30,40),step=1)
            )    
    )
   )

))

buildCache <- function(){

  
  rurls <- c("https://s3-us-west-2.amazonaws.com/mcclintocklab",
              "https://s3-us-west-2.amazonaws.com/mcclintocklab",
              "https://s3-us-west-2.amazonaws.com/mcclintocklab")
  rasts <- c("NO", "NO", "NO")
  local_filenames <- c("swordfish32.tif",
                        "turtles32.tif",
                        "bight32.tif")
  rkeys <- c("swordfish", "turtles", "bight1")
  
  imageCache <- data.table(urls=rurls, rasters=rasts, 
                           fnames=local_filenames, keys=rkeys)
  imageCache
}

imageCache<- buildCache()

getBycatchRaster <- function(imageCache, index, selDate){

  cacheImg <- imageCache[[index, 2]]
  base_name <- imageCache[[index, 3]]
  
  url <- paste(imageCache[[index, 1]],"/", selDate, "/",base_name,sep="")
  local_name <- paste("./","local_",selDate,"_", base_name,sep="")

  if(!file.exists(local_name)){
    img <- download.file(url, local_name, extra="-g")
    rast <- raster(local_name)
    imageCache[index, 2:="YES"]
  } else {
    rast <-raster(local_name)
  }
  rast
}

addFilteredRasters <- function(rstack, ranges, targetRaster){
  filter_fun <- function(x) { x[(x < rangeMin) | (x > rangeMax) ] <- NA; return(x) }
  rangeMin <- ranges[1]*0.01
  rangeMax <- ranges[2]*0.01
  filt <- calc(targetRaster, filter_fun)
  if(!is.null(rstack)){
    rstack <- c(rstack, filt)
  } else{
    rstack <- c(filt)
  }
  rstack
}
#addRasterImage(inrast, colors=pal, opacity = 0.9, maxBytes = 123123123) %>% 
#%>% fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
# Define server logic required to draw a histogram
wc_ext<-list()
wc_ext$lon<-(-119.417931)
wc_ext$lat<-36.778259
wc_ext$zoom<-5
bight_ext<-list()
bight_ext$lon<-(-119.2)
bight_ext$lat<-34.35
bight_ext$zoom<-8
server <- shinyServer(function(input, output) {
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$zoomToWestCoast, {
    #need this null to force a data change and a re-render
    v$data<-NULL
    v$data <- wc_ext
  })
  
  observeEvent(input$zoomToBight, {
    #need this null to force a data change and a re-render
    v$data<-NULL
    v$data <- bight_ext
  }) 
  
  output$mymap <- renderLeaflet({
    target_ext <- v$data
    if(is.null(target_ext)){
      target_ext<-wc_ext
    }
    print("target----")
    print(target_ext)
    
    showSwordFish = input$swordFish
    showTurtles = input$turtle
    showBight = input$bight
    average = input$average
    selDate<- dateVals[input$timeSelect]

    scaledLegendAndColorRange <- FALSE
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 5
      step <-1
      incProgress(1/incrs, detail = paste("reading..."))
      swordFishRaster <- getBycatchRaster(imageCache, 1, selDate)

      if(showTurtles){
        turtleRaster <- getBycatchRaster(imageCache, 2, selDate)  
      }
      if(FALSE){
        bightRaster <- getBycatchRaster(imageCache, 3, selDate)
        
      }
      rstack <- c()
      highResStack <- c()

      if(showSwordFish){
        incProgress(2/incrs, detail = paste("filtering swordfish data..."))
        ranges <- c(input$swordFishSlider)
        rstack <- addFilteredRasters(rstack, ranges, swordFishRaster)
      } 
      
      if(showTurtles){
        incProgress(3/incrs, detail = paste("filtering turtle data..."))
        ranges <- c(input$turtleSlider)
        rstack <- addFilteredRasters(rstack, ranges, turtleRaster)
      } 
      
      if(FALSE){
        incProgress(4/incrs, detail = paste("filtering bight data..."))
        ranges <- c(input$bightSlider)
        highResStack <- addFilteredRasters(highResStack, ranges, bightRaster)
        
      } 
      
      raster_stack <- NULL
      if(length(rstack) > 1){
          for(i in 1:length(rstack)){
            if(i==1){
              targetRast <- rstack[[i]]
              raster_stack <- stack(rstack[[i]])
            } else{
              if(average){
                raster_stack <- addLayer(raster_stack, rstack[[i]])
              } else {
                targetRast <- merge(targetRast, rstack[[i]])
              }
            }
          }
      } else if(length(rstack) == 1){
        targetRast <- rstack[[1]]
      } else {
        if(average && length(rstack) > 0){
          targetRast <- calc(raster_stack, fun=mean, na.rm=TRUE)
        } else {
          if(length(rstack) == 0){
            targetRast <- NULL
          } else {
            targetRast <- rstack[[1]]  
          }
        }
      }
      if(FALSE){
        print("merging in high res stack")
        agg_rast = aggregate(highResStack[[1]], fact=25)
        targetRast <- merge(targetRast, agg_rast)
      }
      incProgress(5/incrs, detail = paste("drawing..."))
      
      #Spectral
      pal <- rev(brewer.pal(10,"Spectral"))
      vals <- NULL
      
      #for now, palette is taken from swordfish and everything is assumed to be same range
      palette <- colorNumeric(pal, values(swordFishRaster),
                              na.color = "transparent")    
      vals = values(swordFishRaster)


      if(!is.null(targetRast)){
        lmap <- leaflet() %>%
          addProviderTiles("CartoDB.Positron",
                           options = providerTileOptions(noWrap = TRUE)
          )  %>% setView(target_ext$lon, target_ext$lat, zoom=target_ext$zoom)  %>%
          addLegend("bottomleft", pal = palette, values = vals, title = "Values") %>%
          addRasterImage(targetRast, colors=palette, group="swordfish", opacity = 0.8, maxBytes = 123123123, project=FALSE)%>% 
          addWMSTiles(
            "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
            layers = "nexrad-n0r-900913",
            options = WMSTileOptions(format = "image/png", transparent = TRUE),
            attribution = "Weather data © 2012 IEM Nexrad"
          )
        lmap
      } else {
        lmap <- leaflet() %>%
          addProviderTiles("CartoDB.Positron",
                           options = providerTileOptions(noWrap = TRUE)
          )  %>% setView(target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
        lmap
      }
    })
  })
  

  
})

# Run the application 
shinyApp(ui = ui, server = server)

