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
   leafletOutput("mymap", width=1000, height=700),
   absolutePanel(top = 105, left = 60, height=80, width=300, fixed=TRUE,
                style = "opacity:0.70;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: white;",
                selectInput("timeSelect", "Date To Show", dateDisplayVals),
                tags$head(tags$style(".tab-content {overflow: visible;}"))
                
   ), 
  absolutePanel(top = 105, right = 30, height=350, width=300, fixed=TRUE,
                 style = "opacity:0.70;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: white;",
                 h5("West Coast Model"),
                 checkboxInput("swordFish", "Show Swordfish",value=TRUE),
                 sliderInput("swordFishSlider", "Swordfish Predictive Surface", 0,100,c(50,70),step=1),
                 checkboxInput("turtle", "Show Sea Turtles", value=TRUE),
                 sliderInput("turtleSlider", "Turtle Predictive Surface", 0,100,c(20,30),step=1),
                 checkboxInput("average", "Average Selected Rasters", value=TRUE)
   ),
   absolutePanel(top = 460, right = 30, height=200, width=300, fixed=TRUE,
                 style = "opacity:0.70;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: white;",
                 h5("High Resolution California Bight Model"),
                 checkboxInput("bight", "Show Snipe Bight Data",value=TRUE),
                 sliderInput("bightSlider", "Snipe Predictive Surface", 0,100,c(30,40),step=1)
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
server <- shinyServer(function(input, output) {
  lon = -119.417931
  lat = 36.778259

  output$mymap <- renderLeaflet({
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
      print("?????")
      print(swordFishRaster)
      if(showTurtles){
        turtleRaster <- getBycatchRaster(imageCache, 2, selDate)  
      }
      if(FALSE){
        bightRaster <- getBycatchRaster(imageCache, 3, selDate)
        
        print(bightRaster)
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
      
      print("drawing leaflet??")
      if(!is.null(targetRast)){
        lmap <- leaflet() %>%
          addProviderTiles("CartoDB.Positron",
                           options = providerTileOptions(noWrap = TRUE)
          )  %>% setView(lon, lat, zoom=5) %>% 
          addLegend("bottomleft", pal = palette, values = vals, title = "Values") %>%
          addRasterImage(targetRast, colors=palette, group="swordfish", opacity = 0.8, maxBytes = 123123123, project=FALSE)
        lmap
      } else {
        lmap <- leaflet() %>%
          addProviderTiles("CartoDB.Positron",
                           options = providerTileOptions(noWrap = TRUE)
          )  %>% setView(lon, lat, zoom=5)
        lmap
      }
    })
  })
  

  
})

# Run the application 
shinyApp(ui = ui, server = server)

