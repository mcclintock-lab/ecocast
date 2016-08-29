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

source('data_utils.R')
displayDates<-getDisplayDates()
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(theme = "style.css",
   tags$style(type = "text/css", "html, body {width:1000px;height:750px;}"),           
   # Application title
   
   fluidRow(
     column(12, id="banner",
        div(class="img_banner", 
            img(src="banner.png",height=95)
        )
     ),
     column(8,id="main",
            leafletOutput("mymap", width=970,height=700),
            absolutePanel(top = 105, left = 60, height=120, width=300, fixed=TRUE,
                          style = "opacity:0.70;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: white;",
                          selectInput("timeSelect", "Date To Show", displayDates),
                          tags$head(tags$style(".tab-content {overflow: visible;}")),
                          actionButton("zoomToWestCoast", "Zoom to West Coast", style="display:inline;font-size:0.8em;"),
                          actionButton("zoomToBight", "Zoom to California Bight", style="display:inline-block;font-size:0.8em;")
            )
          ),
     column(4, id="rightPanel",
            inputPanel(class="largePanel",
                          h6("West Coast Model"),
                          
                          checkboxInput(inputId="swpa2", "Show Swordfish",value=TRUE),
                          conditionalPanel(
                            condition='input.swpa2',
                            class="sliderPanel",
                            sliderInput("swpa2Slider", "Swordfish Predictive Surface", 0,100,c(0,100),step=1)
                          ),                          
                
                          checkboxInput("LBST_BRT", "Show Leatherback Sea Turtles", value=FALSE),
                          conditionalPanel(
                            condition='input.LBST_BRT',
                            class="sliderPanel",
                            sliderInput("LBST_BRTSlider", "Turtle Predictive Surface", 0,100,c(00,100),step=1)
                          ),
                          
                          
                          checkboxInput("blpa2", "Show Blue Shark Bycatch Weighting", value=FALSE),
                          conditionalPanel(
                            condition='input.blpa2',
                            class="sliderPanel",
                            sliderInput("blpa2Slider", "Blue Shark Predictive Surface", 0,100,c(0,100),step=1)
                          ),
                         checkboxInput("blTpa1", "Show Blue Shark Tracking Weighting", value=FALSE),
                         conditionalPanel(
                           condition='input.blTpa1',
                           class="sliderPanel",
                           sliderInput("blTpa1Slider", "Blue Shark Tracking Predictive Surface", 0,100,c(0,100),step=1)
                         ),                          
                         checkboxInput("casl", "Show California Sea Lion", value=FALSE),
                         conditionalPanel(
                            condition='input.casl',
                             class="sliderPanel",
                            sliderInput("caslSlider", "Sea Lion Predictive Surface", 0,100,c(0,100),step=1)
                         ),
                          h5("Combine the selected model outputs:", style="width:260px;margin-top:-5px;margin-bottom:2px;"),
                          span(style="display:block;padding-left:20px;width:260px;",
                               checkboxInput("average",  "Average", value=TRUE)
                          )
         
            ),

            inputPanel(class="smallPanel",
                       h5("High Resolution California Bight Model"),
                       checkboxInput("SWOR_HiRes", "Show Swordfish Bight Data",value=TRUE),
                       conditionalPanel(
                         condition='input.SWOR_HiRes',
                         class="sliderPanel",
                         sliderInput("SWOR_HiResSlider", "Swordfish Predictive Surface", 0,100,c(20,100),step=1)
                       )
            ),
            inputPanel(class="smallPanel",
                       h5("Base Layers"),
                       checkboxInput("usEEZ", "Show US EEZ",value=TRUE),
                       checkboxInput("usEEZ", "Show Leatherback Turtle Conservation Area",value=FALSE)
            )
    )
   )

))



buildCache <- function(){

  rurls <- c("https://s3-us-west-2.amazonaws.com/mcclintocklab",
              "https://s3-us-west-2.amazonaws.com/mcclintocklab",
              "https://s3-us-west-2.amazonaws.com/mcclintocklab",
              "https://s3-us-west-2.amazonaws.com/mcclintocklab",
              "https://s3-us-west-2.amazonaws.com/mcclintocklab"
             )
  rasts <- c("NO", "NO", "NO", "NO", "NO")
  local_filenames <- c("swordfish32.tif",
                        "turtles32.tif",
                        "lbst32.tif",
                        "blsh32.tif",
                        "bight32.tif")
  rkeys <- c("swordfish", "turtles", "bight1","lbst32", "blsh32")
  
  imageCache <- data.table(urls=rurls, rasters=rasts, 
                           fnames=local_filenames, keys=rkeys)
  imageCache
}

getNewBycatchRaster <- function(target_dir, selDate){
  local_dir <- file.path(TIF_DIR, target_dir)
  local_name<-paste(target_dir, "_",selDate,".tif",sep="")
  local_name<-file.path(local_dir, local_name)
  r<-raster(local_name)
  r
}

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
addRaster <- function(showVal, targetRaster, rstack, ranges, incr, progress_str){
  if(showVal){
    incProgress(incr, detail = paste(progress_str))
    rstack <- addFilteredRasters(rstack, ranges, targetRaster)
  } 
  rstack
}

# Define server logic required to draw a histogram
wc_ext<-list()
wc_ext$lon<-(-119.417931)
wc_ext$lat<-40.078259
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
    
    showSwordFish = input$swpa2
    showTurtles = input$LBST_BRT
    showBlueSharkBycatch = input$blpa2
    showBlueSharkTracking = input$blTpa1
    showSeaLions = input$casl
    showSwordFishHiRes = input$SWOR_HiRes
    average = input$average
    selDate<- input$timeSelect
    showSanctuaries = input$usEEZ
    scaledLegendAndColorRange <- FALSE
    withProgress(message = 'Loading images: ', value = 0, {
      incrs <- 7
      step <-1
      rstack<-c()
      incProgress(1/incrs, detail = paste("reading..."))
      swordFishRaster <- getNewBycatchRaster(SWORDFISH, selDate)
      print(swordFishRaster)
      if(showSwordFish){
        rstack <- addRaster(showSwordFish, swordFishRaster, rstack, c(input$swpa2Slider), 2/incrs, "Filtering swordfish data...")
      }
      if(showTurtles){
        turtleRaster <- getNewBycatchRaster(LEATHERBACK, selDate)  
        rstack <- addRaster(showTurtles, turtleRaster, rstack, c(input$LBST_BRTSlider), 3/incrs, "Filtering Turtle data...")
      }
      if(showBlueSharkTracking){
        blueSharkTrackingRaster <- getNewBycatchRaster(BLUE_SHARK_TRACKING, selDate) 
        rstack <- addRaster(showBlueSharkTracking, blueSharkTrackingRaster, rstack, c(input$blTpa1Slider), 4/incrs, "Filtering Blue Shark Tracking data...")
      }
      if(showBlueSharkBycatch){
        blueSharkBycatchRaster <- getNewBycatchRaster(BLUE_SHARK_BYCATCH, selDate) 
        rstack <- addRaster(showBlueSharkBycatch, blueSharkBycatchRaster, rstack, c(input$blpa2Slider), 5/incrs, "Filtering Blue Shark Bycatch data...")
      }
      if(showSeaLions){
        seaLionRaster <- getNewBycatchRaster(SEA_LION, selDate)
        rstack <- addRaster(showSeaLions, seaLionRaster, rstack, c(input$caslSlider), 6/incrs, "Filtering Sea Lion data...")
      }
      if(showSwordFishHiRes){
        swordFishHiResRaster <- getNewBycatchRaster(SWORDFISH_HIRES, selDate)
        #print(swordFishHiResRaster)
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
              r<-rstack[[i]]
              r[is.na(r[])] <- 0 
              if(average){
                raster_stack <- addLayer(raster_stack, r)
              } else {
                targetRast <- merge(targetRast, r)
              }
            }
          }
      } else if(length(rstack) == 1){
        targetRast <- rstack[[1]]
      } 
      
      if(average && length(raster_stack) > 0){
        incProgress(7/incrs, detail = paste("Calculating weighted average..."))
        targetRast <- overlay(raster_stack, fun=mean, na.rm=FALSE)
      } else {
        if(length(rstack) == 0){
          targetRast <- NULL
        } else {
          #use the merged one
          targetRast <- targetRast
        }
      }
      
      if(FALSE){
        print("merging in high res stack")
        agg_rast = aggregate(highResStack[[1]], fact=25)
        targetRast <- merge(targetRast, agg_rast)
      }
      
      
      #Spectral
      pal <- rev(brewer.pal(10,"Spectral"))
      vals <- NULL
      
      #targetRast<-swordFishHiResRaster
      #for now, palette is taken from swordfish and everything is assumed to be same range
         
      if(is.null(targetRast)){
        vals = values(swordFishRaster)
      } else {
        vals = values(targetRast)
      }
      
      palette <- colorNumeric(pal, vals,
                              na.color = "transparent") 
      
      #for some reason, piping %>% wasn't working quite right here, so doing them one at a time
      lmap <- leaflet()
      lmap <- addProviderTiles(lmap, "CartoDB.Positron",options = providerTileOptions(noWrap = TRUE))  
      lmap <- setView(lmap, target_ext$lon, target_ext$lat, zoom=target_ext$zoom)
      if(!is.null(targetRast)){
        lmap <- addLegend(lmap, "bottomleft", pal = palette, values = vals, title = "Values") 
        lmap <- addRasterImage(lmap, targetRast, colors=palette, group="swordfish", opacity = 0.8, maxBytes = 123123123, project=FALSE)
        if(showSanctuaries){
          lmap <- addWMSTiles(lmap, "http://gp1.seasketch.org/arcgis/services/ecocast/Sanctuaries/ImageServer/WMSServer?",
                              layers="0",
                              options = WMSTileOptions(format = "image/png", transparent = TRUE),
                              attribution = "Sanctuaries"
          )
        }
      }
      lmap

    })
  })
  

  
})

# Run the application 
shinyApp(ui = ui, server = server)

