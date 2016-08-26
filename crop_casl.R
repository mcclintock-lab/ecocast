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

casl_dir<-file.path(TIF_DIR, 'casl_orig')
new_casl_dir<-file.path(TIF_DIR, SEA_LION)
files <- list.files(casl_dir)

num_files <- length(files)
print(num_files)
swordfish_dir<-file.path(TIF_DIR, SWORDFISH)
swordfish_files<-list.files(swordfish_dir)
print(swordfish_files[[1]])
sw_raster<-raster(file.path(swordfish_dir,swordfish_files[[1]]))
orig_extent<-extent(sw_raster)
for (i in 1:num_files) {
  casl_file<-files[[i]]
  print(casl_file)
  orig_raster<-raster(file.path(casl_dir,casl_file))
  new_raster<-crop(orig_raster, orig_extent)
  basename<-basename(casl_file)
  new_file<-file.path(new_casl_dir, basename);
  print(paste("writing raster:",new_file))
  writeRaster(new_raster, new_file)
}