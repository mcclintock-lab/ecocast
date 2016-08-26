TIF_DIR <- 'geotif_data'
SEA_LION <- 'casl'
SWORDFISH <- 'swpa2'
BLUE_SHARK_BYCATCH <- 'blpa2'
BLUE_SHARK_TRACKING <- 'blTpa1'
LEATHERBACK <- 'LBST_BRT'
getDataDirs <- function(){

  dirs <- c(file.path(TIF_DIR, SEA_LION), file.path(TIF_DIR, SEA_LION), 
            file.path(TIF_DIR, BLUE_SHARK_BYCATCH), file.path(TIF_DIR, BLUE_SHARK_TRACKING),
            file.path(TIF_DIR, LEATHERBACK))
}

getDates <- function(){
  
  dirs <- getDataDirs()
  p<-dirs[[1]]
  print("here....")
  print(p)
  files <- list.files(p)
  num_files <- length(files)
  
  dates<-vector(length=num_files)
  for (i in 1:num_files) {
    filename<-files[[i]]
    parts <- unlist(strsplit(filename, "_"))
    species <- parts[[1]]
    date_and_suffix_part <- parts[[2]]
    datepart <- unlist(strsplit(date_and_suffix_part, "[.]"))
    full_date <- datepart[[1]]
    date_parts <- unlist(strsplit(full_date, "-"))
    
    year<- date_parts[[1]]
    month <- date_parts[[2]]
    day <- date_parts[[3]]
    dates[[i]]<-full_date
  }
  dates
}
getDisplayDates <-function(){
  dates<-getDates()
  dates
}