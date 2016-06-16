#' Run the Spatial Analysis Application
#' @return no return value
#' @import shiny
#' @export
runSpatial <- function(){
  
  requiredPackages <- c("leaflet", "RColorBrewer", "shinyjs", "htmlwidgets",
      "ggplot2", "rCharts")
  newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  
  if(length(newPackages) > 0) {
    
    install.packages(newPackages)
    
  } 
  
  
  if(!require(shinysky)){
    
    devtools::install_github("AnalytixWare/ShinySky")
    
  }
  
  if(!require(webshot)){
    
    install_phantomjs(version = "2.1.1",
        baseURL = "https://bitbucket.org/ariya/phantomjs/downloads/")
    
    devtools::install_github("wch/webshot")
    
  }
  
  
  
#  # To debug
#  remove.packages(pkgs = c("leaflet", "RColorBrewer", "shinyjs", "htmlwidgets",
#          "shinysky", "webshot", "ggplot2"))
  
  spatialTmpDir <- tempdir()
  
  setwd(spatialTmpDir)
  
  # Copy server.R and ui.R (not folder www)
  spatialUiDir <- system.file("ui", package = "spatialAnalysis")
  spatialUiFiles <- list.files(path = spatialUiDir, full.names = TRUE)
  spatialUiFiles <- spatialUiFiles[!grepl("www", spatialUiFiles)]
  
  sapply(spatialUiFiles, function(x){
        file.copy(from = x, to = file.path(spatialTmpDir, basename(x)),
            overwrite = TRUE)
      }
  )
  
  # Make www directory and copy its files
  if (!dir.exists(file.path(proastTmpDir, "www"))) {
    
    dir.create(path = file.path(spatialTmpDir, "www"), showWarnings = FALSE)
    
  }
  
  wwwFiles <- list.files(path = paste0(spatialUiDir, "/www"), full.names = TRUE)
  
  sapply(wwwFiles, function(x){
        file.copy(from = x, to = file.path(paste0(spatialTmpDir, "/www"), 
                basename(x)), overwrite = TRUE)
      }
  )
  
  
  runApp(appDir = spatialTmpDir)
  
}
