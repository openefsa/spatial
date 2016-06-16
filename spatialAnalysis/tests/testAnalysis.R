library(testthat)
library(spatialAnalysis)
library(spdep)


context("Perform INLA spatial analysis")

dataDir <- system.file("extdata", package = "spatialAnalysis")

test_that("Load merged data", {
      
      mergedData <<- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      expect_equal(dim(mergedData), c(12993, 17))
      
      shapeData <<- readShapeData(zipFile = file.path(dataDir, "allPolygons.zip"),
          id = "SP_ID")
      expect_equal(dim(shapeData), c(234, 5))
      
    })


test_that("Make space time data at time level Year and spatial level LAU2", {
      
      spaceTimeData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Year", 
          spatialUnit = "LAU2", responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("variable1", "variable2"),
          functionCovariates = c("mean", "mean"))
      
      expect_equal(length(unique(spaceTimeData$spatialUnit))*
              length(unique(spaceTimeData$timeUnit)), nrow(spaceTimeData))
      expect_equal(length(unique(spaceTimeData$spatialTimeUnit)), nrow(spaceTimeData))
      
    })


test_that("Save all neighbours for use with inla()", {
      
      neighboursList <- readRDS(file = file.path(dataDir, "allNeighbours.rds"))
      expect_equal(names(neighboursList), c("Country", "NUTS3", "LAU1", "LAU2"))
      
      lapply(names(neighboursList), function(iName) {
            
            if(class(neighboursList[[iName]]) == "nb"){
              
              saveNeighboursInla(neighboursList = neighboursList[[iName]], 
                  weightStyle = "B", fileName = iName)
              
            }
            
            return(NULL)
            
          })
      
    })


test_that("NUTS3 - year analysis, saturated time effect", {
      
      spaceTimeData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Year", 
          spatialUnit = "NUTS3", responsePositive = "numberPositive",
          responseTotal = "numberObservations")
      
      formulaInla <- makeFormulaInla(responseName = "isPositive", 
          timeEffect = "saturated", interactionEffect = "none")
      
      inlaResult <- performInla(spaceTimeData = spaceTimeData, 
          formulaInla = formulaInla, 
          neighboursFileName = "NUTS3")
      #summary(inlaResult)
      
      expect_equal(signif(inlaResult$summary.fixed[,1], 3), 
          c(-3.32, 47.60))
      
    })


test_that("All space-time levels: linear time effect, no covariates", {
      
      iTime <- c("Year", "Month", "Week", "Day")[1]
      iSpace <- c("NUTS3", "LAU1", "LAU2")[1]
      
      
      spaceTimeData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = iTime, 
          spatialUnit = iSpace, responsePositive = "numberPositive",
          responseTotal = "numberObservations")
      
      expect_equal(dim(spaceTimeData), 
          c(length(unique(spaceTimeData$spatialUnit))*
                  length(unique(spaceTimeData$timeUnit)), 14))
      
      formulaInla <- makeFormulaInla(responseName = "isPositive", 
          timeEffect = "linear", interactionEffect = "none")
      
      inlaResult <- performInla(spaceTimeData = spaceTimeData, 
          formulaInla = formulaInla, 
          neighboursFileName = iSpace)
      
      
      if(FALSE){
        
        # Time plot
        plotData <- cbind(spaceTimeData[, c("spatialUnit", "timeUnit", "dates")], 
            inlaResult$summary.fitted.values)
        
        ggplot(data = plotData, aes(x = dates, y = mean, group = spatialUnit)) + 
            ylim(0, 1) + geom_line() + theme_bw()
        
        # Spatial plot
        plotDataWide <- reshape(plotData[, c("timeUnit", "spatialUnit", "mean")],
            timevar = "timeUnit", idvar = "spatialUnit", direction = "wide")
        
        if(iTime %in% c("Year", "Month")){
          
          timePoints <- levels(plotData$timeUnit)
          
        } else {
          
          timePoints <- paste0(iTime, "_", levels(plotData$timeUnit))
          
        }
        
        colnames(plotDataWide) <- c("spatialUnit", timePoints)
        rownames(plotDataWide) <- plotDataWide$spatialUnit
        
        subsetPolygons <- selectPolygons(allPolygons = shapeData, spatialLevel = iSpace)
        plotResult <- SpatialPolygonsDataFrame(Sr = subsetPolygons, data = as.data.frame(plotDataWide))
        
        spplot(plotResult, zcol = timePoints[1:2], as.table = TRUE, 
            col.regions = brewer.pal(n = 9, name = "Reds"),
            cuts = 8)
                
      }      
      
    })


test_that("NUTS3 - month, fit all time effect models", {
      
      
      spaceTimeData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Month", 
          spatialUnit = "NUTS3", responsePositive = "numberPositive",
          responseTotal = "numberObservations")
      
      formulaInla <- makeFormulaInla(responseName = "isPositive", 
          timeEffect = "all")
      
      inlaResult <- performInla(spaceTimeData = spaceTimeData, 
          formulaInla = formulaInla,
          neighboursFileName = "LAU2")
      
    })



test_that("LAU2 - month analysis linear time effect, type I interaction & 2 covariates", {
      
      spaceTimeData <<- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Month", 
          spatialUnit = "LAU2", responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("variable1", "variable2"),
          functionCovariates = c("mean", "mean"))
      
      formulaInla <- makeFormulaInla(responseName = "isPositive", 
          covariateNames = c("variable1", "variable2"), 
          timeEffect = "linear", interactionEffect = "type1")
      
      inlaResult <<- performInla(spaceTimeData = spaceTimeData, 
          formulaInla = formulaInla,
          neighboursFileName = "LAU2")
      
      expect_equal(signif(inlaResult$summary.fixed[,1], 3), 
          c(-1.5600, -0.0454, -0.0520,  0.6050))
      
    })



if(FALSE){
  
context("Make summary plots for INLA analysis")

  test_that("Highchart for prediction over time", {
        
        plotData <- cbind(spaceTimeData[, c("spatialUnit", "timeUnit")], 
            inlaResult$summary.fitted.values)
        
        timePoints <- levels(plotData$timeUnit)
        
        library(rCharts)
        plotPredictions <- Highcharts$new() 
        plotPredictions$xAxis(categories = timePoints)
        plotPredictions$yAxis(min = 0, max = 1)
        
        selectedAreas <- head(plotData$spatialUnit)
        
        plotData$y <- plotData$mean
        
        plotPredictions$series(
            lapply(selectedAreas, function(iArea) {
                  
                  list(name = iArea, data = 
                          toJSONArray2(plotData[plotData$spatialUnit == iArea, ], json = F))
                  
                })
        )
        
        variablesToShow <- c("sd", "mode")
        textTooltip <- paste("<b> {point.spatialUnit} </b> <br> <br>",
            paste0(variablesToShow, ": {point.", variablesToShow, "}", collapse = "<br>")) 
        plotPredictions$tooltip(useHTML = TRUE, pointFormat = textTooltip)
        
        plotPredictions
        
        
        subsetPolygons <- selectPolygons(allPolygons = shapeData, spatialLevel = "LAU2")
        
        plotDataWide <- reshape(plotData[, c("timeUnit", "spatialUnit", "mean")],
            timevar = "timeUnit", idvar = "spatialUnit", direction = "wide")
        
        colnames(plotDataWide) <- c("spatialUnit", timePoints)
        rownames(plotDataWide) <- plotDataWide$spatialUnit
        plotResult <- SpatialPolygonsDataFrame(Sr = subsetPolygons, data = plotDataWide)
        
        library(RColorBrewer)
          
          if(length(levels(spaceTimeData$timeUnit)) > 1){
            
            spplot(plotResult, zcol = levels(spaceTimeData$timeUnit), as.table = TRUE, 
                col.regions = brewer.pal(n = 9, name = "Reds"),
                cuts = 8)
            
          } else {
            
            spplot(plotResult, zcol = levels(spaceTimeData$timeUnit),
                col.regions = brewer.pal(n = 9, name = "Reds"),
                cuts = 8)
            
          }
          
       ggplot(data = plotData, aes(x = timeUnit, y = mean, group = spatialUnit)) + 
            ylim(0, 1) + geom_line() + theme_bw()
        
        
      })
      

test_that("Leaflet interactive map plot for Jan_2016", {
      
      plotDataList <- split(plotData, plotData$timeUnit)
      expect_equal(names(plotDataList), c("Dec_2015", "Jan_2016", "Feb_2016"))
      
      library(RColorBrewer)
      palette <- colorNumeric(palette = "Reds", 
          domain = range(plotDataList[["Jan_2016"]]$mean))
      
      
      map <- leaflet(subsetPolygons) %>%
          
          addPolygons(
              weight = 1, 
              color = "white",
              fillColor = ~ palette(plotDataList[["Jan_2016"]]$mean),
              fillOpacity = 0.8,
              layerId = rownames(plotDataList[["Jan_2016"]]),
              group = "region"
          ) 
      
      map
      
    })
  
  test_that("Country - year analysis with saturated model - AMR data", {
        
        #Run AMR app, after closing following object is available
        tmpDir <- tempdir()
        load(paste0(tmpDir, "/inlaData.Rdata")) # object data_use_spatial
        
        saveRDS(data_use_spatial, file = "inlaData.rds")
#        data_use_spatial <- readRDS(file = "inlaData.rds")
        
        unzip("~/git/spatial/amrApp/NUTS_2013_SHP.zip")
        shapeData <- readShapePoly("NUTS_2013_SHP/data/NUTS_RG_01M_2013.shp")
        
        formulaInla <- makeFormulaInla(responseName = "CIP.res", covariateNames = NULL,
#            timeEffect = "saturated", interactionEffect = "none")
            timeEffect = "linear", interactionEffect = "none")
        
#      Copy in file "test.adj"
#      3
#      1 1 3
#      2 1 3
#      3 2 1 2
#        names(data_use_spatial)
        data_use_spatial$timeId <- data_use_spatial$t.ID
        data_use_spatial$spatialId <- data_use_spatial$s.ID
        
        inlaResult <- performInla(spaceTimeData = data_use_spatial, 
            formulaInla = formulaInla,
            neighboursFileName = "test")
        
        head(expit(inlaResult$summary.fixed[1, "mean"] + 
                    data_use_spatial$timeId*inlaResult$summary.fixed[2, "mean"] +
                    inlaResult$summary.random$spatialId[data_use_spatial$spatialId, "mean"]))
        head(inlaResult$summary.fitted.values$mean)
        
        plotData <- cbind(data_use_spatial[, c("spatialId", "timeId")], 
            inlaResult$summary.fitted.values)
        
        ggplot(data = plotData, aes(x = timeId, y = mean, group = spatialId)) + 
            ylim(0, 1) + geom_line() + theme_bw()
        
        expect_equal(signif(inlaResult$summary.fixed[,1], 5), 
            c(-0.13312, 0.19865, 0.76962, 0.36182, 1.0119, 2.2702e-06))
        
      })
  
}
