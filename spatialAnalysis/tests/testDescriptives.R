library(testthat)
library(spatialAnalysis)


writeData <- FALSE
dataDir <- system.file("extdata", package = "spatialAnalysis")


context("Descriptives - Jenks style classes")


test_that("Load info data", {
      
      dataCountry <<- read.table(file.path(dataDir, "EE_LAU_2.csv"),
          header = TRUE, sep = ",")
      expect_that(dim(dataCountry), equals(c(213, 17)))
      
      shapeData <<- readShapeData(zipFile = file.path(dataDir, "EE_LAU_2.zip"))
      expect_equal(dim(shapeData), c(213, 13))
      
    })

test_that("Create classes", {
      
      factorResponse <- createClasses(response = dataCountry$RatioCorr,
          breaksStyle = "jenks", nClasses = 5)
      expect_equal(nlevels(factorResponse), 5)
      expect_equivalent(as.numeric(table(factorResponse)), 
          c(53, 110, 29, 17, 4))
      
    })

context("Descriptives - Quantiles")

test_that("Create classes", {
      
      factorResponse <- createClasses(response = dataCountry$RatioCorr,
          breaksStyle = "quantile", nClasses = 5)
      expect_that(nlevels(factorResponse), equals(5))
      expect_that(as.numeric(table(factorResponse)), 
          is_equivalent_to(c(0, 53, 75, 42, 43)))
      
    })


context("Descriptives - Manual input for classes")

test_that("Create classes", {
      
      intervalClosure <- "left"
      
      if(FALSE){
        
        breaksTable <- (classIntervals(var = dataCountry$RatioCorr, n = 5, 
                  style = "fixed", fixedBreaks = c(-9999, 0, 0.1, 0.2, 0.3, 0.75), 
                  intervalClosure = intervalClosure))
        breaksTable
        
      }
      
      factorResponse <- createClasses(response = dataCountry$RatioCorr,
          breaksStyle = "fixed", nClasses = 5, 
          fixedBreaks = c(-9999, 0, 0.1, 0.2, 0.3, 0.75),
          intervalClosure = intervalClosure)
      
      
      table(factorResponse)
      
      expect_that(nlevels(factorResponse), equals(5))
      expect_that(as.numeric(table(factorResponse)), 
          is_equivalent_to(c(53, 131, 15, 8, 6)))
      
    })

context("Descriptives - Local Moran's I")

test_that("Construct local statistics", {
      
      neighboursList <<- readRDS(file = file.path(dataDir, "allNeighbours.rds"))[["LAU2"]]
      
      calculatedLocal <- calculateLocal(response = dataCountry$RatioCorr, 
          neighboursList = neighboursList, localMethod = "moran")
      
      expect_that(signif(apply(calculatedLocal, 2, mean, na.rm = TRUE), 5), 
          equals(c("statistics" = 1.33170, "pValues" = 0.36377)))
      
      factorResponse <- createClasses(response = dataCountry$RatioCorr, 
          breaksStyle = "quadrant", localValues = calculatedLocal)
      
      expect_that(as.numeric(table(factorResponse)), 
          is_equivalent_to(c(66, 92, 22, 29)))
      
    })


context("Descriptives - Local Geary's C")

test_that("Construct local statistics", {
      
      calculatedLocal <- calculateLocal(response = dataCountry$RatioCorr, 
          neighboursList = neighboursList, localMethod = "geary")
      
      expect_that(signif(apply(calculatedLocal, 2, mean, na.rm = TRUE), 5), 
          equals(c("statistics" = 0.4343)))
      
      factorResponse <- createClasses(response = dataCountry$RatioCorr, 
          breaksStyle = "quadrant", localValues = calculatedLocal)
      
      expect_that(as.numeric(table(factorResponse)), 
          is_equivalent_to(c(125, 33, 24, 27)))
      
    })





context("Descriptives - Ordinary Kriging")

test_that("Automated kriging, independent of time", {
      
      krigingResult <- performKriging(response = dataCountry$Ratio, 
          shapeData = shapeData)
      
      gstatVariogram <- krigingResult$gstatVariogram
      variogramModel <<- krigingResult$variogramModel
      krigingPredictions <- krigingResult$krigingPredictions
      
      expect_equal(class(gstatVariogram), c("gstatVariogram", "data.frame"))
      expect_equal(class(variogramModel), c("variogramModel", "data.frame"))
      expect_match(class(krigingPredictions), "SpatialPolygonsDataFrame")
      
      expect_equal(signif(head(krigingPredictions@data$var1.pred), 5), 
          c(0.0010401, 0.0362050, 0.0371390, 0.0229130, 0.0033084, 0.0134580))
      
      expect_equal(signif(mean(krigingPredictions@data$var1.pred - dataCountry$Ratio), 5), 
          0.0012309)
      
      if(FALSE){
        
        plot(dataCountry$Ratio, resultKriging@data$var1.pred)
        abline(a = 0, b = 1)
        
        library(RColorBrewer)
        spplot(krigingPredictions, zcol = "var1.pred", 
            col.regions = brewer.pal(n = 9, name = "Reds"), cuts = 8)
        
        
        plot(krigingResult$variogramResult, krigingResult$variogramModel)
        
      }
      
    })


test_that("Kriging with variogramModel the best of autoKrige, independent of time", {
      
      krigingResult <- performKriging(response = dataCountry$Ratio, 
          shapeData = shapeData, variogramModel = variogramModel)
      
      krigingPredictions <- krigingResult$krigingPredictions
      
      expect_match(class(krigingPredictions), "SpatialPolygonsDataFrame")
      
      expect_equal(signif(head(krigingPredictions@data$var1.pred), 5), 
          c(0.0010032, 0.0361840, 0.0367050, 0.0230930, 0.0031652, 0.0130750))
      
      expect_equal(signif(mean(krigingPredictions@data$var1.pred - dataCountry$Ratio), 5), 
          0.0013158)
      
    })


test_that("Automated Kriging, for each year at LAU2 level", {
      
      mergedData <- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      expect_equal(dim(mergedData), c(12993, 17))
      
      
      extendedData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Year", 
          spatialUnit = "LAU2", inlaVariables = FALSE,
          responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("variable1", "variable2"),
          functionCovariates = c("mean", "mean"))
      
      expect_equal(dim(extendedData), c(213*2, 11))  
      
      krigingResult <- tapply(extendedData$numberPositive, extendedData[, "timeUnit"],
          function(x){
            
            performKriging(response = x, shapeData = shapeData)
            
          })
      
      expect_equal(length(krigingResult), 2)
      
      
    })




test_that("Automated Kriging, for each month at LAU2 level", {
      
      mergedData <- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      extendedData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Month", 
          spatialUnit = "LAU2", inlaVariables = FALSE,
          responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("variable1", "variable2"),
          functionCovariates = c("mean", "mean"))
      
      expect_equal(dim(extendedData), c(213*3, 11))
      
#      library(lattice) #Plots for longitudinal data
#      xyplot(variable1 ~ timeUnit, data = extendedData, group = spatialUnit, 
#          type = 'l')
      
      krigingResult <- tapply(extendedData$numberPositive, extendedData$timeUnit,
          function(x){
            
            performKriging(response = x, shapeData = shapeData)
            
          })
      
      
      expect_equal(length(krigingResult), 3)
      
      if(FALSE){
        
        
        # Print variogram model
        for (i in 1:length(krigingResult)) {
          
          cat(names(krigingResult)[i], "\n")
          print(krigingResult[[i]]$variogramModel)
          
        }
        
        plotResult <- krigingResult[[1]]$krigingPredictions
        
        for (i in 1:length(krigingResult)) {
          
          if(all(is.na(krigingResult[[i]]$krigingPredictions[["var1.pred"]]))){
            
            plotResult[[names(krigingResult)[i]]] <- NULL
            
          } else {
            
            plotResult[[names(krigingResult)[i]]] <- 
                krigingResult[[i]]$krigingPredictions[["var1.pred"]]
            
          }
          
        }   
        
        plotNames <- names(krigingResult)[names(krigingResult) %in% names(plotResult)]
        
        
        variogramList <- lapply(plotNames, function(iName) {
              
              plot(krigingResult[[iName]]$gstatVariogram, 
                  krigingResult[[iName]]$variogramModel,
                  main = iName)
              
            })
        
        
        #Plot variogram
        for (i in 1:length(krigingResult)) {
          
          plot(krigingResult[[i]]$gstatVariogram, krigingResult[[i]]$variogramModel)
          
        }
        
        # Plot map
        spplot(plotResult, 
            zcol = names(krigingResult)[names(krigingResult) %in% names(plotResult)],
            as.table = TRUE, colorkey = list(space = "right"))
        
        
      }
      
    })



