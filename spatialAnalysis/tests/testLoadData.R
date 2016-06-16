library(testthat)
library(spatialAnalysis)


writeData <- FALSE
dataDir <- system.file("extdata", package = "spatialAnalysis")


context("Data manipulation: Load data")

test_that("Load info and shape data", {
      
      infoData <<- read.table(file.path(dataDir, "rawDataUnique.csv"),
          header = TRUE, sep = ",")
      expect_equal(dim(infoData), c(12744, 33))
      
      shapeData <<- readShapeData(zipFile = file.path(dataDir, "EE_LAU_2.zip"))
      expect_equal(dim(shapeData), c(213, 13))
      
      responseName <<- "resQualValue" 
      infoData[,responseName] <<- ifelse(infoData[, responseName] == "POS", 1, 0)
      
    })


context("Data manipulation: Shape data")

if(FALSE){ # Takes long time
  
  test_that("Create polygons at each spatial level",{
        
        allPolygons <- makeUnionPolygons(shapeData = shapeData, 
            spatialNames = c("Country", "NUTS3", "LAU1Code", "LAU2Code"),
            covariates = c("LAU1Name", "LAU2Name"), 
            functionCovariates = c("modus", "modus"))
        
        expect_equal(length(allPolygons), 234)
        expect_match(class(allPolygons), "SpatialPolygonsDataFrame")
        
        if(writeData){
          
          writePolyShape(allPolygons, "allPolygons")
          zip(zipfile = "allPolygons.zip", files = Sys.glob("allPolygons.*"))
          
        }
        
      })
  
}





test_that("Read union polygons data and select NUTS3 subset", {
      
      readAllPolygons <<- readShapeData(zipFile = file.path(dataDir, "allPolygons.zip"),
          id = "SP_ID")
      
      expect_equal(length(readAllPolygons), 234)
      rownames(readAllPolygons@data) <- sapply(readAllPolygons@polygons, function(x) slot(x, "ID"))
      
      
      subsetPolygons <- selectPolygons(allPolygons = readAllPolygons, 
          spatialLevel = "NUTS3")
      expect_equal(length(subsetPolygons), 5)
      expect_match(class(subsetPolygons), "SpatialPolygonsDataFrame")
      
#      plot(subsetPolygons)
      
    })


test_that("Select subset of all polygons by ID", {
      
      shapeNames <- row.names(readAllPolygons)
      
      select <- c("EE001", "E004")
      doSelect <- grepl(paste(select, collapse = "|"), shapeNames)
      
      subsetPolygons <- selectPolygons(readAllPolygons, doSelect = doSelect)
      
      allPolygonsLAU2 <- selectPolygons(readAllPolygons, spatialLevel = "LAU2")
      subsetPolygonsLAU2 <- selectPolygons(subsetPolygons, spatialLevel = "LAU2")
      
#      plot(allPolygonsLAU2)
#      plot(subsetPolygonsLAU2, col = "gray", add = TRUE)
      
      expect_equal(length(subsetPolygonsLAU2), 70)
      
    })


test_that("Create spatial data", {
      
      allNames <- rownames(readAllPolygons@data)
      lowestNames <- allNames[nchar(allNames) == max(nchar(allNames))]
      
      allData <- readAllPolygons@data[lowestNames,]
      allData$LAU2 <- lowestNames
      
      spatialData <<- cbind(sapply(c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1"), function(x){
                
                switch(x,
                    "NUTS0" = substring(lowestNames, first = 1, last = 2),
                    "NUTS1" = substring(lowestNames, first = 1, last = 3),
                    "NUTS2" = substring(lowestNames, first = 1, last = 4),
                    "NUTS3" = substring(lowestNames, first = 1, last = 5),
                    "LAU1" = substring(lowestNames, first = 1, last = 9)
                )
              }), 
          allData[, c("LAU2", "LAU1Name", "LAU2Name"), drop = FALSE])
      
      expect_equal(dim(spatialData), c(213, 8))
      
    })


context("Data manipulation: Define neighbours")


if(FALSE){ # Takes long time
  
  test_that("Calculate neighbours for all spatial levels and define new neighbours", {
        
        allNeighbours <<- c(NA, lapply(c("NUTS3", "LAU1", "LAU2"), function(iLevel) {
                  
                  subsetPolygons <- selectPolygons(allPolygons = readAllPolygons, 
                      spatialLevel = iLevel)
                  
                  poly2nb(subsetPolygons)
                  
                })
        )
        
        names(allNeighbours) <<- c("Country", "NUTS3", "LAU1", "LAU2")
        
        expect_equal(length(allNeighbours), 4)
        expect_equal(class(allNeighbours[[4]]), "nb")
        
        if(writeData){
          
          saveRDS(allNeighbours, file = "allNeighbours.rds")
          
        }
        
      })
  
}


test_that("Calculate distance between neighbours and define new neigbours", {
      
      allNeighbours <- readRDS(file = file.path(dataDir, "allNeighbours.rds"))
      
      subsetPolygons <- selectPolygons(allPolygons = readAllPolygons, 
          spatialLevel = "NUTS3")
      
      
      # Plot neighbor linkages                  
#      plot(subsetPolygons, border = "grey") 
#      plot(allNeighbours[["NUTS3"]], coordinates(subsetPolygons), add = TRUE)  
      
#     http://gis.stackexchange.com/questions/83722/calculate-minimal-distance-between-polygons-in-r
      maxDistance <- 100
      library(spdep)
      neighboursMax <- dnearneigh(coordinates(subsetPolygons), 0, 
          maxDistance, row.names = rownames(subsetPolygons@data), longlat = TRUE)
      
      neighboursMatrix <- nb2mat(neighboursMax, style = "B", zero.policy = TRUE)
      colnames(neighboursMatrix) <- rownames(neighboursMatrix)
      
      regions <- c("EE006", "EE007")
      neighboursMatrix["EE004", regions] <- 1
      neighboursMatrix[regions, "EE004"] <- 1
      
      allNeighbours[["NUTS3"]] <- mat2listw(neighboursMatrix)$neighbours
      
      # Plot neighbor linkages                  
#      plot(subsetPolygons, border = "grey") 
#      plot(allNeighbours[["NUTS3"]], coordinates(subsetPolygons), add = TRUE)  
      
      
    })


context("Data manipulation: Data for analysis")

test_that("Handle temporal data", {
      
      dataSelected <- infoData[, c(responseName, "sampY", "sampM", "sampD",
              "sampLAU2")]
      
      # Collapse data by lowest space and time levels
      dataCollapsed <<- collapseData(inputData = dataSelected, 
          collapseVariable = c("sampY", "sampM", "sampD", "sampLAU2"), 
          responsePositive = responseName,
          covariates = NULL,
          functionCovariates = NULL)
      expect_equal(dim(dataCollapsed), c(1970, 8))
      
      
    })



test_that("Covariates data at day level", {
      
      # Generate random covariates data
      spatialNames <- spatialData[, "LAU2"]
      timeNames <- rbind(c(sampY = 2016, sampM = 1, sampD = 1),
          unique(infoData[c("sampY", "sampM", "sampD")]))
      covariatesData <- c()
      
      for(iSpatial in spatialNames){
        
        covariatesData <- rbind(covariatesData, cbind(LAU2 = iSpatial, timeNames))
        
      }
      
      set.seed(1)
      covariatesData <- cbind(covariatesData, 
          variable1 = rnorm(nrow(covariatesData)),
          variable2 = rnorm(nrow(covariatesData), mean = 20, sd = 5))
      
      expect_equal(dim(covariatesData), c(nrow(timeNames)*length(spatialNames), 6))
      
      if(writeData){
        
        write.csv(covariatesData, "covariatesData.csv", row.names = FALSE)
        
      }
      
      # Bind with dataCollapsed
      mergedData <<- merge(x = covariatesData, y = dataCollapsed,
          by.x = c("LAU2", "sampY", "sampM", "sampD"),
          by.y = c("sampLAU2", "sampY", "sampM", "sampD"),
          all.x = TRUE, all.y = TRUE)    
      
      expect_equal(dim(mergedData), c(12993, 10))
      
#      # For missing covariates impute the mean of LAU2 region
#      isMissing <- which(is.na(allData$sampD))
#      
#      allMeans <- summaryBy(variable1 + variable2 ~ LAU2, FUN = mean,
#          data = covariatesData)
#      rownames(allMeans) <- allMeans$LAU2
#      names(allMeans)[2:3] <- c("variable1", "variable2")
#      allData[isMissing, c("variable1", "variable2")] <- 
#          allMeans[allData$LAU2[isMissing], c("variable1", "variable2")]
      
      
      expect_equal(signif(mean(mergedData$variable1[1:200]), 3), 0.0355)
      expect_equal(signif(mean(mergedData$variable2[1:200]), 3), 20)
      
    })


test_that("Merge temporal and spatial data", {
      
      # Merge data with shape data
      allData <<- merge(x = spatialData, y = mergedData,
          all.x = TRUE, by.x = "LAU2", by.y = "LAU2")
      
      
      allData$numberObservations[is.na(allData$numberObservations)] <- 0
      
      expect_equal(sum(allData$numberObservations), nrow(infoData))
      expect_equal(dim(allData), c(12993, 17)) 
      
      
      # Order columns
      names(allData)[names(allData) %in% c("sampY", "sampM", "sampD")] <- 
          c("Year", "Month", "Day")
      
      allNames <- names(allData)
      covariateNames <- c("LAU1Name", "LAU2Name", "variable1", "variable2")
      firstNames <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2", "Year", "Month", "Day")
      toReturn <- allData[, c(firstNames, allNames[!(allNames %in% c(firstNames, covariateNames))],
              covariateNames)]
      
      # Unique LAU2 and LAU1 names
      expect_equal(all(apply(xtabs(~ allData$LAU2 + allData$LAU2Name), 2, 
                  function(x) sum(x==0)) == 212), TRUE)
      expect_equal(all(apply(xtabs(~ allData$LAU2 + allData$LAU2Name), 1, 
                  function(x) sum(x==0)) == 212), TRUE)
      
      expect_equal(all(apply(xtabs(~ allData$LAU1 + allData$LAU1Name), 2, 
                  function(x) sum(x==0)) == 14), TRUE)
      expect_equal(all(apply(xtabs(~ allData$LAU1 + allData$LAU1Name), 1, 
                  function(x) sum(x==0)) == 14), TRUE)
      
      
      if(writeData){
        
        write.csv(toReturn, "mergedData.csv", row.names = FALSE)
        
      }
      
    })



test_that("Merge all data - LAU2 and Day", {
      
      covariatesData <<- read.csv(file.path(dataDir, "covariatesData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      mergedData <- mergeAllData(temporalData = infoData, 
              spatialData = spatialData, covariatesData = covariatesData,
              responseVariable = responseName, timeResolution = "Day", 
              dateFormat = NA, spaceResolution = "LAU2", 
              timeNamesTemporal = c("sampY", "sampM", "sampD"), 
              spatialNameTemporal = "sampLAU2",
              covariatesTemporal = NULL, functionCovariatesTemporal = NULL,
              timeNamesExtra = c("sampY", "sampM", "sampD"), 
              spatialNameExtra = "LAU2", 
              covariatesExtra = c("variable1", "variable2"), 
              functionCovariatesExtra = c("mean", "mean"), 
              lagTime = 0, 
              covariatesSpatial = c("LAU1Name", "LAU2Name"))
      
      expect_equal(dim(mergedData), c(213*61, 17))
      
      expect_equal(sum(mergedData$numberObservations), nrow(infoData))
      
    })



context("Data manipulation: other functionalities")


test_that("Collapse data with numeric and character covariates", {
      
      mergedData <- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      inputData <- mergedData[, c("numberPositive", "numberObservations", 
              "Year", "Month", "LAU2", "LAU1Name", "LAU2Name", "variable1", 
              "variable2")]
      
      dataCollapsed <- collapseData(inputData = inputData, 
          collapseVariable = c("Year", "Month", "LAU2"), responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("LAU1Name", "LAU2Name", "variable1", "variable2"), 
          functionCovariates = c("modus", "modus", "mean", "mean"))
      
      extendedData <- makeSpaceTimeData(mergedData = mergedData, 
          timeNames = c("Year", "Month", "Day"), timeUnit = "Month", 
          spatialUnit = "LAU2", inlaVariables = TRUE,
          responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("LAU1Name", "LAU2Name", "variable1", "variable2"),
          functionCovariates = c("modus", "modus", "mean", "mean"))
      
    })


test_that("Covariates data with lag time", {
      
      covariatesData <- read.csv(file.path(dataDir, "covariatesData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      # Create dates for info and covariates data
      covariatesData$date <- makeDates(inputData = covariatesData, 
          timeNames = c("sampY", "sampM"))
      
      library(lubridate)
      
      expect_equal(year(covariatesData$date), covariatesData$sampY)
      expect_equal(month(covariatesData$date), covariatesData$sampM)
      expect_equal(all(day(covariatesData$date) == 1), TRUE) 
      
      covariatesData$date <- makeDates(inputData = covariatesData, 
          timeNames = c("sampY", "sampM", "sampD"))
      
      expect_equal(year(covariatesData$date), covariatesData$sampY)
      expect_equal(month(covariatesData$date), covariatesData$sampM)
      expect_equal(day(covariatesData$date), covariatesData$sampD)
      
      dataCollapsed$date <- makeDates(inputData = dataCollapsed, 
          timeNames = c("sampY", "sampM", "sampD"))
      
      
      # Add 2 years
      expect_equal(all(difftime(makeLagDates(dates = covariatesData$date,
                          lagTime = 2, timeUnit = "Year"), covariatesData$date) >= 
                  730), TRUE)
      
      # Add 1 month
      expect_equal(all(difftime(makeLagDates(dates = covariatesData$date,
                          lagTime = 1, timeUnit = "Month"), covariatesData$date) >= 
                  29), TRUE)
      
      # Add 5 days
      laggedData <- covariatesData
      laggedData$date <- makeLagDates(dates = laggedData$date, lagTime = 5, 
          timeUnit = "Day")
      
      expect_equal(all(difftime(laggedData$date, covariatesData$date) == 5), TRUE)
      
      
      
      # Bind with dataCollapsed (lag time 5 days)
      newData <- merge(x = dataCollapsed, y = laggedData,
          by.x = c("sampLAU2", "date"),
          by.y = c("LAU2", "date"),
          all.x = TRUE, all.y = TRUE)    
      
      expect_equal(dim(newData), c(13053, 14))
      
      
      # Read dates
      timeData <- readDates(dates = newData$date, dateFormat = "%Y-%m-%d", timeUnit = "Day")
      
      library(zoo)
      mergedTimeData <- merge.zoo(x = newData, y = timeData)
      
      expect_equal(dim(mergedTimeData), c(13053, 18))
      
    })


test_that("Create dates and back to year, month day", {
      
      tmpData <- makeDates(inputData = infoData, 
          timeNames = c("sampY", "sampM", "sampD"))
      
      if(writeData){
        
        write.csv(tmpData, "rawDataUnique_Date.csv", row.names = FALSE)
        
      }
      
      
      tmpData <- makeDates(inputData = covariatesData, 
          timeNames = c("Year", "Month", "Day")) 
      
      if(writeData){
        
        write.csv(tmpData, "covariatesData_Date.csv", row.names = FALSE)
        
      }            
      
      
      testData <- tmpData
      testData$Date <- strptime(testData$Date, "%Y/%m/%d")
      testData$Year <-  as.numeric(strftime(testData$Date, format = "%Y"))
      testData$Month <-  as.numeric(strftime(testData$Date, format = "%m"))
      testData$Day <-  as.numeric(strftime(testData$Date, format = "%d"))
      
      expect_equal(testData$Year, tmpData$sampY)
      expect_equal(testData$Month, tmpData$sampM)
      expect_equal(testData$Day, tmpData$sampD)
      
    })


test_that("Merge all data - LAU2 and Date", {
      
      infoDataDate <- read.csv("~/git/spatial/spatialData/rawDataUnique_Date.csv", 
          header = TRUE, stringsAsFactors = FALSE)
      
      covariatesDataDate <- read.csv("~/git/spatial/spatialData/covariatesData_Date.csv", 
          header = TRUE, stringsAsFactors = FALSE)
      
      mergedData <- mergeAllData(temporalData = infoDataDate, 
          spatialData = spatialData, covariatesData = covariatesDataDate,
          responseVariable = responseName, timeResolution = "Date", 
          dateFormat = NA, spaceResolution = "LAU2", 
          timeNamesTemporal = "Date", 
          spatialNameTemporal = "sampLAU2",
          covariatesTemporal = NULL, functionCovariatesTemporal = NULL,
          timeNamesExtra = "Date", spatialNameExtra = "LAU2", 
          covariatesExtra = c("variable1", "variable2"), 
          functionCovariatesExtra = c("mean", "mean"), 
          lagTime = 0, 
          covariatesSpatial = c("LAU1Name", "LAU2Name"))
      
      expect_equal(dim(mergedData), c(213*61, 17))
      
      expect_equal(sum(mergedData$numberObservations), nrow(infoDataDate))
      
    })



test_that("Collapse data by LAU1 and Month", {
      
      allData <- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      inputData <- allData[, !(names(allData) %in% c("Day", "LAU2"))]
      inputData$sampId <- NULL
      
      collapsedData <- collapseData(inputData = inputData, 
          collapseVariable = c("LAU1", "Year", "Month"),
          responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = c("variable1", "variable2", "LAU1Name", "LAU2Name"),
          functionCovariates = c("mean", "mean", "modus", "modus"))      
      
      expect_equal(dim(collapsedData), c(45, 15))
      
    })

test_that("Collapse data by relative frequencies", {
      
      dataSelected <- infoData[, c(responseName, "sampY", "sampM", "sampD",
              "sampLAU2", "sex", "age")]
      
      dataSelected$age <- as.character(dataSelected$age)
      dataSelected$sex <- as.character(dataSelected$sex)
      
      
      # Collapse data by lowest space and time levels
      dataCollapsed <- collapseData(inputData = dataSelected, 
          collapseVariable = c("sampY", "sampM", "sampD", "sampLAU2"), 
          responsePositive = responseName,
          responseTotal= NA,
          covariates = c("sex", "age"),
          functionCovariates = c("modus", "relativeFrequency"))
      
      expect_equal(dim(dataCollapsed), c(1970, 12))
      
      
    })




test_that("Merge all data - LAU2 and Day with relFreq summary function", {
      
      mergedData <- mergeAllData(temporalData = infoData, 
          spatialData = spatialData, covariatesData = covariatesData,
          responseVariable = responseName, timeResolution = "Day", 
          dateFormat = NA, spaceResolution = "LAU2", 
          timeNamesTemporal = c("sampY", "sampM", "sampD"), 
          spatialNameTemporal = "sampLAU2",
          covariatesTemporal = c("sex", "age"), 
          functionCovariatesTemporal = c("modus", "relativeFrequency"),
          timeNamesExtra = c("sampY", "sampM", "sampD"), 
          spatialNameExtra = "LAU2", 
          covariatesExtra = c("variable1", "variable2"), 
          functionCovariatesExtra = c("mean", "mean"), 
          lagTime = 0, 
          covariatesSpatial = c("LAU1Name", "LAU2Name"))
      
      expect_equal(dim(mergedData), c(213*61, 21))
      
      expect_equal(sum(mergedData$numberObservations), nrow(infoData))
      
    })



test_that("Give error if data cannot be collapsed", {
      
      inputData <- infoData[, c("sampCountry", "resQualValue")]
      
      expect_error(collapseData(inputData = inputData, 
              collapseVariable = "sampCountry", 
              responsePositive = "resQualValue"), 
          "The data cannot be collapsed: The collapsed data size will be 1")
      
    })


context("Data manipulation: space-time data")

test_that("Make space-time data for each spatial and temporal level", {
      
      mergedData <- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      suppressWarning( # Expect warning for Day - LAU2
          
      for (iTime in c("Year", "Month", "Week", "Day")) {
        
        for (iSpace in c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2")){
          
          extendedData <- makeSpaceTimeData(mergedData = mergedData, 
              timeNames = c("Year", "Month", "Day"), timeUnit = iTime, 
              spatialUnit = iSpace, inlaVariables = FALSE,
              responsePositive = "numberPositive",
              responseTotal = "numberObservations",
              covariates = c("variable1", "variable2"),
              functionCovariates = c("mean", "mean"))
          
          expect_equal(dim(extendedData), 
              c(length(unique(extendedData$spatialUnit))*
                      length(unique(extendedData$timeUnit)), 11))
          
        }
      }
  
  )
      
      
    })



test_that("Make space-time data when day is missing", {
      
      mergedData <- read.csv(file.path(dataDir, "mergedData.csv"), 
          header = TRUE, stringsAsFactors = FALSE)
      
      mergedData$Day <- NULL
      
      for (iTime in c("Year", "Month")) {
        
        for (iSpace in c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2")){
          
          extendedData <- makeSpaceTimeData(mergedData = mergedData, 
              timeNames = c("Year", "Month"), timeUnit = iTime, 
              spatialUnit = iSpace, inlaVariables = FALSE,
              responsePositive = "numberPositive",
              responseTotal = "numberObservations",
              covariates = c("variable1", "variable2"),
              functionCovariates = c("mean", "mean"))
          
          expect_equal(dim(extendedData), 
              c(length(unique(extendedData$spatialUnit))*
                      length(unique(extendedData$timeUnit)), 11))
          
        }
      }
      
    })


# Data for workshop

if(FALSE){
  
  shapeWorkshop <- readShapeData(zipFile = "~/git/spatial/spatialData/shapeWorkshop.zip")
  covariatesWorkshop <- read.csv(file = "~/git/spatial/spatialData/covariatesWorkshop.csv",
      header = TRUE)
  
  
  data1 <- read.csv(file.path(dataDir, "EE2014_1.csv"), header = TRUE)
  data2 <- read.csv(file.path(dataDir, "EE2016_4.csv"), header = TRUE)
  
  estonia <- rbind(data1[,-c(32,33)], data2[,-32])
  if(writeData){
    
    write.csv(estonia, "estonia.csv", row.names = FALSE)
    
  }
  
  selectedData <- estonia[,c(responseName, "sampY", "sampM", "sampD",
          "sampLAU2", "sampId")]
  selectedData[, responseName] <- ifelse(selectedData[,responseName] == "POS", 1, 0)
  collapsedData <- collapseData(inputData = selectedData, collapseVariable = "sampId", 
      responsePositive = responseName)
  
}
