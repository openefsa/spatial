
#' Read shape data from zipped file
#' @param zipFile the pathname of the zip file 
#' @param id character string, the name of a column in the shapefile .dbf 
#' containing the ID values to be used; default value is NULL
#' @return a SpatialPolygonsDataFrame object, with polygons and data as provided
#' in the zipFile
#' @importFrom maptools readShapePoly
#' @export
readShapeData <- function(zipFile, id = NULL){
  
  tmpDir <- tempdir()
  unlink(file.path(tmpDir, "*.shp"))
  unzip(zipFile, exdir = tmpDir)
  allShapeFiles <- list.files(path = tmpDir, pattern = ".shp", 
      full.names = TRUE)
  shapeData <- readShapePoly(allShapeFiles, IDvar = id)
  
  
  return(shapeData)
  
}


#' Create all union polygons of the shapeData at the given spatial levels
#' @param shapeData SpatialPolygonsDataFrame object, containing polygons at smallest
#' spatial level and data frame with polygon names at different spatial levels
#' @param spatialNames character vector, names in data of shapeData that 
#' contain ID at the different spatial levels used for taking the union; ordered
#' from largest to smallest spatial level; "none" if undefined
#' @param covariates character vector, variable names in shapeData data object
#' of the covariates that should be summarized when collapsing the data
#' @param functionCovariates character vector, the function for summarizing each
#' of the covariates; each one of \code{c("mean", "max", "min", "modus")};
#' default is NULL
#' @return SpatialPolygonsDataFrame object with union of shapeData polygons at
#' each of the levels defined by spatialNames; data contain x and y coordinates
#' @importFrom sp SpatialPolygons spChFIDs
#' @importFrom maptools unionSpatialPolygons spRbind
#' @importFrom plyr ddply
#' @export
makeUnionPolygons <- function(shapeData, spatialNames, covariates = NULL,
    functionCovariates){
  
  allPolygons <- SpatialPolygons(shapeData@polygons)
  allPolygons <- spChFIDs(obj = allPolygons, 
      x = as.character(shapeData@data[, spatialNames[length(spatialNames)]]))
  
  if (is.null(covariates)) {
    
    covariatesData <- NULL
    
  } else {
    
    if (length(functionCovariates) != length(covariates)) {
      
      stop("The length of functionCovariates should be the same as for covariates")
      
    }
    
    lowestCovariatesData <- shapeData@data[, covariates, drop = FALSE]
    lowestCovariatesData <- data.frame(
        sapply(lowestCovariatesData, function(x) {
              
              if (is.factor(x)) {
                
                as.character(x)
                
              } else {
                
                x
                
              }
              
            }), stringsAsFactors = FALSE)
    
    names(lowestCovariatesData) <- covariates
    covariatesData <- lowestCovariatesData
    
  }
  
  
  for(iLevel in rev(spatialNames[-length(spatialNames)])) {
    
    if(iLevel != "none") {
      
      IDs <- shapeData@data[, iLevel]    
      unionPolygons <- unionSpatialPolygons(SpP = shapeData, IDs = IDs)
      allPolygons <- spRbind(allPolygons, unionPolygons)
      
      if(!is.null(covariates)){
        
        collapsedCovariates <- data.frame(
            sapply(seq_along(covariates), function(iCovariate) { 
                  
                  tapply(lowestCovariatesData[, iCovariate], IDs, function(data){
                        
                        do.call(functionCovariates[iCovariate], 
                            list(x = data))
                        
                      })
                  
                }, simplify = FALSE))
        
        colnames(collapsedCovariates) <- covariates
        
        covariatesData <- rbind(covariatesData, collapsedCovariates)
        
      }
      
    }
    
  }
  
  coordinatesData <- coordinates(allPolygons)
  rownames(coordinatesData) <- NULL
  colnames(coordinatesData) <- c("xCenter", "yCenter")
  
  polygonsData <- cbind(coordinatesData, covariatesData)
  
  rownames(polygonsData) <- sapply(allPolygons@polygons, function(x) slot(x, "ID"))
  
  allPolygonsData <- SpatialPolygonsDataFrame(Sr = allPolygons, 
      data = polygonsData)
  
  
  return(allPolygonsData)
  
}


#' Select subset of polygons for the given spatial level
#' @param allPolygons SpatialPolygonsDataFrame object as returned from 
#' makeUnionPolygons(); contains polygons at different spatial levels
#' @param spatialLevel character string, indicates the spatial level at which
#' all polygons should be selected; one of c("NUTS0", "NUTS1", "NUTS2", "NUTS3", 
#' "LAU1", "LAU2"); ignored if doSelect is not NULL; default value is "NUTS0"
#' @param doSelect boolean vector with length the number of polygons in allPolygons, 
#' indicates whether to select the polygon; if NULL, spatial level should be 
#' defined; default value is NULL 
#' @return SpatialPolygonsDataFrame object, which is subset of allPolygons 
#' containing only polygons at the specified spatial level
#' @export
selectPolygons <- function(allPolygons, 
    spatialLevel = c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2"),
    doSelect = NULL) {
  
  
  if(is.null(doSelect)){
    
    spatialLevel <- match.arg(spatialLevel)
    
    nCharacters <- switch(spatialLevel,
        "NUTS0" = 2,
        "NUTS1" = 3,
        "NUTS2" = 4,
        "NUTS3" = 5,
        "LAU1" = 9,
        "LAU2" = 13
    )
    
    doSelect <- nchar(row.names(allPolygons)) == nCharacters
    
  } 
  
  subsetPolygons <- subset(allPolygons, doSelect)
  
  if(length(subsetPolygons) == 0){
    
    warning("No polygons are selected at ", spatialLevel,". 
            Please check whether rownames of shape data can be recognized as 
            NUTS0 (2 characters), NUTS1 (3 characters), NUTS2 (4 characters), 
            NUTS3 (5 characters), LAU1 (9 characters), LAU2 (13 characters)")
    
  }
  
  return(subsetPolygons)
  
}


#' Create dates for given year, month and day 
#' @param inputData data frame, the original data
#' @param timeNames character vector of max length 3, variable names in inputData 
#' for year, month and day (in that order); month or day may be missing and are
#' then replaced by value 1 
#' @return dates vector, the date based on given year, month and day in inputData
#' @export
makeDates <- function(inputData, timeNames){
  
  dates <- switch(length(timeNames), 
      
      "1" = paste0(inputData[, timeNames[1]], "/1/1"),
      "2" = apply(inputData[, timeNames[1:2]], 1, function(iRow){
            
            paste0(paste(iRow, collapse = "/"), "/1")
            
          }),
      
      "3" = apply(inputData[, timeNames], 1, function(iRow){
            
            paste(iRow, collapse = "/")
            
          })
  
  )
  
  
  return(as.Date(dates))
  
}


#' Read dates to generat year, month and day
#' @param dates Date vector, the date from which year, month and day will 
#' be derived
#' @param dateFormat character, indicates the format of dates 
#' @param timeUnit character, indicates to which time unit the dates should be
#' translated
#' @return data frame with Year, Month and Day (or less depending on timeUnit) 
#' and the original Date
#' @export
readDates <- function(dates, dateFormat, timeUnit = c("Year", "Month", "Day")) {
  
  timeUnit <- match.arg(timeUnit)
  
  allTimes <- data.frame(
      Year = as.numeric(strftime(dates, format = "%Y")),
      Month = as.numeric(strftime(dates, format = "%m")),
      Day = as.numeric(strftime(dates, format = "%d")),
      Date = dates
  )
  
  toReturn <- switch(timeUnit, 
      "Year" = allTimes[, c("Date", "Year")],
      "Month" = allTimes[, c("Date", "Year", "Month")],
      "Day" = allTimes[, c("Date", "Year", "Month", "Day")])
  
  
  return(toReturn)
  
}




#' Add lag time to dates 
#' @param dates dates vector, input values for the dates
#' @param lagTime integer, the number of years/months/days that should
#' be added to the dates
#' @param timeUnit character, the timeUnit for addition; should be one of
#' \code{c("Year", "Month", "Day")}; default value is "Year"
#' @return dates vector, the lagged dates ie original dates with added number 
#' of years/months/days 
#' @importFrom lubridate leap_year years %m+%
#' @export
makeLagDates <- function(dates, lagTime, 
    timeUnit = c("Year", "Month", "Day")) {
  
  
  timeUnit <- match.arg(timeUnit)
  
  laggedDates <- switch(timeUnit, 
      
      "Year" = {
        
        isLeapDate <- leap_year(year(dates)) & !leap_year(year(dates) + lagTime) & 
            month(dates) == 2 & day(dates) == 29
        
        tmpDates <- dates + years(lagTime)
        
        tmpDates[isLeapDate] <- as.Date(paste0(year(dates[isLeapDate]) + 
                    lagTime, "/02/28"))   
        
        tmpDates
        
      },
      
      "Month" = dates %m+% months(lagTime),
      "Day" = dates + lagTime
  
  )
  
  
  return(laggedDates)
  
}


#' Calculate the modus
#' @param x vector for which the modus should be calculated
#' @param na.rm boolean, whether missing values can be ignored; default is TRUE
#' @return the modus of x
#' @export
modus <- function(x, na.rm = TRUE) {
  
  if (is.factor(x)) {
    
    x <- as.character(x)
    
  }
  
  if(na.rm){
    
    x <- x[!is.na(x)]
    
  }
  
  uniqueX <- unique(x)
  
  
  return(uniqueX[which.max(tabulate(match(x, uniqueX)))])
  
}

#' Calculate the relative frequency 
#' @param x factor vector for which the relative frequency of each of its values should
#' be calculated
#' @return the relative frequencies of x
#' @export
relativeFrequency <- function(x) {
  
  proportions <- prop.table(table(x))
  proportionsData <- data.frame(t(as.matrix(proportions, nrow = 1)))
  
  
  return(proportionsData)
  
}


#' Collapse data according to the user-defined variable
#' @param inputData data frame, the original data
#' @param collapseVariable character vector, variable names in inputData with 
#' respect to which the data should be collapsed
#' @param responsePositive character, variable name in inputData that indicates
#' the number of positive responses; default value is NULL
#' @param responseTotal character, variable name in inputData that indicates 
#' the total number of observations for the response; default value is NA
#' @param makeResponses boolean, whether to create response variables or not;
#' default value is TRUE
#' @param covariates character vector, variable names in inputData of the 
#' covariates that should be summarized when collapsing the data
#' @param functionCovariates character vector, the function for summarizing each
#' of the covariates; each one of \code{c("mean", "max", "min", "modus", 
#' "relativeFrequency")}; default is NULL
#' @return data frame, with the unique rows of the inputData (ie collapsed
#' inputData excluding the response), (numberPositive) the number of positive 
#' response values and (numberObservations) the total number of response values
#' @importFrom plyr ddply
#' @export
collapseData <- function(inputData, collapseVariable, responsePositive = NULL,
    responseTotal = NA, covariates = NULL, functionCovariates = NULL) {
  
  
  if(!is.null(covariates)){
    
    if (length(functionCovariates) != length(covariates)) {
      
      stop("The length of functionCovariates should be either 1 or 
              the same as for covariates")
      
    }
    
  } 
  
#  if(is.null(collapseVariable)){
#    
#    inputData$numberPositive <- inputData[, responsePositive]
#    inputData$numberObservations <- 1
#    inputData[, responsePositive] <- NULL
#    
#    return(inputData)
#    
#  }
  
  if(!is.null(inputData$isPositive)){
    
    inputData$isPositive <- NULL
    
  }
  
  if(!is.null(inputData$percentagePositive)){
    
    inputData$percentagePositive <- NULL
    
  }
  
  
  if(!is.null(responsePositive) & is.na(responseTotal)) {
    
    responseTotal <- "responseTotal"    
    inputData$responseTotal <- 1
    
  }
  
  
  allNames <- names(inputData)
  idData <- unique(inputData[, collapseVariable, drop = FALSE])
  nObservations <- nrow(idData)
  
  
  if(nObservations == nrow(inputData)){
    
    warning("The data cannot be collapsed: The collapsed data size equals the orginal size")
    
  }
  
  if(nObservations == 1){
    
    stop("The data cannot be collapsed: The collapsed data size will be 1")
    
  }
  
  
  if (!is.null(responsePositive)) {
    
    collapsedResponses <- ddply(inputData, collapseVariable, function(data){
          
          data.frame(numberPositive = sum(data[, responsePositive], na.rm = TRUE), 
              numberObservations = sum(data[, responseTotal], na.rm = TRUE)
          )
          
        })
    
  }
  
  if(is.null(covariates)){
    
    collapsedData <- collapsedResponses
    
  } else {
    
    toFactor <- covariates[which(functionCovariates == "relativeFrequency")]
    inputData[, toFactor] <- as.factor(unlist(inputData[, toFactor]))
    
    collapsedCovariates <- ddply(inputData, collapseVariable, function(data){
          
#          sapply(seq_along(covariates), function(i) { 
#                
#                do.call(functionCovariates[i], 
#                    list(x = data[, covariates[i]]))
#                
#              })
          
          tmp <- data.frame(sapply(seq_along(covariates), function(i) { 
                    
                    assign(covariates[i], do.call(functionCovariates[i], 
                            list(x = data[, covariates[i]])))
                    
                  }, simplify = FALSE), stringsAsFactors = FALSE)
          
          covariateNames <- unlist(sapply(seq_along(covariates), function(i) { 
                    
                    if(functionCovariates[i] == "relativeFrequency") {
                      
                      levels(data[, covariates[i]])
                      
                    } else {
                      
                      covariates[i]
                      
                    }
                    
                  }))
          
          names(tmp) <- covariateNames
          
          tmp
          
        })
    
    if (!is.null(responsePositive)) {
      
      collapsedData <- merge(x = collapsedResponses, y = collapsedCovariates,
          by = collapseVariable)
      
    } else {
      
      collapsedData <- collapsedCovariates
      
    }
    
  }
  
  
  # Check with unique inputData
  uniqueData <- unique(inputData[, !(allNames %in% 
                c(responsePositive, responseTotal, covariates)), drop = FALSE])
  
  nObservations <- nrow(uniqueData)
  
  
  if(nrow(collapsedData) != nObservations){
    
    toReport <- duplicated(uniqueData)
    
    if(any(toReport)) {
      
      warning("Following observations do not have a unique ID as expected \n", 
          call. = FALSE,
          paste(capture.output(print(uniqueData[toReport,])), collapse = "\n"))
      
    }
    
  }
  
  toReturn <- merge(x = uniqueData, y = collapsedData, by = collapseVariable)
  
  
  if (!is.null(responsePositive)) {
    
    toReturn$numberObservations[is.na(toReturn$numberObservations)] <- 0
    toReturn$isPositive <- as.numeric(toReturn$numberPositive > 0)
    toReturn$percentagePositive <- toReturn$numberPositive / toReturn$numberObservations
    
  }
  
  
  return(toReturn)
  
}




#' Merge the temporal, spatial and covarites data at lowest space and time level
#' @param temporalData data frame, the temporal data
#' @param spatialData data frame, the spatial data
#' @param covariatesData data frame, the covariates data
#' @param responseVariable character, name of the binary response in the temporal data
#' @param timeResolution character, indicates the lowest time level;
#' one of \code{c("Year", "Month", "Day", "Date")}
#' @param dateFormat character, indicates the format of dates; is only used for
#' timeResolution "Date" 
#' @param spaceResolution character, indicates the lowest space level;
#' one of \code{c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2")} 
#' @param timeNamesTemporal character vector, the variable names in temporalData
#' for the time variables
#' @param spatialNameTemporal character, the variable name in temporalData for
#' the space level 
#' @param covariatesTemporal character vector, the variable names in temporalData
#' for the covariates 
#' @param functionCovariatesTemporal character vector, the names of the functions
#' used to summarize the covariates in temporalData 
#' @param timeNamesExtra character vector, the variable names in covariatesData
#' for the time variables
#' @param spatialNameExtra character vector, the variable name in covariatesData
#' for the space level 
#' @param covariatesExtra character vector, the variable names in covariatesData
#' for the covariates 
#' @param functionCovariatesExtra character vector, the names of the functions
#' used to summarize the covariates in covariatesData 
#' @param lagTime integer, the number of years/months/days that should
#' be added to the dates; specified in units of timeResolution
#' @param covariatesSpatial character vector, the variable names in spatialData
#' for the covariates 
#' @return data frame, merged temporal, spatial and covariates data; variable names
#' are the spatial and time levels from highest to the resolution level, 
#' response variables and covariates from each of the data frames to be retained
#' @export
mergeAllData <- function(temporalData, spatialData, covariatesData,
    responseVariable,
    timeResolution = c("Year", "Month", "Day", "Date"), dateFormat,
    spaceResolution = c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2"),
    timeNamesTemporal, spatialNameTemporal, 
    covariatesTemporal = NULL, functionCovariatesTemporal = NULL,
    timeNamesExtra, spatialNameExtra, 
    covariatesExtra = NULL, functionCovariatesExtra = NULL, lagTime = 0, 
    covariatesSpatial = NULL) {
  
  
  timeLevels <- c("Year", "Month", "Day", "Date")
  spatialLevels <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2")
  
  
  # Step 1: Handle temporal data
  
  selectedTemporal <- temporalData[, c(responseVariable, timeNamesTemporal,
          spatialNameTemporal, covariatesTemporal)]
  
  # Create dates for temporal data
  if (timeResolution != "Date") {
    
    selectedTemporal$date <- makeDates(inputData = selectedTemporal, 
        timeNames = timeNamesTemporal)
    selectedTemporal <- selectedTemporal[, !(names(selectedTemporal) %in% timeNamesTemporal)]
    
    timeUnit <- timeResolution
    
  } else {
    
    selectedTemporal$date <- selectedTemporal[, timeNamesTemporal]
    selectedTemporal[, timeNamesTemporal] <- NULL
    
    timeUnit <- "Day"
    
  }
  
  # Collapse temporal data
  collapseVariable <- c(spatialNameTemporal, "date")
  names(collapseVariable) <- NULL
  
  collapsedTemporal <- collapseData(inputData = selectedTemporal,
      collapseVariable = collapseVariable, 
      responsePositive = responseVariable,
      covariates = covariatesTemporal,
      functionCovariates = functionCovariatesTemporal)
  
  
  # Step 2: Handle covariates data
  
  if (!is.null(covariatesExtra)) {
    
    selectedExtra <- covariatesData[, c(spatialNameExtra, timeNamesExtra, 
            covariatesExtra)]
    
    # Create dates for covariates data
    if(timeResolution != "Date") {
      
      selectedExtra$date <- makeDates(inputData = selectedExtra, 
          timeNames = timeNamesExtra)
      selectedExtra <- selectedExtra[, !(names(selectedExtra) %in% timeNamesExtra)]                              
      
    } else {
      
      selectedExtra$date <- selectedExtra[, timeNamesExtra]
      selectedExtra[, timeNamesExtra] <- NULL
      
    }
    
    # Add lag time
    if (lagTime > 0) {
      
      selectedExtra$date <- makeLagDates(dates = selectedExtra$date, 
          lagTime = lagTime, timeUnit = timeUnit)  
      
    }
    
    # Collapse covariates data
    collapseVariable <- c(spatialNameExtra, "date")
    names(collapseVariable) <- NULL
    
    suppressWarnings( # Expect warning if data cannot be collapsed
        collapsedExtra <- collapseData(inputData = selectedExtra,
            collapseVariable = collapseVariable, 
            responsePositive = NULL,
            covariates = covariatesExtra,
            functionCovariates = functionCovariatesExtra)
    )
    
  }
  
  # Step 3: Merge temporal and loaded covariates data
  
  if (!is.null(covariatesExtra)) {
    
    mergedData <- merge(x = collapsedTemporal, y = collapsedExtra,
        by.x = c(spatialNameTemporal, "date"),
        by.y = c(spatialNameExtra, "date"),
        all.x = TRUE, all.y = TRUE)   
    
  } else {
    
    mergedData <- collapsedTemporal
    
  }
  
  
  # Step 4: Merge this with spatial data according to lowest spatial level
  
  mergedData <- merge(x = spatialData, y = mergedData, 
      by.x = spaceResolution, by.y = spatialNameTemporal,
      all.x = TRUE)
  
  
  # Step 5: Handle dates
  
  if (timeResolution != "Date") {
    
    finalData <- cbind(mergedData, readDates(dates = mergedData$date, 
            dateFormat = "%Y-%m-%d", timeUnit = timeUnit))
    
    dateNames <- timeLevels[1:which(timeLevels == timeResolution)]
    
  } else {
    
    finalData <- cbind(mergedData, readDates(dates = mergedData$date, 
            dateFormat = dateFormat, timeUnit = timeUnit))
    
    dateNames <- timeLevels[-4]
    
  }
  
  finalData$date <- NULL
  finalData$Date <- NULL
  
  
  # Step 6: Define outcomes and order columns
  
  finalData$numberObservations[is.na(finalData$numberObservations)] <- 0
  
  finalData$isPositive <- as.numeric(finalData$numberPositive > 0)
  finalData$percentagePositive <- with(finalData, numberPositive / numberObservations)
  
  
  orderedData <- finalData
  
  allNames <- names(orderedData)
  firstNames <- c(spatialLevels[1:which(spatialLevels == spaceResolution)], dateNames)
  responseNames <- c("numberPositive", "numberObservations", "isPositive", 
      "percentagePositive")
  toReturn <- orderedData[, c(firstNames, responseNames,
          allNames[!(allNames %in% c(firstNames, responseNames))])]
  
  
  return(toReturn)
  
}


#' Collapse and extend data frame to have one row for each time unit at the 
#' spatial level of interest
#' @param infoData data frame at the spatial level of interest, contains 
#' the timeNames (for year, month and day) and spatialUnit (for NUTS0, NUTS1, 
#' NUTS2, NUTS3, LAU1 or LAU2)
#' @param timeNames character vector, names of the time variables for 
#' year, month and day (in that order)
#' @param timeUnit character string that determines the time level of interest; 
#' should be one of \code{c("Year", "Month", "Week", "Day")}; default is "year"
#' @param spatialUnit character string, variable name in the infoData that 
#' determines the spatial level of interest
#' @param inlaVariables boolean, if TRUE extra variables at space and time level
#' are created, as needed for inla(); default value is TRUE
#' @param responsePositive character, variable name in inputData that indicates
#' the number of positive responses
#' @param responseTotal character, variable name in inputData that indicates 
#' the total number of observations for the response; default value is NA
#' @param covariates character vector, variable names in inputData of the 
#' covariates that should be summarized when collapsing the data; default is NULL
#' @param functionCovariates character vector, the function for summarizing each
#' of the covariates; each one of \code{c("mean", "max", "min", "modus")}; 
#' if one function is specified for a vector of covariates that same function 
#' is used for all covariates; default is NULL
#' @return extended data frame of infoData with variables: spatialUnit and 
#' timeUnit with unique value for resp. each spatial unit and time unit; 
#' if inlaVariables is TRUE also spatialTimeUnit which is combination of both
#' @importFrom zoo as.yearmon 
#' @importFrom lubridate floor_date
#' @export
makeSpaceTimeData <- function(mergedData, timeNames, 
    timeUnit = c("Year", "Month", "Week", "Day"), spatialUnit,
    inlaVariables = TRUE,
    responsePositive, responseTotal = NA, covariates = NULL, 
    functionCovariates = NULL){
  
  
  timeUnit <- match.arg(timeUnit)
  
  # Merged data
  dataSelected <- mergedData
  dataSelected$spatialUnit <- dataSelected[, spatialUnit]
  
  minYear <- min(dataSelected[, timeNames[1]], na.rm = TRUE)
  maxYear <- max(dataSelected[, timeNames[1]], na.rm = TRUE)
  
  dataSelected$sampleCountry <- substr(dataSelected$spatialUnit, 1, 2)
  
  
  # Define time variables
  
  switch(timeUnit,
      
      "Year" = {
        
        dates <- as.Date(paste0("1/1/", dataSelected[, timeNames[1]]), 
            format = "%d/%m/%Y")
        dates[is.na(dates)] <- min(dates, na.rm = TRUE) 
        dataSelected$timeUnit <- as.factor(format(dates, "%Y"))
        allTimes <- levels(dataSelected$timeUnit)
        
      },
      
      "Month" = {
        
        dates <- as.Date(paste0("1/", dataSelected[, timeNames[2]], "/", 
                dataSelected[, timeNames[1]]), 
            format = "%d/%m/%Y")
        dates[is.na(dates)] <- min(dates, na.rm = TRUE)
        dataSelected$timeUnit <- as.factor(as.yearmon(dates))
        levels(dataSelected$timeUnit) <- gsub(" ", "_", levels(dataSelected$timeUnit))
        allTimes <- levels(dataSelected$timeUnit)
        
      },
      
      "Week" = {
        
        dates <- as.Date(paste0(dataSelected[, timeNames[3]], "/", 
                dataSelected[, timeNames[2]], "/", dataSelected[, timeNames[1]]), 
            format = "%d/%m/%Y")
        dates <- floor_date(dates, unit = "week")
        minDate <- min(dates, na.rm = TRUE)
        dates[is.na(dates)] <- minDate
        dataSelected$timeUnit <- as.factor(ceiling(difftime(dates, minDate, units = "weeks") + 1))
        allTimes <- levels(dataSelected$timeUnit)
        
      },
      
      "Day" = {
        
        dates <- as.Date(paste0(dataSelected[, timeNames[3]], "/", 
                dataSelected[, timeNames[2]], "/", dataSelected[, timeNames[1]]), 
            format = "%d/%m/%Y")
        minDate <- min(dates, na.rm = TRUE)
        dates[is.na(dates)] <- minDate
        dataSelected$timeUnit <- as.factor(difftime(dates, minDate, units = "days") + 1)
        allTimes <- levels(dataSelected$timeUnit)
        
      })
  
  
  #Save dates
  dataSelected$dates <- dates
  allDates <- unique(dataSelected[, c("timeUnit", "dates")])
  rownames(allDates) <- allDates$timeUnit
  
  
  
  # 1. Collapse data: Summarize data per uniquely observed time and spatial level
  dataSelected <- collapseData(inputData = dataSelected[, c("spatialUnit", 
              "timeUnit", "dates", "sampleCountry", responsePositive, responseTotal,
              covariates)], 
      collapseVariable = c("spatialUnit", "timeUnit"),
      responsePositive = responsePositive, responseTotal = responseTotal,
      covariates = covariates, 
      functionCovariates = functionCovariates)
  
  
  # 2. Extend data: Create missing values for times without observations
  for (iArea in unique(dataSelected$spatialUnit)){
    
    missingTimes <- allTimes[!allTimes %in% dataSelected$timeUnit[dataSelected$spatialUnit == iArea]]
    
    if (length(missingTimes) > 0) {
      
      dataTmp <- data.frame(matrix(NA, ncol = ncol(dataSelected), 
              nrow = length(missingTimes)))
      colnames(dataTmp) <- colnames(dataSelected)
      
      dataTmp$timeUnit <- missingTimes
      dataTmp$dates <- allDates[missingTimes, "dates"]
      dataTmp$spatialUnit <- iArea
      dataTmp$sampleCountry <- substr(iArea, 1, 2)
      
      dataSelected <- rbind(dataSelected, dataTmp)
      
    }
    
  }
  
  
  # Create weights: weight, a weight given to each country*year combination
#  completeData <- dataSelected[dataSelected$present == 1,] 
#  tab1 <- table(completeData$sampleCountry, completeData[, timeNames[1]])
#  
#  # TODO 170 is the planned number of observations: in our study also?
#  weight <- ifelse(tab1 == 0, 0, 170/tab1)
#  weightData <- data.frame(weight = as.numeric(weight),
#      CY = paste0(rep(rownames(tab1), ncol(tab1)), rep(colnames(tab1), each = nrow(tab1))))
#  
#  dataSelected$CY <- paste0(dataSelected$sampleCountry, dataSelected[, timeNames[1]])
#  dataSelected <- merge(dataSelected, weightData, by = "CY")
#  dataSelected$CY <- NULL
  
  dataSelected <- dataSelected[order(dataSelected$spatialUnit, dataSelected$timeUnit),]
  dataSelected$spatialTimeUnit <- as.factor(paste0(dataSelected$spatialUnit, "_", dataSelected$timeUnit))
  
  if (inlaVariables) {
    
    # TODO remove redundant ones
    dataSelected$spatialId <- dataSelected$spatialId2 <- 
        as.numeric(as.factor(dataSelected$spatialUnit))
    #    dataSelected$st.ID.s <- as.numeric(as.factor(dataSelected$spatialUnit))
    
#  dataSelected$year <- dataSelected$sampY - minYear + 1 
#  dataSelected$cyear <- as.factor(dataSelected$year)
#  
#  dataSelected$month <- dataSelected$trendMonth
#  dataSelected$cmonth <- as.factor(dataSelected$sampM)
    
    dataSelected$timeId <- dataSelected$timeId2 <- as.numeric(dataSelected$timeUnit)
#    dataSelected$t.ID.iid <- dataSelected$st.ID.t <- as.numeric(dataSelected$timeUnit)
#    dataSelected$ID.all <- 1:nrow(dataSelected)
    
    dataSelected$spatialTimeId <- as.numeric(dataSelected$spatialTimeUnit)
#    dataSelected$st.ID <- as.numeric(dataSelected$spatialTimeUnit)
    
  }
  
  
  return(dataSelected)
  
}


#' Save neighboursList for use with inla()
#' @param neighboursList object of class "nb", a neighbours list
#' @param weightStyle character, indicating style for assigning neighbour weights;
#' one of \code{c("B", "W")} respectively row-standardized (sum of weights per 
#' area is 1) or binary (neighbour is 1, non-neighbour is 0); default is "B"
#' @param fileName character string, specifies the file name (without extension)
#' to save neighbours info required by inla(); default is "adjacencyINLA" 
#' @return NULL. Writes file with neighbours as required by inla() for the 
#' spatial models in temporary directory 
#' @importFrom spdep nb2mat nb2INLA
#' @export 
saveNeighboursInla <- function(neighboursList, weightStyle = c("B", "W"), 
    fileName = "adjacencyINLA") {
  
  weightStyle <- match.arg(weightStyle)
  
  neighboursMatrix <- nb2mat(neighboursList, style = weightStyle, zero.policy = TRUE)
  neighb.adj <- file.path(tempdir(), paste0(fileName, ".adj"))
  nb2INLA(neighb.adj, mat2listw(neighboursMatrix)$neighbours)
  
  
  return(NULL)
  
}



#' Calculate local Moran's I or local Geary's C
#' @param response numeric vector; response of interest for analysis
#' @param neighboursList object as returned by poly2nb()
#' @param weightStyle character, indicating style for assigning neighbour weights;
#' one of \code{c("B", "W")} respectively row-standardized (sum of weights per 
#' area is 1) or binary (neighbour is 1, non-neighbour is 0); default is "B"
#' @param localMethod method for local values calculation; one of 
#' \code{c("moran", "geary")}; default is "moran"
#' @return dataframe with 'statistics' ie local moran/geary statistic, 
#' regions with missing response or without neighbours will have value NA;
#' and 'pValues' for localMethod moran only ie p-value of local moran statistic
#' @importFrom spdep nb2listw localmoran localG poly2nb
#' @export 
calculateLocal <- function(response, neighboursList, weightStyle = c("B", "W"),
    localMethod = c("moran", "geary")){
  
  weightStyle <- match.arg(weightStyle)
  localMethod <- match.arg(localMethod)
  
  neighboursList <- nb2listw(neighboursList, style = weightStyle, 
      zero.policy = TRUE)
  
  # zero.policy = TRUE defines that regions without neighbours will have 0 values
  # for the local statistics values; FALSE will stop with error
  
  if(localMethod == "moran"){
    
    localValues <- localmoran(response, neighboursList, zero.policy = TRUE, 
        na.action = na.exclude)
    localValues[is.na(localValues[,5]), 1] <- NA
    
    return(data.frame(statistics = localValues[,1], pValues = localValues[,5]))
    
  } else {
    
    # For localG() no na.action option included
    if(any(is.na(response))){
      
      response <- na.exclude(response)
      na.act <- attr(response, "na.action")
#      newNames <- attr(neighboursWeights, "region.id")
      
      subset <- !(seq_along(neighboursList$neighbours) %in% na.act)
      neighboursSubset <- subset(neighboursList, subset, zero.policy = TRUE)
      
      localValues <- localG(as.numeric(response), neighboursSubset, 
          zero.policy = TRUE)
      
      localValues <- naresid(na.act, localValues)
      
    } else {
      
      localValues <- localG(response, neighboursList, zero.policy = TRUE)
      
    }
    
    return(data.frame(statistics = as.numeric(localValues)))
    
  }
  
}



#' Create classes for response according to breaksStyle 
#' @param response numeric vector; response of interest for analysis
#' @param breaksStyle character, defines the style for creating classes; one of 
#' \code{c("quantile", "jenks", "fixed", "quadrant")}; default is quantile;
#' quadrants are for calculated local values, for the others see classIntervals() 
#' @param nClasses numeric; number of classes that should be created;
#' value is ignored if breaksStyle is "quadrants"
#' @param fixedBreaks numeric vector with breaks values; ignored unless 
#' breaksStyle is "fixed"
#' @param intervalClosure character; one of \code{c("left", "right")}; 
#' default is left
#' @param localValues object as returned by calculateLocal()
#' @param significance numeric value, used to identify significant local values;
#' default is NA; ignored unless breaksStyle is "quadrants" 
#' @return vector of factor levels with same length as response
#' @importFrom classInt classIntervals findCols
#' @export 
createClasses <- function(response, 
    breaksStyle = c("quantile", "jenks", "fixed", "quadrant"), 
    nClasses, fixedBreaks, intervalClosure = c("left", "right"), 
    localValues, significance = NA){
  
  breaksStyle <- match.arg(breaksStyle)
  intervalClosure <- match.arg(intervalClosure)
  
  if(breaksStyle == "quadrant"){
    
    factorResponse <- factor(response, 
        levels = c("High-High", "High-Low", "Low-High", "Low-Low"))
    
    centeredResponse <- scale(response, scale = FALSE)     
    centeredLocal <- scale(localValues$statistics, scale = FALSE)    
    
    factorResponse[is.nan(centeredLocal)| is.na(centeredLocal)] <- NA
    factorResponse[centeredResponse > 0 & centeredLocal > 0] <- "High-High"      
    factorResponse[centeredResponse < 0 & centeredLocal < 0] <- "Low-Low"      
    factorResponse[centeredResponse < 0 & centeredLocal > 0] <- "Low-High"
    factorResponse[centeredResponse > 0 & centeredLocal < 0] <- "High-Low"
    
    if(!is.na(significance) & !is.null(localValues$pValues)){
      
      factorResponse[localValues$pValues > significance] <- "Non-significant"  
      
    }
    
  } else {
    
    suppressWarnings(breaksTable <- classIntervals(response, n = nClasses, 
            style = breaksStyle, fixedBreaks = fixedBreaks, 
            intervalClosure = intervalClosure, pal = letters[1:nClasses]))
    
    # To adjust for ties in most left or right interval; cut() cannot handle them
    breaks <- breaksTable$brks
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
    breaks <- unique(breaks)
    
    if(breaksStyle == "jenks"){
      
      intervalClosure <- "right"
      
    }
    
    tmpTable <- classInt:::tableClassIntervals(
        cols = findCols(breaksTable), brks = breaks, 
        intervalClosure = intervalClosure)
    
    factorResponse <- cut(breaksTable$var, breaks = breaks,
        include.lowest = TRUE, right = (intervalClosure == "right"), 
        labels = names(tmpTable))
    
#    factorResponse <- factor(names(tmpTable)[findInterval(
#                breaksTable$var, breaksTable$brks, rightmost.closed = TRUE)])
    
  }
  
  return(factorResponse)
  
}



#' Perform ordinary kriging: region center used for modelling
#' @param response numeric vector, response in the formula for defining kriging
#' @param shapeData a spatialPolygonsDataFrame object as returned from 
#' readShapeData() function
#' @param variogramModel object of class variogramModel as returned by function
#' vgm(); if missing autoKrige() is performed, default value is NA
#' @return list of gstatVariogram, variogramModel and SpatialPolygonsDataFrame 
#' objects; the first is an extended data.frame with the sample variogram as 
#' returned by variogram(); the second is an extended data.frame with model 
#' information on the variogram, as returned from vgm(); the latter contains a 
#' data frame with for each region the predicted values 'var1.pred'; their 
#' variance 'var1.var' and standard deviation 'var1.stdev'
#' @importFrom automap autoKrige
#' @importFrom gstat variogram fit.variogram krige
#' @importFrom sp coordinates
#' @export
performKriging <- function(response, shapeData, variogramModel = NA){
  
  # We use the center of the shapes for modelling and then predict for each region
  
  # Exclude missing values: autoKrige() cannot handle them
  idMissing <- is.na(response)
  completeResponse <- response[!idMissing]
  
  infoData <- data.frame(response = response, 
      x = coordinates(shapeData)[,1], y = coordinates(shapeData)[,2])[!idMissing, ]
  coordinates(infoData) <- ~ x + y
  # spplot(infoData)
  
  if(any(is.na(variogramModel))){
    
    krigingResult <- autoKrige(formula = completeResponse ~ 1,
        input_data = infoData, new_data = shapeData, 
        model = c("Sph", "Exp", "Gau"))
    
    variogramResult <- krigingResult$exp_var 
    variogramModel <- krigingResult$var_model
    
    krigingPredictions <- krigingResult$krige_output
    
    # model full names: vgm()
    
  } else {
    
    variogramResult <- variogram(completeResponse ~ 1, data = infoData)
    variogramModel <- fit.variogram(variogramResult, model = variogramModel)
    
    krigingPredictions <- krige(completeResponse ~ 1,
        infoData, newdata = shapeData, model = variogramModel)
    
  }
  
  rownames(krigingPredictions@data) <- as.character(1:nrow(krigingPredictions@data)) 
  
  return(
      list(gstatVariogram = variogramResult,
          variogramModel = variogramModel,
          krigingPredictions = krigingPredictions)
  )
  
}




#' Construct the formula for the inla() model fit
#' @param responseName character string, name of the response to analyse
#' @param covariateNames character vector, names of the covariates for the 
#' inla model; default is NULL
#' @param timeEffect character string, defines the time effect; one of 
#' \code{c("none", "saturated", "linear", "rw1", "ar1", "rw2", "all")};
#' default is "none"
#' @param interactionEffect character string, defines the interaction effect;
#' one of c("none", "type1", "type2", "type3", "type4"); default is "none"
#' @return character string, with the formula for inla(); if timeEffect is "all"
#' a vector of character strings for all possible models is returned 
#' @export
makeFormulaInla <- function(responseName, covariateNames = NULL,
    timeEffect = c("none", "saturated", "linear", "rw1", "ar1", "rw2", "all"),
    interactionEffect = c("none", "type1", "type2", "type3", "type4")){
  
  timeEffect <- match.arg(timeEffect)
  interactionEffect <- match.arg(interactionEffect)
  
  # Main effects #
  
  formulaSpace <- "+ f(spatialId, model = 'bym', graph = neighb.adj, 
      adjust.for.con.comp = FALSE, scale.model = TRUE, constr = TRUE, 
      hyper = list(prec.unstruct = list(param = c(1, 0.01), initial = 4),
      prec.spatial = list(initial = 4, param = c(1, 0.015))))"
  
  timeParts <- c(none = "~ 1", 
      saturated = "~ 1 + as.factor(timeId)", 
      linear = "~ 1 + f(timeId, model = 'linear', replicate = spatialId)",
      rw1 = "~ 1 + f(timeId, model = 'rw1', 
          hyper = list(prec = list(prior = 'loggamma', param=c(1, 0.01))), 
          replicate = spatialId)",
      ar1 = "~ 1 + f(timeId, model = 'ar1', 
          hyper = list(prec = list(prior = 'loggamma', param = c(1, 0.01)), 
          rho = list(param = c(0, 0.2), initial = 2)), replicate = spatialId)",
      rw2 = "~1 + f(timeId, model = 'rw2', 
          hyper = list(prec = list(prior = 'loggamma' , param=c(1, 0.00005))),
          replicate = spatialId)")    
  
  allFormulas <- paste(responseName, timeParts, formulaSpace)
  names(allFormulas) <- names(timeParts)
  
  if(timeEffect != "all"){
    
    mainFormula <- allFormulas[timeEffect]
    
  } else {
    
    mainFormula <- allFormulas
    
  }
  
  
  # Interaction effects #
  
  if (timeEffect %in% c("none", "saturated", "linear")) {
    
    trend <- "rw1"
    
  } else if (timeEffect == "all") {
    
    trend <- c(rep("rw1", 4), "ar1", "rw2") 
    
  } else {
    
    trend <- timeEffect
    
  }
  
  interactionFormula <- switch(interactionEffect, 
      
      "none" = "",
      
      "type1" = rep("+ f(timeId2, model = 'iid', 
              hyper = list(list(prior = 'loggamma', param = c(1, 0.0001))),
              replicate = spatialId2)", length.out = length(trend)),
      
      "type2" = paste0("+ f(spatialId2, model = 'iid', group = timeId2,
              control.group = list(model = '", trend, "'))"),
      
      "type3" = rep("+ f(timeId2, model = 'iid', 
              hyper = list(list(prior = 'loggamma', param = c(1, 0.0001))), 
              group = spatialId2, control.group = list(model= 'besag', graph = neighb.adj,
              scale.model = TRUE, adjust.for.con.comp = FALSE))", 
          length.out = length(trend)),
      
      "type4" =  paste0("+ f(spatialId2, model = 'besag', graph = neighb.adj,
              scale.model = TRUE, adjust.for.con.comp = FALSE, 
              hyper = list(prec = list(initial = 4, param = c(1,0.01))),
              constr = TRUE, group = timeId2, 
              control.group = list(model='", trend, "'))")
  
  )
  
  
  # Bind main and interaction formula; add covariates if necessary #
  
  formulaInla <- paste(mainFormula, interactionFormula)
  
  if (!is.null(covariateNames)){
    
    formulaInla <- paste(formulaInla, "+", paste(covariateNames, collapse = "+"))
    
  } 
  
  
  return(formulaInla)
  
}



#' Fit space-time model with inla()
#' @param spaceTimeData data frame with all necessary data for the inla() model fitting;
#' as returned from function makeSpaceTimeData()
#' @param formulaInla character string, with formula returned by formulaInla()
#' and used as first argument of inla()
#' @param neighboursFileName character string indicates the file name (without 
#' extension) for the saved INLA neighbours list
#' @param weighted boolean, whether weighted inla() regression should be 
#' performed or not; default is FALSE
#' @param verbose boolean, whether to print info during the inla analysis;
#' default value is FALSE
#' @return the output as generated by the inla() procedure; if the length of 
#' formulaInla is larger than 1, only the model with best time-effect will 
#' be returned as well as all models' dic, logScore and convergence status
#' @importFrom INLA inla   
#' @export
performInla <- function(spaceTimeData, formulaInla, neighboursFileName, 
    weighted = FALSE, verbose = FALSE){
  
  
  neighb.adj <- file.path(tempdir(), paste0(neighboursFileName, ".adj"))
  familyModel <- "binomial"
  
  
  
  # Fit INLA model & refit at most 5x if not converged
  allResults <- lapply(formulaInla, function(formulaInla){
        
        tryCatch({
              
              argumentsInla <- list(formula = as.formula(formulaInla), data = spaceTimeData, 
                  family = familyModel, control.compute = list(dic = TRUE, cpo = TRUE), 
                  verbose = verbose, control.predictor = list(link = 1), 
                  control.fixed = list(correlation.matrix = TRUE))
              
              if(weighted){
                
                inla.setOption(enable.inla.argument.weights = TRUE)
                argumentsInla$weights <- weight 
                argumentsInla$control.inla <- list(strategy = "gaussian", int.strategy = "eb")
                
              }
              
              inlaResult <- do.call("inla", argumentsInla)
              
              nRefit <- 0
              while ((inlaResult$mode$mode.status > 0) & (nRefit < 5)) {
                
                nRefit <- nRefit + 1
                
                argumentsInla$control.fixed <- NULL
                argumentsInla$control.mode <- list(result = inlaResult, restart = TRUE)
                
                if(weighted){
                  
                  argumentsInla$control.inla <- list(strategy = "gaussian", 
                      int.strategy = "eb", h = 1e-3)
                  
                } else {
                  
                  argumentsInla$control.inla <- list(h = 1e-3)
                  
                }
                
                inlaResult <- do.call("inla", argumentsInla)
                
              }
              
              cat("Number of restarts for fitting the model:", nRefit, "\n")
              return(inlaResult)
              
            }, error = function(err) {
              
              return(err)
              
            })
        
      })
  
  
  # Exclude results for models with error
  withError <- sapply(allResults, function(x) any(attr(x, "class") == "error"))
  
  if (any(withError)) {
    
    cat("The INLA model could not be fitted for \n", 
        paste(formulaInla[withError], allResults[withError], sep = "\n"))  
    allResults <- allResults[!withError]   
    allTimeEffects <- c("none", "saturated", "linear", "rw1", "ar1", "rw2")[!withError]
    
  } else {
    
    allTimeEffects <- c("none", "saturated", "linear", "rw1", "ar1", "rw2")
    
  }
  
  if (length(allResults) == 0) {
    
    return()
    
  }
  
  
  # Find best model out of allResults 
  if(length(formulaInla) > 1) {
    
    # mode.status == 0 : converged
    converged <- sapply(allResults, function(x) x$mode$mode.status) == 0
    modelDic <- sapply(allResults, function(x) x$dic$dic)
    logScore <- sapply(allResults, function(x) -mean(log(x$cpo$cpo), na.rm = TRUE))
    
    if ( any(converged) ) {
      
      inlaResult <- allResults[converged]
      inlaResult <- inlaResult[[which.min(sapply(inlaResult, function(x) x$dic$dic))]]
      
    } else {
      
      inlaResult <- allResults[[which.min(modelDic)]]
      
    }
    
    inlaResult <- list(inlaResult = inlaResult, timeEffect = allTimeEffects, 
        modelDic = modelDic, logScore = logScore, converged = converged,
        notFitted = c("none", "saturated", "linear", "rw1", "ar1", "rw2")[withError])    
    
  } else {
    
    inlaResult <- allResults[[1]]
    
  } 
  
  return(inlaResult)
  
}
