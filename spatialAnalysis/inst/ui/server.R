# Project: spatialAnalysis_git
# 
# Author: mvarewyck
###############################################################################



options(shiny.maxRequestSize = 1000*1024^2)   ## Needed to upload big data files

library(leaflet)  #for the map plotting
library(RColorBrewer) #for color pallettes
library(shinyjs) #for colourInput()

library(htmlwidgets) #for saving leaflet maps
library(webshot) #for saving leaflet maps

library(spatialAnalysis)

library(spdep) #for poly2nb and dnearneigh
#library(gstat)
library(ggplot2)


`%then%` <- shiny:::`%OR%`


spatialLevels <- c("NUTS0", "NUTS1", "NUTS2", "NUTS3", "LAU1", "LAU2")
timeLevels <- c("Year", "Month", "Day", "Date")

dataDir <- system.file("extdata", package = "spatialAnalysis")

# To prevent warnings when loading and matching data 
# "Warning in grepl(pattern, text) : input string xx is invalid in this locale"
Sys.setlocale('LC_ALL','C') 


shinyServer(function(input, output, session) {
      
      
      # To debug
      output$print <- renderPrint({
            
            
            
            
#results$subsetPlotData()
#            
#            
#            print(input$showRegion)
#            input$mapInla_shape_click$id
            
            
            
          })
      
      
      results <- reactiveValues()
      
      
      ### 1. Load Data Tab ###
      
      ## 1.0 Functions to make input fields ##
      
      makeTimeFields <- function(type = c("info", "extra"), varNames) {
        
        type <- match.arg(type)
        
        output[[paste0(type, "Time")]] <- renderUI({
              
              allLevels <- lapply(timeLevels, 
                  function(iLevel) {
                    selectInput(paste0(type, iLevel), iLevel, choices = varNames)
                  })
              
              if(input$timeResolution != "4") {
                
                allLevels[1:as.numeric(input$timeResolution)]
                
              } else {
                
                allLevels[[4]]
                
              }
              
            })
        
      }
      
      makeFunctionFields <- function(type = c("Info", "Shape", "Extra")){
        
        type <- match.arg(type)
        
        output[[paste0("functionCovariates", type)]] <- renderUI({
              
              validate(need(input[[paste0("covariates", type)]], "No covariates selected"))
              
              if(is.null(input[[paste0("covariates", type)]])) {
                
                return(NULL)
                
              } else {
                
                functionCovariates <- lapply(input[[paste0("covariates", type)]],
                    function(iName){
                      
                      selectInput(paste0("functionCovariates", type, iName), iName,
                          choices = c("mean", "max", "min", "modus", "relativeFrequency"))
                      
                    })
                
                return( 
                    column(10,
                        list(
                            strong("with summary functions:"),
                            functionCovariates
                        ), offset = 2) 
                )
                
              }
              
            })
        
      }
      
      
      ## Variables for further use ##
      observe({
            
            sapply(c("Info", "Shape", "Extra"), function(type) {
                  
                  results[[paste0("functionCovariates", type)]] <- reactive({
                        
                        if(is.null(input[[paste0("covariates", type)]])) {
                          
                          return(NULL)
                          
                        } else {
                          
                          allFunctions <- sapply(input[[paste0("covariates", type)]], 
                              function(iName) {
                                
                                input[[paste0("functionCovariates", type, iName)]]
                                
                              })
                          
                          return(allFunctions)
                          
                        }
                        
                      })
                  
                  return(NULL)
                  
                })
            
            results$allCovariates <- reactive(
                
                c(input$covariatesInfo, input$covariatesShape,
                    input$covariatesExtra)
            
            )
            
            results$allFunctionCovariates <- reactive(
                
                c(results$functionCovariatesInfo(), results$functionCovariatesShape(),
                    results$functionCovariatesExtra())
            
            )
            
          })
      
      
      
      
      
      results$spatialLevels <- reactive({
            
            allLevels <- spatialLevels[1:as.numeric(input$spaceResolution)]
            
            if(input$shapeDataPolygons == "lowest"){
              
              sapply(allLevels, function(iLevel) {
                    input[[paste0("shape", iLevel)]]
                  })
              
            } else {
              
              allLevels
              
            }
            
            
          })
      
      
      observe({
            
            sapply(c("info", "extra"), function(type) {
                  
                  if(type == "info"){
                    
                    iName <- "timeLevels"
                    
                  } else {
                    
                    iName <- "timeLevelsExtra"
                    
                  }
                  
                  results[[iName]] <- reactive({
                        
                        validate(need(input$timeResolution, "No temporal resolution selected"))
                        
                        if(input$timeResolution != "4") {
                          
                          sapply(timeLevels[1:as.numeric(input$timeResolution)],
                              function(iLevel) {
                                input[[paste0(type, iLevel)]]
                              })              
                          
                        } else {
                          
                          input[[paste0(type, "Date")]]
                          
                        }
                        
                      }) 
                  
                })
            
          })
      
      
      
      ## 1.1 Temporal Data ##
      
      # Read temporal data
      results$rawInfoData <- reactive({
            
            inFile <- input$infoData
            
            if(input$example > 0){
              
              read.table(file.path(dataDir, "rawDataUnique.csv"),
                  header = TRUE, sep = ",")
              
            } else {
              
              validate(need(inFile, "Please load temporal data"))
              
              read.table(inFile$datapath, header = TRUE, sep = input$sep, 
                  quote = '"', stringsAsFactors = FALSE)
              
            }
            
          })
      
      
      # Show temporal data
      output$showInfoData <- DT::renderDataTable({
            
            DT::datatable(results$rawInfoData(), options = list(
                    lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
                    pageLength = 10))
            
          })
      
      
      # Input fields: wellPanel
      output$levelPos <- renderUI({
            
            validate(need(input$responseVariable, "Please select 'Binary response'"))
            
            values <- results$rawInfoData()[, input$responseVariable]
            
            if(is.factor(values)){
              
              choices <- levels(values)
              
            } else {
              
              choices <- unique(values)
              
            }
            
            selectInput("levelPos", "with positive response value",
                choices = choices)
            
            
          })
      
      
      output$levelSubset <- renderUI({
            
            validate(need(input$subsetVariable, "Please select subset variable"))
            
            if(input$subsetVariable == "none"){
              
              return(NULL)
              
            } else {
              
              values <- results$rawInfoData()[, input$subsetVariable]
              
              if(is.factor(values)){
                
                choices <- levels(values)
                
              } else {
                
                choices <- unique(values)
                
              }
              
              return(
                  column(10,
                      selectInput("levelSubset", "keep value", 
                          choices = choices, selected = choices[2]),
                      offset = 2)
              )
            }
            
          })
      
      
      output$infoSpatial <- renderUI({
            
            varNames <- names(results$rawInfoData())
            
            selectInput("infoSpatial", 
                spatialLevels[as.numeric(input$spaceResolution)],
                choices = varNames)
            
          })
      
      
      output$infoData <- renderUI({
            
            varNames <- names(results$rawInfoData())
            makeTimeFields(type = "info", varNames = varNames)
            makeFunctionFields(type = "Info")
            
            list(
                
                fluidRow(
                    column(6, selectInput("responseVariable", "Binary response",
                            choices = varNames)),
                    column(6, uiOutput("levelPos"))
                ),
                
                selectInput("covariatesInfo", "Covariates", choices = varNames, 
                    multiple = TRUE),
                uiOutput("functionCovariatesInfo"),
                
                tags$hr(),
                
                strong("Select variable name for:"),
                uiOutput("infoTime"),
                uiOutput("infoSpatial"),
                
                tags$hr(),
                
                selectInput("subsetVariable", "Subset of the data according to:",
                    choices = c("<none>" = "none", varNames), selected = "none"),
                
                uiOutput("levelSubset")
            
            )
            
          })
      
      
      
      ## 1.2 Spatial Data ##
      
      # Read spatial data
      results$shapeData <- reactive({
            
            if(input$example > 0){
              
              return(
                  readShapeData(zipFile = file.path(dataDir, "allPolygons.zip"),
                      id = "SP_ID")
              )
              
            } else {
              
              validate(need(input$shapeData, "Please load spatial data"))
              
              if(input$shapeDataPolygons == "lowest") {
                
                return(
                    readShapeData(zipFile = (input$shapeData)$datapath)
                )
                
              } else {
                
                shapeData <- tryCatch({
                      
                      readShapeData(zipFile = (input$shapeData)$datapath,
                          id = "SP_ID")
                      
                    }, error = function(err) {
                      
                      err
                      
                    })
                
                validate(need(!any(attr(shapeData, "class") == "error"), 
                        shapeData))
                
                return(shapeData)
                
              }
              
            }
            
          })
      
      
      output$shapeSpatial <- renderUI({
            
            varNames <- names(results$shapeData()@data)
            
            shapeLevels <- lapply(spatialLevels, 
                function(iLevel) {
                  selectInput(paste0("shape", iLevel), iLevel, 
                      choices = c("<none>" = "none", varNames))
                })
            
            
            return(shapeLevels[1:as.numeric(input$spaceResolution)])
            
          })
      
      
      output$shapeData <- renderUI({
            
            varNames <- names(results$shapeData()@data)
            makeFunctionFields(type = "Shape")
            
            
            if(input$shapeDataPolygons == 'lowest') {
              
              list(
                  
                  strong("Select variable name for:"),
                  
                  uiOutput("shapeSpatial"),
                  selectInput("covariatesShape", "Covariates", choices = varNames, 
                      multiple = TRUE),
                  uiOutput("functionCovariatesShape"),
                  
                  busyIndicator("In progress", wait = 0),                  
                  actionButton("createAllPolygons", "Create all polygons", 
                      styleclass = "primary", size = "mini"),
                  
                  downloadButton("downloadAllPolygons", "Download")
              
              )
              
            } else {
              
              list(
                  
                  strong("Select variable name for:"),
                  
                  selectInput("covariatesShape", "Covariates", choices = varNames, 
                      multiple = TRUE),
                  uiOutput("functionCovariatesShape")
              
              )
              
            }
            
          })
      
      
      output$subsetShapeData <- renderUI({
            
            shapeNames <- row.names(results$shapeData())
            
            return(
                list(
                    
                    checkboxInput("doSubsetShape", "Select subset of the data", 
                        value = FALSE),
                    conditionalPanel("input.doSubsetShape == true",
                        selectInput("namesSubsetShape", "Keep polygons (and all its subpolygons)",
                            choices = shapeNames, multiple = TRUE)
                    
                    )
                
                )
            )
            
          })
      
      
      results$initialPolygons <- reactive({
            
            if (input$shapeDataPolygons == "lowest") {
              
              if(is.null(input$createAllPolygons))
                return(NULL)
              
              if(input$createAllPolygons == 0)
                return(NULL)
              
              input$createAllPolygons
              
              isolate({
                    
                    allPolygons <- makeUnionPolygons(shapeData = results$shapeData(), 
                        spatialNames = results$spatialLevels(),
                        covariates = input$covariatesShape,
                        functionCovariates = results$functionCovariatesShape())
                    
                  })
              
            } else {
              
              allPolygons <- results$shapeData()
              
            }
            
          })
      
      
      results$allPolygons <- reactive({
            
            validate(need(results$initialPolygons(), "Please load or create all polygons") %then%
                    need(!is.null(input$doSubsetShape), ""))
            
            if(!is.null(input$namesSubsetShape) & input$doSubsetShape) {
              
              doSelect <- grepl(pattern = paste(input$namesSubsetShape, collapse = "|"),
                  x = row.names(results$shapeData()))
              
              selectedPolygons <- selectPolygons(results$initialPolygons(), 
                  doSelect = doSelect)
              
            } else {
              
              selectedPolygons <- results$initialPolygons()
              
            }              
            
            
            return(selectedPolygons)
            
          })
      
      
      output$selectedPolygons <- renderPlot({
            
            allPolygonsLAU2 <- selectPolygons(results$initialPolygons(), spatialLevel = "LAU2")
            subsetPolygonsLAU2 <- selectPolygons(results$allPolygons(), spatialLevel = "LAU2")
            
            
            plot(allPolygonsLAU2)
            plot(subsetPolygonsLAU2, col = "gray", add = TRUE)
            
          })
      
      
      results$spatialData <- reactive({
            
            validate(need(!is.null(input$doSubsetShape), ""))
            
            shapeNames <- row.names(results$shapeData())
            doSelect <- rep(TRUE, times = length(shapeNames))
            
            if(input$doSubsetShape){
              
              if(!is.null(input$namesSubsetShape)) {
                
                doSelect <- grepl(pattern = paste(input$namesSubsetShape, collapse = "|"),
                    x = shapeNames)
                
              }              
              
            }
            
            
            if(input$shapeDataPolygons == "lowest") {
              
              allData <- results$shapeData()@data[doSelect, c(results$spatialLevels(),
                      input$covariatesShape)]
              names(allData)[names(allData) %in% results$spatialLevels()] <- 
                  spatialLevels[1:as.numeric(input$spaceResolution)]
              
              return(allData)
              
            } else {
              
              lowestNames <- shapeNames[nchar(shapeNames) == max(nchar(shapeNames))]
              selectedNames <- (shapeNames %in% lowestNames) & doSelect
              
              allData <- results$allPolygons()@data[selectedNames,]
              newName <- spatialLevels[as.numeric(input$spaceResolution)]
              allData[[newName]] <- shapeNames[selectedNames]
              
              missingLevels <- spatialLevels[1:(as.numeric(input$spaceResolution) - 1)]
              
              spatialData <- cbind(sapply(missingLevels, function(iLevel){
                        
                        switch(iLevel,
                            "NUTS0" = substring(lowestNames, first = 1, last = 2),
                            "NUTS1" = substring(lowestNames, first = 1, last = 3),
                            "NUTS2" = substring(lowestNames, first = 1, last = 4),
                            "NUTS3" = substring(lowestNames, first = 1, last = 5),
                            "LAU1" = substring(lowestNames, first = 1, last = 9)
                        )
                        
                      }), 
                  allData[, c(newName, input$covariatesShape), drop = FALSE])
              
              
              return(spatialData)
              
            }
            
          })
      
      
      
      # Show spatial data
      output$showShapeData <- DT::renderDataTable({
            
            validate(need(!is.null(input$doSubsetShape), ""))
            
            shapeNames <- row.names(results$shapeData())
            doSelect <- rep(TRUE, times = length(shapeNames))
            
            if(input$doSubsetShape){
              
              if(!is.null(input$namesSubsetShape)) {
                
                doSelect <- grepl(pattern = paste(input$namesSubsetShape, collapse = "|"),
                    x = shapeNames)
                
              }              
              
            }
            
            DT::datatable(results$shapeData()@data[doSelect,], options = list(
                    lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
                    pageLength = 10))
            
          })
      
      
      output$downloadAllPolygons <- downloadHandler(
          
          filename = function() { "allPolygons.zip" },
          content = function(file) {
            
            currentWd <- getwd()
            setwd(tempdir())
            
            unlink("allPolygons.*")
            writePolyShape(results$allPolygons(), "allPolygons")
            zip(zipfile = file, files = Sys.glob("allPolygons.*"))
            
            setwd(currentWd)
            
          })
      
      
      ## 1.3 Define Covariates ##
      
      results$covariatesData <- reactive({
            
            inFile <- input$covariatesData
            
            if(input$example > 0){
              
              read.table(file.path(dataDir, "covariatesData.csv"),
                  header = TRUE, sep = ",")
              
            } else {
              
              validate(need(inFile, "No covariates data loaded"))
              
              read.table(inFile$datapath, header = TRUE, sep = input$sepCovariates, 
                  quote = '"', stringsAsFactors = FALSE)
              
            }
            
          })
      
      
      # Show covariates data
      output$showCovariatesData <- DT::renderDataTable({
            
            DT::datatable(results$covariatesData(), options = list(
                    lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
                    pageLength = 10))
            
          })
      
      
      
      output$covariatesData <- renderUI({
            
            validate(need(results$covariatesData(), "Please load covariates data"))
            varNames <- names(results$covariatesData())
            
            makeTimeFields(type = "extra", varNames = varNames)
            makeFunctionFields(type = "Extra")
            
            list(
                
                strong("Select variable name for:"),
                
                selectInput("covariatesExtra", "Covariates",
                    choices = varNames, multiple = TRUE),
                uiOutput("functionCovariatesExtra"),
                
                selectInput("covariatesSpatial", 
                    spatialLevels[as.numeric(input$spaceResolution)],
                    choices = varNames),
                
                uiOutput("extraTime"),
                
                numericInput("lagTime", paste0("Lag time (in ", 
                        timeLevels[as.numeric(input$timeResolution)], "s)"),
                    value = 0)
            
            )
            
          })
      
      
      
      ## 1.4 Define Neighbours ##
      
      output$neighboursSpatialLevel <- renderUI({
            
            selectInput("neighboursSpatialLevel", "Adapt neighbours at spatial level",
                choices = spatialLevels[1:as.numeric(input$spaceResolution)])
            
          })
      
      
      results$initialNeighbours <- reactive({
            
            if (input$example > 0) {
              
              return(readRDS(file = file.path(dataDir, "allNeighbours.rds")))
              
            } else if(input$copyShape == "no") {
              
              inFile <- input$neighboursData
              validate(need(inFile, "Please load all neighbours data"))
              
              
              return(readRDS(file = inFile$datapath))
              
            } else {
              
              if (is.null(input$calculateNeighbours))
                return(NULL)
              
              if (input$calculateNeighbours == 0)
                return(NULL)
              
              validate(need(results$allPolygons(), "Please create all polygons first"))
              
              input$calculateNeighbours
              
              isolate({
                    
                    allNeighbours <- lapply(spatialLevels[1:as.numeric(input$spaceResolution)], 
                        function(iLevel) {
                          
                          if (iLevel == "none") {
                            
                            return(NA)
                            
                          } else {
                            
                            subsetPolygons <- selectPolygons(allPolygons = results$allPolygons(), 
                                spatialLevel = iLevel)
                            
                            if (length(subsetPolygons) < 2) {
                              
                              return(NA)
                              
                            } else {
                              
                              return(poly2nb(subsetPolygons))
                              
                            }
                            
                          }
                          
                        })
                    
                    names(allNeighbours) <- spatialLevels[1:as.numeric(input$spaceResolution)]
                    
                    return(allNeighbours)
                    
                  })
            }
            
          })
      
      
      results$neighboursMax <- reactive({
            
            validate(need(results$allPolygons(), "Please load spatial data and create all polygons") %then%
                    need(input$neighboursSpatialLevel, "") %then%
                    need(input$neighboursSpatialLevel != "none", "Please select another spatial level") %then%
                    need(input$maxDistance > 0, "Please define a distance larger than zero"))
            
            subsetPolygons <- selectPolygons(allPolygons = results$allPolygons(), 
                spatialLevel = input$neighboursSpatialLevel)
            
            validate(need(length(subsetPolygons) > 1, 
                    "Neighbours cannot be defined: Less than 2 polygons at this spatial level \n"))
            
            #http://gis.stackexchange.com/questions/83722/calculate-minimal-distance-between-polygons-in-r
            neighboursMax <- dnearneigh(coordinates(subsetPolygons), 0, 
                input$maxDistance, row.names = rownames(subsetPolygons@data), 
                longlat = TRUE)
            
            neighboursMatrix <- nb2mat(neighboursMax, style = "B", zero.policy = TRUE)
            colnames(neighboursMatrix) <- rownames(neighboursMatrix) 
            
            
            return(neighboursMatrix)
            
          })  
      
      
      observe({
            
            results$allNeighbours <- results$initialNeighbours()      
            
          })
      
      observeEvent(input$makeNeighbours, {
            
            validate(need(results$allNeighbours, 
                    "Please push the button Calculate all neighbours"))
            
            allNeighbours <- results$allNeighbours
            subsetNeighbours <- nb2mat(allNeighbours[[input$neighboursSpatialLevel]],
                style = "B", zero.policy = TRUE)
            colnames(subsetNeighbours) <- rownames(subsetNeighbours)
            
            subsetNeighbours[input$region1, input$region2] <- 1
            subsetNeighbours[input$region2, input$region1] <- 1
            
            allNeighbours[[input$neighboursSpatialLevel]] <- 
                mat2listw(subsetNeighbours)$neighbours
            
            
            results$allNeighbours <- allNeighbours
            
          })
      
      
      output$defineRegion1 <- renderUI({
            
            validate(need(results$neighboursMax(), "Loading..."))
            
            allRegions <- rownames(results$neighboursMax())
            
            
            selectInput("region1", "Region 1", choices = allRegions,
                selected = allRegions[1])
            
          })
      
      output$defineRegion2 <- renderUI({
            
            validate(need(results$allNeighbours, "Please load or calculate all neighbours") %then%
                    need(input$neighboursSpatialLevel, "Please select spatial level") %then%
                    need(results$neighboursMax(), ""))
            
            allNeighbours <- nb2mat(results$allNeighbours[[input$neighboursSpatialLevel]],
                style = "B", zero.policy = TRUE)
            
            neededCharacters <- switch(input$neighboursSpatialLevel,
                "Country" = 2,
                "NUTS3" = 5,
                "LAU1" = 9,
                "LAU2" = 13
            )
            
            validate(need(dim(allNeighbours) == dim(results$neighboursMax()), "") %then%
                    need(neededCharacters == nchar(input$region1), ""))
            
            newNeighbours <- results$neighboursMax() - allNeighbours
            
            neighbourRegions <- newNeighbours[input$region1, ]
            choices <- names(neighbourRegions[neighbourRegions == 1])
            
            validate(need(length(choices) > 0, "No new potential neighbours available. You might want to increase the maximum distance between neighbours."))
            
            selectInput("region2", "Region 2", choices = choices, 
                multiple = TRUE)
            
          })
      
      
      output$downloadNeighbours <- downloadHandler(
          
          filename = function() { "allNeighbours.rds" },
          content = function(file) {
            saveRDS(results$allNeighbours, file = file)
            
          })
      
      
      output$plotShapeData <- renderPlot({
            
            validate(need(results$allPolygons(), "Please push the button 'Create all polygons'") %then%
                    need(input$neighboursSpatialLevel, "Please select spatial level") %then%
                    need(input$neighboursSpatialLevel != "none", "Please select another spatial level"))
            
            subsetPolygons <- selectPolygons(allPolygons = results$allPolygons(),
                spatialLevel = input$neighboursSpatialLevel)
            
            plot(subsetPolygons, border = "grey")
            
            if(class(results$allNeighbours[[input$neighboursSpatialLevel]]) == "nb") {
              
              plot(results$allNeighbours[[input$neighboursSpatialLevel]], 
                  coordinates(subsetPolygons), add = TRUE)
              
            }
            
          })
      
      
      
      
      
      ## 1.5 Data for Analysis ##
      
      # Collapse data 
      results$mergedData <- reactive({
            
            if (input$example > 0) {
              
              dataDir <- system.file("extdata", package = "spatialAnalysis")
              mergedData <- read.table(file.path(dataDir, "mergedData.csv"),
                  header = TRUE, sep = ",")
              
              return(mergedData)
              
            } else {
              
              if (is.null(input$mergeData)) {
                
                return(NULL)
                
              }
              
              if (input$mergeData == 0) {
                
                return(NULL)
                
              }
              
              input$mergeData
              
              isolate({
                    
                    tryCatch({
                          
                          
                          validate(need(results$rawInfoData(), "Please load temporal data") %then%
                                  need(results$shapeData(), "Please load spatial data"))                          
                          
                          # Take subset      
                          if (input$subsetVariable != "none") {
                            
                            subsetData <- results$rawInfoData()[
                                (results$rawInfoData()[, input$subsetVariable] == input$levelSubset),]
                            
                          } else {
                            
                            subsetData <- results$rawInfoData()
                            
                          }
                          
                          # Change response levels
                          subsetData[, input$responseVariable] <- 
                              ifelse(subsetData[, input$responseVariable] == input$levelPos, 1, 0)
                          
                          if(input$timeResolution != "4") {
                            
                            timeNamesTemporal <- results$timeLevels()
                            
                          } else {
                            
                            timeNamesTemporal <- input$infoDate
                            
                          }
                          
                          
                          if (!is.null(input$covariatesExtra)) {
                            
                            validate(need(results$covariatesData(), "Please load covariates data") %then%
                                    need(!is.null(input$lagTime), "Please provide numeric value for lag time (default is 0)"))
                            
                            if(input$timeResolution != "4") {
                              
                              timeNamesExtra <- results$timeLevelsExtra()
                              
                            } else {
                              
                              timeNamesExtra <- input$extraDate
                              
                            }
                            
                          }
                          
                          
                          mergedData <- mergeAllData(temporalData = subsetData,
                              spatialData = results$spatialData(), 
                              covariatesData = results$covariatesData(),
                              responseVariable = input$responseVariable,
                              timeResolution = timeLevels[as.numeric(input$timeResolution)],
                              dateFormat = input$dateFormat,
                              spaceResolution = spatialLevels[as.numeric(input$spaceResolution)],
                              timeNamesTemporal = timeNamesTemporal,
                              spatialNameTemporal = input$infoSpatial,
                              covariatesTemporal = input$covariatesInfo,
                              functionCovariatesTemporal = results$functionCovariatesInfo(),
                              timeNamesExtra = timeNamesExtra,
                              spatialNameExtra = input$covariatesSpatial,
                              covariatesExtra = input$covariatesExtra,
                              functionCovariatesExtra = results$functionCovariatesExtra(),
                              lagTime = input$lagTime,
                              covariatesSpatial = input$covariatesSpatial)
                          
                          
                          return(mergedData)
                          
                        }, error = function(error){
                          
                          return(error)
                          
                        })
                    
                  })
            }
          })
      
      
      # Show data for analysis
      
      output$totalNumberObservations <- renderText({
            
            validate(need(results$mergedData(), 
                        "Please load all data and push the button 'Show Data for Analysis'") %then%
                    need(!any(attr(results$mergedData(), "class") == "error"), 
                        paste("Error:", results$mergedData()$message)))
            
            paste("Total number of observations:", 
                sum(results$mergedData()$numberObservations))
            
          })
      
      output$showMergedData <- DT::renderDataTable({
            
            if(is.null(results$mergedData()))
              return(NULL)
            
            if(any(attr(results$mergedData(), "class") == "error"))
              return(NULL)
            
            DT::datatable(results$mergedData(), options = list(
                    lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
                    pageLength = 10))
            
          })
      
      
      
      ## Update selected parameters for 'Example'
      observeEvent(input$example, {
            
            updateSelectInput(session, "timeResolution", selected = "3")
            updateSelectInput(session, "spaceResolution", selected = "6")
            
            
            # Temporal data
            varNames <- names(results$rawInfoData())
            updateSelectInput(session, "responseVariable", selected = "resQualValue")
            updateSelectInput(session, "levelPos", selected = "POS")
            updateSelectInput(session, "infoYear", selected = varNames[12])
            updateSelectInput(session, "infoMonth", selected = varNames[13])
            updateSelectInput(session, "infoDay", selected = varNames[14])
            
            updateSelectInput(session, "infoSpatial", selected = varNames[11])
            
            
            # Shape data
            updateSelectInput(session, "covariatesShape", 
                selected = c("LAU1Name", "LAU2Name"))
            updateSelectInput(session, "functionCovariatesShapeLAU1Name", 
                selected = "modus")
            updateSelectInput(session, "functionCovariatesShapeLAU2Name", 
                selected = "modus")
            
            
            # Covariates data
            updateSelectInput(session, "covariatesExtra", 
                selected = c("variable1","variable2"))
            
            updateSelectInput(session, "covariatesSpatial", selected = "LAU2")
            
            updateSelectInput(session, "extraYear", selected = "sampY")
            updateSelectInput(session, "extraMonth", selected = "sampM")
            updateSelectInput(session, "extraDay", selected = "sampD")
            
            
          })
      
      
      ### 2 & 3 Descriptives and analysis tabs ###
      
      source("serverDescriptives.R", local = TRUE)
      source("serverAnalysis.R", local = TRUE)
      
      
    })