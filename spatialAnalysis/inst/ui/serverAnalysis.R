
### 3. Analysis Tab ###

## 3.1 Define parameters and data for inla() ##

output$parametersInla <- renderUI({
      
      timeChoices <- switch(input$timeResolution,
          "1" = "Year",
          "2" = c("Year", "Month"),
          "3" = c("Year", "Month", "Week", "Day"),
          "4" = c("Year", "Month", "Week", "Day")
      )
      
      spaceChoices <- spatialLevels[1:as.numeric(input$spaceResolution)]
      
      list(
          
          p(strong("Binary response: "), "positive value of", 
              input$responseVariable, "(isPositive)"),
          
          selectInput("covariatesInla", "Covariate names", 
              choices = results$allCovariates(), multiple = TRUE),
          
          selectInput("spatialUnit", "Spatial unit",
              choices = spaceChoices, selected = tail(spaceChoices, n = 1)),
          
          selectInput("timeUnit", "Time unit",
              choices = timeChoices)   
      
      
      )
      
    })


observeEvent(input$example, {
      
      updateSelectInput(session, "covariatesInla", 
          selected = c("variable1", "variable2"))
      
      updateSelectInput(session, "spatialUnit", selected = "LAU2")
      
      updateSelectInput(session, "timeUnit", selected = "Month")
      
      updateSelectInput(session, "timeEffect", selected = "linear")
      
      
    })


results$inlaShapeData <- reactive({
      
      validate(need(results$allPolygons(), "Please create all polygons in the 'Load Data' tab") %then%
              need(input$spatialUnit, "Please select Spatial unit"))
      
      subsetPolygons <- selectPolygons(allPolygons = results$allPolygons(),
          spatialLevel = input$spatialUnit)
      
      
      return(subsetPolygons)
      
    })


# Save neighbours for inla()
observe({
      
      validate(need(results$allNeighbours, "Please calculate/load all neighbours"))
      
      lapply(names(results$allNeighbours), function(iName) {
            
            if(class(results$allNeighbours[[iName]]) == "nb"){
              
              saveNeighboursInla(neighboursList = results$allNeighbours[[iName]], 
                  weightStyle = "B", fileName = iName)
              
            }
            
          })
      
    })



## 3.2 Perform inla & show summary tables and graphs ## 

allTimeEffects <- c("<none>" = "none", "saturated" = "saturated", 
    "linear" = "linear", "first-order random walk (RW1)" = "rw1", 
    "first-order autoregressive (AR1)" = "ar1", 
    "second-order random walk (RW2)" = "rw2")


results$spaceTimeData <- eventReactive(input$submitInla, {
      
      validate(need(results$mergedData(),
                  "Please generate Data for Analysis in the Load Data tab") %then%
              need(input$timeUnit, "Please select Time unit") %then%
              need(input$spatialUnit, "Please select Spatial unit"))
      
      
      makeSpaceTimeData(mergedData = results$mergedData(), 
          timeNames = c("Year", "Month", "Day"), 
          timeUnit = input$timeUnit, 
          spatialUnit = input$spatialUnit,
          responsePositive = "numberPositive",
          responseTotal = "numberObservations",
          covariates = results$allCovariates(),
          functionCovariates = results$allFunctionCovariates())
      
      
    })


results$timePoints <- eventReactive(input$submitInla, {
      
      validate(need(input$timeUnit, "Please select time unit"))
      
      if (input$timeUnit == "Year") {
        
        paste0("Year_", levels(results$spaceTimeData()$timeUnit))
        
      } else if (input$timeUnit == "Month") {
        
        levels(results$spaceTimeData()$timeUnit)
        
      } else {
        
        paste0(input$timeUnit, "_", levels(results$spaceTimeData()$timeUnit))
        
      }
      
    })


results$allResultsInla <- eventReactive(input$submitInla, {
      
      validate(need(results$spaceTimeData(), "No data available"))
      
      # TODO make this active again
      
      if(FALSE){
        
        if(length(results$inlaShapeData()) < 2) {
          
          stop("**Error** Only one area available: Please select another spatial unit 
                  and push button Perform INLA again.")
          
        }
        
        # Check for complete separation
        allMeans <- tapply(results$spaceTimeData()$isPositive,
            results$spaceTimeData()$timeUnit, mean)
        
        if(all(allMeans %in% c(0, 1))) {
          
          stop("**Error** Complete separation: Please select another time unit 
                  and push button Perform INLA again")
          
        }
        
        allMeans <- tapply(results$spaceTimeData()$isPositive,
            results$spaceTimeData()$spatialUnit, mean)
        
        
        if(all(allMeans %in% c(0, 1))) {
          
          stop("**Error** Complete separation: Please select another spatial unit 
                  and push button Perform INLA again")
          
        }
        
      }
#            # Note: Error if missing values for factor covariates
#            if(!is.null(dataInla$sex)) {
#              
#              dataInla[is.na(dataInla$sex), "sex"] <- "U"
#              
#            }
#            
#            if(!is.null(dataInla$age)) {
#              
#              dataInla[is.na(dataInla$age), "age"] <- "Unknown"
#              
#            }
      
      formulaInla <- makeFormulaInla(responseName = "isPositive",
          covariateNames = input$covariatesInla,
          timeEffect = input$timeEffect,
          interactionEffect = input$interactionEffect)
      
      performedInla <- performInla(spaceTimeData = results$spaceTimeData(), 
          formulaInla = formulaInla, 
          neighboursFileName = input$spatialUnit,
          weighted = FALSE)
      
      
      if (input$timeEffect == "all") {
        
        class(performedInla) <- c(class(performedInla), "allTimeEffects")
        
      }
      
      
      return(performedInla)
      
    })



results$performedInla <- reactive({
      
      validate(need(results$allResultsInla(), "No results available") %then%
              need(input$timeEffect, "Please select time effect"))      
      
      tryCatch({
            
            if (input$timeEffect == "all") {
              
              validate(need(inherits(results$allResultsInla(), "allTimeEffects"),
                      "Please push the button Perform INLA"))
              
              return(results$allResultsInla()[[1]])
              
            } else {
              
              validate(need(!inherits(results$allResultsInla(), "allTimeEffects"),
                      "Please push the button Perform INLA"))
              
              return(results$allResultsInla())
              
            }
            
            
          }, error = function(err) {
            
            return(err)
            
          })
      
    })





# For debugging (not needed)
#output$summaryInla <- renderPrint({
#      
#      validate(need(!any(attr(results$performedInla(), "class") == "error"), 
#                  results$performedInla()$message) %then%
#              need(results$performedInla(), "No results available"))
#      
#      if (input$timeEffect == "all") {
#        
#        allModels <- with(results$performedInla(),
#            cbind(timeEffect, modelDic, logScore, converged))
#        print(allModels)
#        
#        print("Best model\n")
#        summary(results$performedInla()$inlaResult)
#        
#      } else {
#        
#        summary(results$performedInla())
#        
#      }      
#      
#    })


results$plotData <- eventReactive(input$submitInla, {
      
      validate(need(!any(attr(results$performedInla(), "class") == "error"), 
                  results$performedInla()$message) %then%
              need(results$performedInla(), "No results available"))
      
      plotData <- cbind(results$spaceTimeData()[, c("spatialUnit", "timeUnit", 
                  "dates", "numberPositive", "numberObservations",
                  results$allCovariates())], 
          results$performedInla()$summary.fitted.values)
      
      
      areNumeric <- sapply(plotData, is.numeric)
      plotData[, areNumeric] <- signif(plotData[, areNumeric], 3)
      
      return(plotData)
      
    })


results$subsetPlotData <- reactive({
      
      validate(need(results$plotData(), "No plot data available"))
      
      if(is.null(input$showTime)) {
        
        selectedTime <- results$timePoints()[1]
        
      } else {
        
        selectedTime <- input$showTime
        
      }
      
      if(is.null(input$showVariable)) {
        
        selectedVariable <- "mean"
        
      } else {
        
        selectedVariable <- input$showVariable
        
      }
      
      
      if (input$timeUnit == "Day") {
        
        selectedTime <- substr(selectedTime, start = 5, stop = nchar(selectedTime))          
        
      } else if (input$timeUnit == "Week") {
        
        selectedTime <- substr(selectedTime, start = 6, stop = nchar(selectedTime))
        
      } else if (input$timeUnit == "Year") {
        
        selectedTime <- substr(selectedTime, start = 6, stop = nchar(selectedTime))
        
      }
      
      subsetData <- results$plotData()[results$plotData()$timeUnit == selectedTime, ]
      returnData <- subsetData[, selectedVariable]
      names(returnData) <- subsetData$spatialUnit
      
      
      return(returnData)
      
    })


predictionsTime <- function() {
  
  validate(need(results$plotData(), "No plot data available"))
  
  if (input$timeUnit %in% c("Year", "Month")) {
    
    ggplot(data = results$plotData(), aes(x = timeUnit, y = mean, group = spatialUnit)) + 
        geom_line() + theme_bw()
    
  } else {
    
    ggplot(data = results$plotData(), aes(x = dates, y = mean, group = spatialUnit)) + 
        geom_line() + theme_bw()
    
  }
  
}


output$downloadPredictionsTime <- downloadHandler(
    file = "predictionsTime.png",
    content = function(file) {
      ggsave(file, plot = predictionsTime(), device = png, 
          width = 500, height = 1000, limitsize = FALSE)
    }
)


predictionsSpatial <- function() {
  
  validate(need(results$plotData(), "No plot data available"))
  
  plotDataWide <- reshape(results$plotData()[, c("timeUnit", "spatialUnit", "mean")], 
      timevar = "timeUnit", idvar = "spatialUnit", direction = "wide")
  
  colnames(plotDataWide) <- c("spatialUnit", results$timePoints())
  rownames(plotDataWide) <- plotDataWide$spatialUnit
  
#  validate(need(length(results$inlaShapeData()) == nrow(plotDataWide),
#          "Loading..."))
#  validate(need(length(results$timePoints()) == (ncol(plotDataWide) - 1), 
#          "Some time points have no results"))
  
  
  plotResult <- SpatialPolygonsDataFrame(Sr = results$inlaShapeData(), 
      data = plotDataWide)
  
  if (length(results$timePoints()) > 1) {
    
    spplot(plotResult, zcol = results$timePoints(), as.table = TRUE, 
        col.regions = brewer.pal(n = 9, name = input$colorPaletteInla), 
        cuts = 8)
    
  } else {
    
    spplot(plotResult, zcol = results$timePoints(),
        col.regions = brewer.pal(n = 9, name = input$colorPaletteInla), 
        cuts = 8)
    
  }
  
}


#output$downloadPredictionsSpatial <- downloadHandler(
#    file = "predictionsSpatial.png",
#    content = function(file) {
#      ggsave(file, plot = predictionsSpatial(), device = png, 
#          width = 500, height = 1000, limitsize = FALSE)
#    }
#)



output$summaryInla <- renderUI({
      
      validate(need(results$performedInla(), "No results available") %then%
              need(!any(attr(results$performedInla(), "class") == "error"), 
                  results$performedInla()$message))
#                    need(length(results$inlaShapeData())*length(results$timePoints()) 
#                            == nrow(results$spaceTimeData()),
#                        "Please push the button Perform INLA")
      
      if (input$timeEffect == "all") {
        
        validate(need(inherits(results$allResultsInla(), "allTimeEffects"),
                "Please push the button Perform INLA"))
        
        bestResult <- results$performedInla()
        
        bestTmp <- with(results$allResultsInla(),
            timeEffect[inlaResult$dic$dic == modelDic])
        bestName <- names(allTimeEffects)[which(allTimeEffects %in% bestTmp)]
        
        allTables <- list(tableFixed = bestResult$summary.fixed,
            tableRandom = bestResult$summary.hyperpar,
            tableDiagnostics = data.frame("DIC" = bestResult$dic$dic,
                "Effective number of parameters" = bestResult$dic$p.eff,
                "Logarithmic score" = - mean(log(bestResult$cpo$cpo), na.rm = TRUE),
                "Converged" = bestResult$mode$mode.status == 0))
        
        allModels <- with(results$allResultsInla(),
            data.frame("Time effect" = names(allTimeEffects)[which(allTimeEffects %in% timeEffect)],  
                "DIC" = signif(modelDic, 3), 
                "Logarithmic score" = signif(logScore, 3), 
                "Converged" = converged))
        
        notFitted <- results$allResultsInla()$notFitted
        
        if(length(notFitted > 0)) {
          
          textNotFitted <- em("Note: Model was not fitted for time effect:", 
                  paste(names(allTimeEffects)[which(allTimeEffects %in% notFitted)], collapse = ","))
          
        } else {
          
          textNotFitted <- NULL
          
        }
        
        
        output$allModels <- renderUI({
              
              list(
                  h4("Summary of all fitted models"),
                  renderTable(allModels),
                  downloadButton("downloadTableAllModels", "Download"),
                  tags$br(),
                  textNotFitted,
                  h3("Summary for best model with time effect: ", bestName)
              
              )
              
            })
        
        
        output$downloadTableAllModels <- downloadHandler(
            filename = function() { "INLA_AllModels.csv" },
            content = function(file) {
              write.csv(allModels, file)
            })
        
        
      } else {
        
        validate(need(!inherits(results$allResultsInla(), "allTimeEffects"),
                "Please push the button Perform INLA"))
        
        allTables <- with(results$performedInla(),
            list(tableFixed = summary.fixed,
                tableRandom = summary.hyperpar,
                tableDiagnostics = data.frame("DIC" = dic$dic,
                    "Effective number of parameters" = dic$p.eff,
                    "Logarithmic score" = - mean(log(cpo$cpo), na.rm = TRUE),
                    "Converged" = mode$mode.status == 0))
        )
        
        output$allModels <- renderUI(NULL)
        
      }
      
      sapply(c("Fixed", "Random", "Diagnostics"), function(iTable) {
            
            output[[paste0("downloadTable", iTable)]] <- downloadHandler(
                filename = function() { paste0("INLA_", iTable, ".csv") },
                content = function(file) {
                  write.csv(allTables[[paste0("table", iTable)]], file)
                })
            
            return(NULL)
            
          })
      
      return(
            
            list(
                uiOutput("allModels"),
                h4("Fixed parameters"),
                renderTable(allTables[["tableFixed"]]),
                downloadButton("downloadTableFixed", "Download"),
                h4("Random parameters"),
                renderTable(allTables[["tableRandom"]]),
                downloadButton("downloadTableRandom", "Download"),
                h4("Model diagnostics"),
                renderTable(allTables[["tableDiagnostics"]]),
                downloadButton("downloadTableDiagnostics", "Download"),
                h4(p("Mean of estimated probability for positive value of", input$responseVariable)),
                renderPlot(predictionsTime()),
#                downloadButton("downloadPredictionsTime", "Download"),
                renderPlot(predictionsSpatial())
            )
            
          )
      
    })



# Note: very nice space-time shiny app 
# https://blog.snap.uaf.edu/2015/05/27/climate-projections-by-cities-r-shiny-rcharts-leaflet/

## 3.3 Show inla results on interactive map ##

output$warningsInla <- renderUI({
      
      validate(need(results$performedInla(), "Please push the button Perform INLA"))
      
      list(
          
          busyIndicator("In progress", wait = 0),
          actionButton(inputId = "showMapInla", label = "Show interactive plots", 
              styleclass = "primary", size = "mini")
      
      )
      
    })


showVariables <- c("Mean" = "mean", "Standard deviation" = "sd", 
    "2.5% quantile" = "0.025quant", "Median" = "0.5quant", 
    "97.5% quantile" = "0.975quant", "Modus" = "mode")

output$controlMapInla <- renderUI({
      
      if(is.null(input$showMapInla))
        return(NULL)
      
      if(input$showMapInla > 0){
        
        wellPanel(
            
            strong("For estimated probabilities, show:"),
            fluidRow(
                column(3, selectInput("showVariable", "Statistic",
                        choices = showVariables)
                ),
                column(3, selectInput("showTime", "at time", 
                        choices = results$timePoints())
                ),
                column(3, selectInput("showRegion", "for region",
                        choices = levels(results$plotData()$spatialUnit), 
                        multiple = TRUE))
            ),
            
            fluidRow(
                column(3, selectInput("popupVariablesInla", "Extra variables displayed in popup",
                        choices = names(results$plotData()), multiple = TRUE)
                ),
                column(3, selectInput("legendPlacementInla", "Legend placement",
                        choices = c("<none>" = "none", "topright", "bottomright", 
                            "bottomleft", "topleft"))
                )
            ),
            
            actionLink("providerTilesInla", label = "Add world map",
                icon = icon("globe"))
        
        )
        
      }
      
    })



observe({
      
      event <- input$mapInla_shape_click
      
      if(!is.null(event)){
        
        currentSelected <- isolate(input$showRegion)
        
        if(event$id %in% currentSelected){
          
          updateSelectInput(session, "showRegion", 
              selected = currentSelected[ - which(currentSelected == event$id)])
          
        } else {
          
          updateSelectInput(session, "showRegion", 
              selected = c(currentSelected, event$id))
          
        }
        
      }
      
    })


results$textPopupInla <- reactive({
      
      validate(need(input$timeUnit, "Please select time unit"))
      
      extraVariables <- ""
      
      selectedTime <- input$showTime
      
      if (input$timeUnit == "Day") {
        
        selectedTime <- substr(selectedTime, start = 5, stop = nchar(selectedTime))          
        
      } else if (input$timeUnit == "Week") {
        
        selectedTime <- substr(selectedTime, start = 6, stop = nchar(selectedTime))
        
      } else if (input$timeUnit == "Year") {
        
        selectedTime <- substr(selectedTime, start = 6, stop = nchar(selectedTime))
        
      }
      
      
      if (!is.null(input$popupVariablesInla)) {
        
        validate(need(input$popupVariablesInla %in% names(results$plotData()), 
                "These variables are not included in the data"))
        
        for(iName in input$popupVariablesInla){
          
          extraVariables <- paste(extraVariables,
              "<br> <b>", iName, "</b>: ", 
              results$plotData()[results$plotData()$timeUnit == selectedTime, iName])
          
        }
        
      }
      
      
      regionNames <- names(results$subsetPlotData())
      
      validate(need(input$showVariable, "Please select variable to show analysis results") %then% 
              need(input$showTime, "Please select time to show analysis results"))
      
      
      
      textPopup <- paste0("<h4>", regionNames, "</h4>",  
          "<strong>", names(showVariables)[showVariables == input$showVariable], 
          " of estimated probabilities in ", input$showTime, "</strong>: ", 
          round(results$subsetPlotData(), 2),
          "<br>", extraVariables
      )
      
      
      return(textPopup)
      
    })


results$mapInla <- reactive({
      
      validate(need(results$subsetPlotData(), "No plot data available") %then%
              need(length(results$inlaShapeData()) == length(results$subsetPlotData()),
                  "Loading..."))
      
      palette <- colorNumeric(palette = input$colorPaletteInla, 
          domain = range(results$subsetPlotData()))
      
      
      leaflet(results$inlaShapeData()) %>%
          
          addPolygons(
              weight = 1, 
              color = "white",
              fillColor = ~ palette(results$subsetPlotData()),
              fillOpacity = 0.8,
              layerId = names(results$subsetPlotData()),
              group = "region"
          ) 
      
    })


output$mapInla <- renderLeaflet({
      
#      if(is.null(input$showMapInla))
#        return(NULL)
#      
#      if(input$showMapInla > 0){
#        
#        results$mapInla()
#        
#      }
      
      validate(need(input$submitInla, "Please push the button Perform INLA") %then%
              need(input$submitInla > 0, "Please push the button Perform INLA") %then%
              need(input$showMapInla, "Please push the button Show interactive plots") %then%
          need(input$showMapInla > 0, "Please push the button Show interactive plots"))
  
  results$mapInla()
  
    })

# TODO save leaflet map
#output$downloadPlotTime <- downloadHandler("inlaPlotSpace.html",
#    content = function(file) {
#      
#      saveWidget(widget = leaflet(results$inlaShapeData()), 
#          file = file.path(tempdir(), "inlaPlotSpace.html"))
#      webshot(file.path(tempdir(), "inlaPlotSpace.html"), file = file, 
#          cliprect = "viewport")
#      
#    }
#)



# Plot polygons
observe({
      
      validate(need(length(results$inlaShapeData()) == length(results$subsetPlotData()),
              "Loading..."))
      
      palette <- colorNumeric(palette = input$colorPaletteInla, 
          domain = range(results$subsetPlotData()))
      
      leafletProxy("mapInla", data = results$inlaShapeData()) %>%
          
          clearShapes() %>%
          
          addPolygons(
              weight = 1,
              color = "white",
              fillColor = ~ palette(results$subsetPlotData()),
              fillOpacity = 0.8,
              layerId = names(results$subsetPlotData()),
              group = "region"
          ) 
      
    })


# Plot thick border for selected regions
observe({
      
      validate(need(length(results$inlaShapeData()) == length(results$subsetPlotData()),
              "Loading..."))
      
      if(!is.null(input$showRegion)){
        
        selectedPolygons <- subset(results$inlaShapeData(), 
            names(results$subsetPlotData()) %in% input$showRegion)
        
        leafletProxy("mapInla", data = results$inlaShapeData()) %>%
            
            clearGroup(group = "regionLines") %>%
            
            addPolylines(data = selectedPolygons, color = "white", weight = 5,
                group = "regionLines")
        
      }
      
    })


# Add world map
observe({
      
      proxy <- leafletProxy("mapInla", data = results$inlaShapeData())
      
      if(!is.null(input$providerTilesInla) & !is.null(proxy)){
        
        if(input$providerTilesInla %% 2 == 1){
          
          proxy %>% addProviderTiles("Hydda.Full")
          
        } else {
          
          proxy %>% clearTiles()
          
        }
        
      }
      
    })


# Add legend
observe({
      
      if(!is.null(input$legendPlacementInla)){
        
        validate(need(length(results$inlaShapeData()) == length(results$subsetPlotData()),
                "Loading..."))
        
        proxy <- leafletProxy("mapInla", data = results$inlaShapeData())
        
        proxy %>% removeControl(layerId = "legend")
        
        if(input$legendPlacementInla != "none"){
          
          validate(need(input$showTime, "Please select time point to show results for"))
          
          proxy %>% addLegend(position = input$legendPlacementInla,
              pal = colorNumeric(palette = input$colorPaletteInla, 
                  domain = range(results$subsetPlotData())), 
              values = results$subsetPlotData(),
              opacity = 0.8,
              title = "Legend",
              layerId = "legend"
          )
          
        } 
        
      }
      
    })


# Add popups
observe({
      
      currentMap <- leafletProxy("mapInla", data = results$inlaShapeData()) 
      currentMap %>% clearPopups()
      
      event <- input$mapInla_shape_click
      
      if(!is.null(event)){
        
        textSelected <- results$textPopupInla()[names(results$subsetPlotData()) == event$id]
        
        isolate({
              
              currentMap %>% 
                  addPopups(event$lng, event$lat, popup = textSelected)
              
            }) 
        
      }
      
    })


# Interactive time plot
output$interactiveTime <- renderChart2({
      
      validate(need(results$plotData(), "No plot data available") %then%
              need(input$showMapInla, "") %then%
              need(input$showMapInla > 0, "") %then%
              need(input$showRegion, "Please select region(s) to show results"))
      
      plotPredictions <- Highcharts$new()
      
      plotPredictions$xAxis(categories = results$timePoints())
#      plotPredictions$yAxis(min = 0, max = 1)
#      plotPredictions$chart(width = 600)
      
      plotPredictions$title(text = paste(names(showVariables)[showVariables == input$showVariable],
              "of estimated probabilities"))
      
      currentData <- results$plotData()
      currentData$y <- currentData[, input$showVariable]      
      
      
      plotPredictions$series(
          lapply(input$showRegion, function(iArea) {
                
                list(name = iArea, data = toJSONArray2(currentData[
                            currentData$spatialUnit == iArea, ], json = F))
                
              })
      )
      
      if(!is.null(input$popupVariablesInla)){
        
        textTooltip <- paste("<b> {point.spatialUnit} </b> <br> <br>",
            paste0(input$popupVariablesInla, ": {point.", input$popupVariablesInla, "}",
                collapse = "<br>")) 
        plotPredictions$tooltip(useHTML = TRUE, pointFormat = textTooltip)
        
      }
      
      plotPredictions$save(file.path(tempdir(), "inlaPlotTime.html"),
          standalone = TRUE)
      
      plotPredictions
      
      return(plotPredictions)
      
    })


output$downloadPlotTime <- downloadHandler("inlaPlotTime.html",
    content = function(file) {
      file.copy(file.path(tempdir(), "inlaPlotTime.html"),
          file)
    }
)


output$titleInteractivePlot <- renderText({
      
      if(is.null(input$showMapInla))
        return(NULL)
      
      if(input$showMapInla > 0){
        
        h4(paste(names(showVariables)[showVariables == input$showVariable],
                "of estimated probability for positive value of", 
                input$responseVariable, "in", input$showTime))
      }
      
    })

output$downloadInla <- renderUI({
      
      if(is.null(input$showMapInla))
        return(NULL)
      
      if(input$showMapInla > 0){
        
        fluidRow(
            column(6, 
                downloadButton("downloadPlotSpace", "Download")
            ),
            column(6, 
                downloadButton("downloadPlotTime", "Download")
            )
        )
        
      }
      
    })