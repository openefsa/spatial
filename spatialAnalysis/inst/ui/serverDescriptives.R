
### 2. Descriptives Tab ###

## 2.1 Input parameters (wellPanel) ##

output$descriptiveLevels <- renderUI({
      
      timeChoices <- switch(input$timeResolution,
          "1" = "Year",
          "2" = c("Year", "Month"),
          "3" = c("Year", "Month", "Week", "Day"),
          "4" = c("Year", "Month", "Week", "Day")
      )
      
      spaceChoices <- spatialLevels[1:as.numeric(input$spaceResolution)]
      
      
      fluidRow(
          
          column(6, selectInput("descriptiveSpatial", "Spatial level",
                  choices = spaceChoices, selected = tail(spaceChoices, n = 1))),
          
          conditionalPanel("input.calculate == 'kriging'",
              
              column(6, selectInput("descriptiveTime", "Time level",
                      choices = timeChoices))
          
          
          )
      )
      
    })


# Select shape and neighbours at specified spatial level
results$descriptiveShapeData <- reactive({
      
      validate(need(results$allPolygons(), "Please create all polygons in the Load Data tab") %then%
              need(input$descriptiveSpatial, "Please select spatial level"))
      
      subsetPolygons <- selectPolygons(allPolygons = results$allPolygons(),
          spatialLevel = input$descriptiveSpatial)
      
      
      return(subsetPolygons)
      
    })


results$neighboursWeights <- reactive({
      
      validate(need(results$allNeighbours, "Please calculate all neighbours 
                      in the Load Data tab") %then%
              need(input$descriptiveSpatial, "Please select spatial level"))
      
      
      return(results$allNeighbours[[input$descriptiveSpatial]])
      
    })



results$descriptiveData <- reactive({
      
      validate(need(results$mergedData(),
                  "Please generate Data for Analysis in the Load Data tab") %then%
              need(!is.null(input$descriptiveSpatial), 
                  "Please select spatial level for descriptives"))
      
      inputData <- results$mergedData()
      
      tryCatch({
            
            # Collapse by space
            allLevels <- spatialLevels[1:as.numeric(input$spaceResolution)]
            index <- which(input$descriptiveSpatial == allLevels)
            
            if(index < length(allLevels)){
              
              inputData <- inputData[, !(names(inputData) %in% 
                        allLevels[(index + 1) : length(allLevels)])]
              
            }
            
            
            if (input$calculate == "kriging") {
              
              validate(need(!is.null(input$descriptiveTime), 
                      "Please select time level for descriptives"))
              
              # Collapse and extend by time
              descriptiveData <- makeSpaceTimeData(mergedData = inputData, 
                  timeNames = c("Year", "Month", "Day"), 
                  timeUnit = input$descriptiveTime, 
                  spatialUnit = input$descriptiveSpatial,
                  inlaVariables = FALSE,
                  responsePositive = "numberPositive",
                  responseTotal = "numberObservations",
                  covariates = results$allCovariates(),
                  functionCovariates = results$allFunctionCovariates())
              
            } else {
              
              if (input$timeResolution != "4") {
                
                timeVariables <- timeLevels[1:as.numeric(input$timeResolution)]
                
              } else {
                
                timeVariables <- timeLevels[-4]
                
              }
              
              inputData <- inputData[, !(names(inputData) %in% timeVariables)]
              
              descriptiveData <- collapseData(inputData = inputData,
                  collapseVariable = input$descriptiveSpatial, 
                  responsePositive = "numberPositive",
                  responseTotal = "numberObservations",
                  covariates = results$allCovariates(), 
                  functionCovariates = results$allFunctionCovariates())
              
            }
            
            return(descriptiveData)
            
          }, error = function(error){
            
            return(error)
            
          })
      
    })




results$varNames <- reactive({
      
      validate(need(!any(attr(results$descriptiveData(), "class") == "error"), 
              paste("Error:", results$descriptiveData()$message,
                  "\nYou might need to choose another spatial level")))
      
      names(results$descriptiveData())
      
    })



output$selectVariable1 <- renderUI({
      
      selectInput("variable1", label = "Variable 1", 
          choices = results$varNames(), tail(results$varNames(), n = 1))
      
    })

output$selectVariable2 <- renderUI({
      
      selectInput("variable2", label = "Variable 2", 
          choices = c("<none>" = "none", results$varNames()))
      
    })

results$valuesVariable1 <- reactive({
      
      if (is.null(results$descriptiveData()) | is.null(input$variable1)) {
        
        return(NULL)
        
      }
      
      valuesVariable1 <- results$descriptiveData()[, input$variable1]
      
      validate(need(is.numeric(valuesVariable1), 
              "Please select a numeric 'Variable 1'"))
      
      return(valuesVariable1)
      
    })


results$valuesVariable2 <- reactive({
      
      if(is.null(input$variable2)){
        
        return(NULL)
        
      } else if (input$variable2 == "none"){
        
        return(0.8)
        
      } else {
        
        valuesVariable2 <- results$descriptiveData()[, input$variable2]
        
        validate(need(is.numeric(valuesVariable2), 
                "Please select a numeric 'Variable 2'"))
        
        if (input$calculate == "regression") {
          
          return(valuesVariable2)
          
        } else {
          
          rescale <- function(x){
            
            (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
            
          } 
          
          return(rescale(valuesVariable2))
          
        }
        
      }
      
    })


observe({
      
      if(input$calculate %in% c("moran", "geary")){
        
        updateSelectInput(session, "optionBreaks", 
            choices = c("Quantiles" = "quantile",
                "Natural (Jenks)" = "jenks",
                "Manual input - lower bounds" = "fixedLower",
                "Manual input - upper bounds" = "fixedUpper",
                "Quadrants" = "quadrant"))
        
      }
      
      if(input$example > 0){
        
        updateSelectInput(session, "variable1", 
            selected = "percentagePositive")
        
        updateSelectInput(session, "variable2", 
            selected = "none")
        
      } 
      
    })


results$nClasses <- reactive({
      
      if(input$optionBreaks == "quadrant"){
        
        4
        
      } else {
        
        input$nClasses
        
      }
      
    })

output$variogramParameters <- renderHotable({ 
      
      validate(need(results$descriptiveData(), "Please load data"))
      timeUnits <- unique(results$descriptiveData()[, "timeUnit"])
      timeUnits <- timeUnits[!is.na(timeUnits)]
      parameterValues <- data.frame(matrix(0, nrow = length(timeUnits), ncol = 4))
      parameterValues[, 1] <- timeUnits
      colnames(parameterValues) <- c("Time unit", "(Partial) sill", "Range", "Nugget")
      
      return(parameterValues)
      
      
    }, readOnly = FALSE)  


# Manual input of class labels and breaks
output$manualBreaks <- renderUI({
      
      manualBreaks <- list()
      
      if(input$optionBreaks %in% c("fixedLower", "fixedUpper")) {
        
        labelBound <- switch(input$optionBreaks, "fixedLower" = "Lower bound",
            "fixedUpper" = "Upper bound")
        
        for(i in seq_len(results$nClasses())) {
          
          manualBreaks[[i]] <- fluidRow(
              column(6, textInput(paste0("classLabel", i), label = "Name", 
                      value = paste("Class", i))),
              column(6, numericInput(paste0("classBound", i), label = labelBound,
                      
                      value = NA))
          )
          
        }
        
      } else if(input$optionBreaks == "quadrant") {
        
        levelNames <- c("High-High", "High-Low", "Low-High", "Low-Low")
        
        for(i in seq_len(results$nClasses())) {
          
          manualBreaks[[i]] <- textInput(paste0("classLabel", i), label = "Name", 
              value = levelNames[i])
          
        }
        
      } else {
        
        for(i in seq_len(results$nClasses())) {
          
          manualBreaks[[i]] <- textInput(paste0("classLabel", i), label = "Name", 
              value = paste("Class", i))
          
        }
        
      }
      
      manualBreaks
      
    })


# Manual input of colors
output$manualColors <- renderUI({
      
      manualColors <- list()
      
      if(input$colorPalette == "fixed") {
        
        for(i in seq_len(results$nClasses())) {
          
          manualColors[[i]] <- colourInput(paste0("classColor", i), 
              label = "Colour", showColour = "background")
          
        }
        
      } else {
        
        factorColor <- brewer.pal(n = results$nClasses(), 
            name = input$colorPalette)
        
        validate(need(length(factorColor) >= results$nClasses(),
                "The chosen color palette has not enough colors. 
                    \nPlease choose another palette or define colors yourself (Manual input)"))
        #other palettes: http://www.r-bloggers.com/choosing-colour-palettes-part-ii-educated-choices/
        
        for(i in seq_len(results$nClasses())) {
          
          manualColors[[i]] <- colourInput(paste0("classColor", i),
              value = factorColor[i],
              label = "Colour", showColour = "background")
          
        }
        
      }
      
      manualColors  
      
    })


output$classMissing <- renderUI({
      
      if(input$classMissing){
        
        fluidRow(
            column(9, textInput("classLabelMissing", "Name", value = "Missing")),
            column(3, colourInput("classColorMissing", 
                    label = "Colour", showColour = "background"))
        )
        
      }
      
    })


# Manual input: auto-complete with min/max observed value
observe({
      
      if(input$optionBreaks == "fixedLower"){
        
        updateNumericInput(session, "classBound1",
            value = min(results$valuesVariable1(), na.rm = TRUE))
        
        updateRadioButtons(session, "intervalClosure", selected = "left")
        
      }
      
      if(input$optionBreaks == "fixedUpper"){
        
        updateNumericInput(session, paste0("classBound", results$nClasses()),
            value = max(results$valuesVariable1(), na.rm = TRUE))
        
        updateRadioButtons(session, "intervalClosure", selected = "right")
        
      }
      
    })



results$manualInput <- reactive({
      
      validate(need(input$classColor1, "No colors defined"))
      
      if(input$optionBreaks %in% c("fixedLower", "fixedUpper")){
        
        validate(need(input$classBound2, "No bounds defined"))
        
        manualInput <- data.frame(matrix(nrow = results$nClasses(), ncol = 3))
        names(manualInput) <- c("classColor", "classLabel", "classBound")
        
      } else {
        
        manualInput <- data.frame(matrix(nrow = results$nClasses(), ncol = 2))
        names(manualInput) <- c("classColor", "classLabel")
        
      }
      
      local({
            
            for(i in 1:results$nClasses()) {
              
              i <- i
              if(!is.null(input[[paste0("classColor", i)]])){
                manualInput[i,1] <- input[[paste0("classColor", i)]]
              }
              if(!is.null(input[[paste0("classLabel", i)]])){
                manualInput[i,2] <- input[[paste0("classLabel", i)]]
              }
              
              if(input$optionBreaks %in% c("fixedLower", "fixedUpper")){
                
                if(!is.null(input[[paste0("classBound", i)]])){
                  manualInput[i,3] <- input[[paste0("classBound", i)]]
                }
                
              }
            }
            
            
            if(input$classMissing){
              
              if(input$optionBreaks %in% c("fixedLower", "fixedUpper")){
                
                manualInput <- rbind(manualInput, 
                    c(input$classColorMissing, input$classLabelMissing, NA))
                
              } else {
                
                manualInput <- rbind(manualInput, 
                    c(input$classColorMissing, input$classLabelMissing))
                
              } 
              
            }
            
            manualInput
            
          })
      
    })



## 2.2 Show results descriptives ##

results$summaryData <- eventReactive(input$doCalculate, {
      
      
      if(input$calculate == "kriging"){
        
        if(input$variogramModel == "auto"){
          
          
          krigingResult <- tapply(results$descriptiveData()[, input$variable1], 
              results$descriptiveData()[, "timeUnit"],
              function(response){
                
                performKriging(response = response, 
                    shapeData = results$descriptiveShapeData())
                
              })
          
        } else {
          
          variogramParameters <- hot.to.df(input$variogramParameters)
          timeUnits <- variogramParameters[, 1]
          krigingResult <- list()
          
          for (timeUnit in timeUnits){
            
            response <- results$descriptiveData()[(results$descriptiveData()[, "timeUnit"] == timeUnit),
                input$variable1]
            idRow <- which(variogramParameters[, "Time unit"] == timeUnit)
            variogramModel <- vgm(psill = variogramParameters[idRow, "(Partial) sill"],
                model = input$variogramModel, 
                range = variogramParameters[idRow, "Range"],
                nugget = variogramParameters[idRow, "Nugget"])
            
            krigingResult[[timeUnit]] <- performKriging(response = response, 
                shapeData = results$descriptiveShapeData(),
                variogramModel = variogramModel)
            
          }
          
        }
        
#        krigingResult <- performKriging(response = results$valuesVariable1(),
#            shapeData = results$descriptiveShapeData())
        
        class(krigingResult) <- c(class(krigingResult), "kriging")
        
        return(krigingResult)
        
      } else if(input$calculate == "regression"){
        
        validate(need(input$variable2 != "none", 
                "Please select a name for 'Variable 2'"))
        
        if(all(unique(results$valuesVariable1()[!is.na(results$valuesVariable1())])[1:2] == c(0, 1))){
          
          family = "binomial"
          
        } else {
          
          family = "gaussian"
          
        }
        
        glmFormula <- as.formula(paste(input$variable1, "~", input$variable2))
        regressionResult <- glm(glmFormula, family = family, 
            data = results$descriptiveData())
        
        
        return(regressionResult)
        
      } else {
        
        optionBreaks <- input$optionBreaks
        
        validate(need(!any(is.na(results$manualInput())), 
                "Please provide values for class input"))
        
        # TODO test manual input lower bounds and missing values class
        
        if (input$optionBreaks == "fixedLower") {
          
          optionBreaks <- "fixed"
          fixedBreaks <- c(results$manualInput()$classBound, 
              max(results$valuesVariable1(), na.rm = TRUE))
          
          validate(need(!any(duplicated(tail(fixedBreaks, 2))), 
                      paste("The largest lower bound needs to differ from the maximum value", 
                          max(results$valuesVariable1(), na.rm = TRUE))) %then%
                  need(!any(duplicated(fixedBreaks)), 
                      "Please provide unique lower bounds"))
          
        } else if (input$optionBreaks == "fixedUpper") {
          
          optionBreaks <- "fixed"
          fixedBreaks <- c(min(results$valuesVariable1(), na.rm = TRUE), 
              results$manualInput()$classBound)
          
          validate(need(!any(duplicated(head(fixedBreaks, 2))), 
                      paste("The lowest upper bound needs to differ from the minimum value", 
                          min(results$valuesVariable1(), na.rm = TRUE))) %then%
                  need(!any(duplicated(fixedBreaks)), 
                      "Please provide unique upper bounds"))
          
        }
        
        if(input$calculate %in% c("moran", "geary")){
          
          suppressWarnings(
              localValues <- calculateLocal(response = results$valuesVariable1(), 
                  neighboursList = results$neighboursWeights(),
                  localMethod = input$calculate)
          )
          
        } 
        
        # TODO class with one value?
        if (input$calculate %in% c("moran", "geary") & 
            input$optionBreaks != "quadrant"){
          
          factorResponse <- createClasses(response = localValues$statistics, 
              breaksStyle = optionBreaks, nClasses = results$nClasses(), 
              fixedBreaks = fixedBreaks, 
              intervalClosure = input$intervalClosure,
              localValues = localValues)
          
        } else {
          
          factorResponse <- createClasses(response = results$valuesVariable1(), 
              breaksStyle = optionBreaks, nClasses = results$nClasses(), 
              fixedBreaks = fixedBreaks, 
              intervalClosure = input$intervalClosure,
              localValues = localValues)
          
        }          
        
        allClasses <- 1:nlevels(factorResponse)
        bounds <- levels(factorResponse)
        
        if(input$classMissing){
          
          allClasses <- c(allClasses, nrow(results$manualInput()))
          bounds <- c(bounds, "NA")
          factorResponse <- addNA(factorResponse)
          
        }
        
        levels(factorResponse) <- results$manualInput()$classLabel[allClasses]
        color <- results$manualInput()$classColor[allClasses]
        
        return(list(value = factorResponse, bounds = bounds,
                color = color))
        
      }
      
    })



output$warningBreaks <- renderUI({
      
      toPrint <- list()
      
      if(input$optionBreaks %in% c("fixedLower", "fixedUpper")){
        
        if((input$manualBreaks) & 
            !is.null(results$manualInput()$classBound)){
          
          allBounds <- results$manualInput()$classBound
          
          if(input$optionBreaks == "fixedLower" & 
              min(results$valuesVariable1(), na.rm = TRUE) < allBounds[1]) {
            
            toPrint <- list(
                tags$em("Warning: Response range exceeds defined breaks"),
                tags$br())
          }
          
          if(input$optionBreaks == "fixedUpper" & 
              max(results$valuesVariable1(), na.rm = TRUE) > tail(allBounds, 1)){
            
            toPrint <- list(
                tags$em("Warning: Response range exceeds defined breaks"),
                tags$br())
            
          }
          
        }
        
      }
      
      if(input$classMissing){
        
        if(results$nClasses() + 1 > nlevels(results$summaryData()$value)){
          
          toPrint <- list(toPrint,
              em("Warning: The actual number of classes is smaller than defined"))
          
        }
        
      } else {
        
        if(results$nClasses() > nlevels(results$summaryData()$value)){
          
          toPrint <- list(toPrint,
              em("Warning: The actual number of classes is smaller than defined"))
          
        }
        
      }
      
      toPrint
      
    })



output$showVariable1 <- renderPlot({
      
      input$doCalculate
      
      isolate({
            
            validate(need(input$calculate, "") %then%
                    need(results$summaryData(), "No data available") %then%
                    need(!inherits(results$summaryData(), "kriging"),
                        "Please push the button 'Calculate'"))
            
            if (input$calculate == "regression") {
              
              xRange <- range(results$valuesVariable2())
              xValues <- data.frame(valuesVariable2 = seq(xRange[1], xRange[2], 
                      length.out = 1000))
              names(xValues) <- input$variable2
              yValues <- predict(results$summaryData(), newdata = xValues, 
                  type = "response")
              
              plot(results$valuesVariable2(), results$valuesVariable1(),
                  pch = 19, xlab = input$variable2, ylab = input$variable1)
              lines(xValues[,1], yValues)
              
              
            } else {
              
              classTable <- table(results$summaryData()$value)
              tmpPlot <- barplot(classTable, main = input$variable1,
                  col = results$summaryData()$color, las = 1, ylab = "Frequency")
              percentages <- paste0(round(classTable/sum(classTable)*100, 2), "%")
              
              text(tmpPlot, 0, percentages, pos = 3)
              tmpPlot
              
              if(length(tmpPlot) == length(results$summaryData()$bounds)){
                
                axis(1, at = tmpPlot, tick = FALSE, line = 2,
                    labels = results$summaryData()$bounds)
                
              }
              
            }       
            
          })
      
    })


output$showVariable2 <- renderPlot({
      
      input$doCalculate
      
      isolate({
            
            validate(need(input$variable2, ""))
            
            if(input$variable2 == "none"){
              
              return(NULL)
              
            } else {
              
              validate(need(results$valuesVariable2(), 
                      label = "No data for variable 2 available"))
              
              hist(100*results$valuesVariable2(), 
                  main = input$variable2, xlab = "Rescaled values (percentages)", 
                  col = "lightgray", las = 1)  
              
            }
            
          })
      
    })




# For Kriging
output$showKriging <- renderUI({
      
      input$doCalculate
      
      isolate({
            
            if(input$calculate != "kriging"){
              
              return(NULL)
              
            } else {
              
              krigingResult <- results$summaryData()
              
              validate(need(inherits(krigingResult, "kriging"),
                      "Please push the button 'Calculate'"))
              
              plotResult <- krigingResult[[1]]$krigingPredictions
              
              for (i in seq_along(krigingResult)) {
                
                if(all(krigingResult[[i]]$krigingPredictions[["var1.pred"]] %in% 
                        c(0, NA))) {
                  
                  plotResult[[names(krigingResult)[i]]] <- NULL
                  
                } else {
                  
                  plotResult[[names(krigingResult)[i]]] <- 
                      krigingResult[[i]]$krigingPredictions[["var1.pred"]]
                  
                }
                
              }
              
              variogramList <- lapply(names(krigingResult), function(iName) {
                    
                    renderPlot({
                          
                          validate(need(!all(krigingResult[[iName]]$gstatVariogram$gamma == 0), 
                                  paste("No results available for", iName)))
                          
                          plot(krigingResult[[iName]]$gstatVariogram, 
                              krigingResult[[iName]]$variogramModel,
                              main = iName)
                          
                        })
                    
                  })
              
              # TODO plotNames <- names(plotResult) ??
              plotNames <- names(krigingResult)[names(krigingResult) %in% names(plotResult)]
              
              if(length(plotNames) > 1){
                
                maps <- spplot(plotResult, zcol = plotNames, as.table = TRUE, 
                    col.regions = brewer.pal(n = 9, name = input$colorPaletteKriging),
                    cuts = 8)
                
              } else {
                
                maps <- spplot(krigingResult[[plotNames]]$krigingPredictions, 
                    zcol = "var1.pred",
                    col.regions = brewer.pal(n = 9, name = input$colorPaletteKriging),
                    cuts = 8)
                
              }
              
              return(
                  list(
                      fluidRow(
                          column(6, variogramList[c(TRUE, FALSE)]),
                          column(6, variogramList[c(FALSE, TRUE)])
                      ),
                      renderPlot(maps)
                  )
              )
              
            }
            
          })
      
    })



results$parametersVariogram <- eventReactive(input$doCalculate, {
      
      if(input$calculate != "kriging"){
        
        return(NULL)
        
      } else {
        
        validate(need(inherits(results$summaryData(), "kriging"),
                "Please push the button 'Calculate'"))        
        
        timeUnits <- names(results$summaryData())
        
        parameterValues <- t(sapply(seq_along(timeUnits), function(i){
                  
                  c(c("Nugget", "Exponential", "Spherical", "Gaussian")[
                          results$summaryData()[[i]]$variogramModel[2, "model"]],
                      signif(c(results$summaryData()[[i]]$variogramModel[2, "psill"],
                              results$summaryData()[[i]]$variogramModel[2, "range"], 
                              results$summaryData()[[i]]$variogramModel[1, "psill"]), 5))
                  
                }))
        
        colnames(parameterValues) <- c("Model", "(Partial) sill", "Range", "Nugget")
        rownames(parameterValues) <- timeUnits
        
        
        return(parameterValues)
        
      }
      
    })


# For linear/logistic regression or Kriging
output$summaryAnalysis <- renderUI({
      
      input$doCalculate
      
      isolate({
            
            if(input$calculate == "kriging"){
              
              validate(need(results$parametersVariogram(), 
                      "Please push the button 'Calculate'"))
              
              return(
                  list(
                      h4("Variogram models"),
                      renderTable(results$parametersVariogram())
                  )
              )
              
            } else if (input$calculate == "regression") {
              
              return(
                  list(
                      h4("Linear/Logistic regression model"),
                      renderPrint(summary(results$summaryData()))
                  )
              )
              
            } else {
              
              return(NULL)
              
            }
            
          })
      
    })


## 2.3 Show descriptives on map ##

output$warnings <- renderUI({
      
      validate(need(input$calculate != "regression", 
                  "No map is shown for Linear/Logistic regression") %then%
              need(results$summaryData(), "Please push the button Calculate")
      )
      
      list(
          busyIndicator("In progress", wait = 0),
          actionButton(inputId = "showMap", label = "Show interactive map", 
              styleclass = "primary", size = "mini")
      )
      
    })

output$controlMap <- renderUI({
      
      if(is.null(input$showMap))
        return(NULL)
      
      if(input$showMap > 0){
        
        wellPanel(
            
            actionLink("providerTiles", label = "Add world map",
                icon = icon("globe")),
            
            conditionalPanel("input.calculate == 'kriging'",
                
                list(
                    
                    strong("Show kriging results for:"),
                    fluidRow(
                        column(6,
                            selectInput("variableKriging", "Variable",
                                choices = c("Predicted response" = "var1.pred",
                                    "Variance" = "var1.var",
                                    "Standard deviation" = "var1.stdev"))
                        ),
                        column(6,
                            selectInput("timeUnitKriging", "at time",
                                choices = names(results$summaryData()))
                        )
                    
                    )
                )
            ),
            
            selectInput("legendPlacement", "Legend placement",
                choices = c("<none>" = "none", "topright", "bottomright", 
                    "bottomleft", "topleft")),
            
            selectInput("popupVariables", "Extra variables displayed in popup",
                choices = results$varNames(), multiple = TRUE)                  
        
        )
        
      }
      
    })


results$popupData <- eventReactive(results$descriptiveData(), {
      
      popupData <- results$descriptiveData()
      
      areNumeric <- sapply(popupData, is.numeric)
      popupData[, areNumeric] <- signif(popupData[, areNumeric], 3)
 
      
      return(popupData)
      
    })


results$textPopup <- reactive({
      
      extraVariables <- ""
      
      if (!is.null(input$popupVariables)) {
        
        validate(need(input$popupVariables %in% names(results$popupData()), 
                "These variables are not included in the data"))
        
        for(iName in input$popupVariables){
          
          extraVariables <- paste0(extraVariables,
              "<br> <b>", iName, "</b>: ", 
              results$popupData()[, iName])
          
        }
        
      }
            
      
      if (input$calculate == "kriging") {
        
        validate(need(inherits(results$summaryData(), "kriging"),
                "Please push the button 'Calculate'"))
        
        regionNames <- results$descriptiveData()$spatialUnit
        
        if(!is.null(input$variableKriging) & !is.null(input$timeUnitKriging)){
          
          varNames <- c("Predicted response" = "var1.pred",
              "Variance" = "var1.var",
              "Standard deviation" = "var1.stdev")
          
          textKriging <- paste0("<strong>", 
              names(varNames)[varNames == input$variableKriging], "</strong>: ", 
              round(results$summaryData()[[input$timeUnitKriging]]$
                  krigingPredictions[[input$variableKriging]], 2))
          
        } else {
          
          textKriging <- paste0("<strong>", "Predicted response", "</strong>: ", 
              round(results$summaryData()[[1]]$krigingPredictions[["var1.pred"]], 2))
          
        }
        
        textPopup <- paste0("<h4>", regionNames, "</h4>",  
            
            "<strong>", input$variable1, "</strong>: ", 
            round(results$valuesVariable1(), 2), "<br>",
            
            textKriging,
            extraVariables              
        )
        
        
      } else {
        
        regionNames <- results$descriptiveData()[, input$descriptiveSpatial]
        
        validate(need(input$variable2, ""))
        
        if(input$variable2 == "none"){
          
          textVariable2 <- ""
          
        } else {
          
          textVariable2 <- paste("<br> <strong>", input$variable2, "</strong>: ", 
              round(results$descriptiveData()[, input$variable2], 2),
              "<br> <em> Defined percentage: </em>", 
              sprintf("%1.2f%%", 100*results$valuesVariable2()))
          
        }
        
        textPopup <- paste0("<h4>", regionNames, "</h4>",  
            
            "<strong>", input$variable1, "</strong>: ", 
            round(results$valuesVariable1(), 2),
            "<br> <em> Defined class: </em>", 
            results$summaryData()$value, "; ", 
            results$summaryData()$bound[results$summaryData()$value],
            
            textVariable2,                  
            extraVariables              
        )
        
      }
      
      return(textPopup)
      
    })


# Note: conflict between leaflet and nvd3, use Highchart instead, or
#http://stackoverflow.com/questions/23151171/rcharts-conflict-between-leaflet-and-nvd3-in-shiny
results$mapDescriptives <- eventReactive(input$showMap, {
      
      if(input$calculate == "kriging"){
        
        validate(need(inherits(results$summaryData(), "kriging"),
                "Please push the button 'Calculate'"))
        
        palette <- colorNumeric(palette = input$colorPaletteKriging,
            domain = results$summaryData()[[1]]$krigingPredictions[["var1.pred"]])
        
        ids <- as.character(seq_along(results$summaryData()[[1]]$
                krigingPredictions[["var1.pred"]]))
        
        leaflet(results$descriptiveShapeData()) %>%
            
            addPolygons(
                weight = 1,
                color = "white",
                fillColor = ~palette(results$summaryData()[[1]]$
                    krigingPredictions[["var1.pred"]]),
                fillOpacity = 0.8,
                layerId = ids,
                group = "region")
        
      } else {
        
        leaflet(results$descriptiveShapeData()) %>%
            
            addPolygons(
                weight = 1, 
                color = "white",
                fillColor = results$summaryData()$color[results$summaryData()$value],
                fillOpacity = results$valuesVariable2(),
                layerId = seq_len(nrow(results$descriptiveData())),
#                layerId = rownames(results$descriptiveShapeData()@data),
                group = "region"
            ) 
        
      }
      
      
    })

output$mapDescriptives <- renderLeaflet({
      
      input$showMap
      
      isolate({
            
            results$mapDescriptives()
            
          })
      
    })


# Add world map
addWorldMap <- function(map, providerTiles) {
  
  if(!is.null(providerTiles)){
    
    if(providerTiles %% 2 == 1){
      
      addProviderTiles(map, "Hydda.Full")
      
    } else {
      
      clearTiles(map)
      
    }
    
  }
  
}

observe({
      
      proxy <- leafletProxy("mapDescriptives")
      
      addWorldMap(map = proxy, providerTiles = input$providerTiles)
      
    })

#observe({
#      
#      proxy <- leafletProxy("mapDescriptives", data = results$descriptiveShapeData())
#      
#      if(!is.null(input$providerTiles)){
#          
#          if(input$providerTiles %% 2 == 1){
#            
#            proxy %>% addProviderTiles("Hydda.Full")
#            
#          } else {
#            
#            proxy %>% clearTiles()
#            
#          }
#          
#        }
#            
#    })



# Add legend
observe({
      
      if(!is.null(input$legendPlacement)){
        
        proxy <- leafletProxy("mapDescriptives", data = results$descriptiveShapeData())
        
        proxy %>% removeControl(layerId = "legend")
        
        if(input$legendPlacement != "none"){
          
          if(input$calculate == "kriging"){
            
            validate(need(input$timeUnitKriging, "") %then%
                    need(inherits(results$summaryData(), "kriging"),
                        "Please push the button 'Calculate'"))
            
            palette <- colorNumeric(palette = input$colorPaletteKriging,
                domain = results$summaryData()[[input$timeUnitKriging]]$
                krigingPredictions[[input$variableKriging]])
            
            proxy %>% addLegend(position = input$legendPlacement,
                pal = palette, 
                values = results$summaryData()[[input$timeUnitKriging]]$
                krigingPredictions[[input$variableKriging]],
                opacity = 0.8,
                title = "Legend",
                layerId = "legend"
            )
            
          } else {
            
            proxy %>% addLegend(position = input$legendPlacement,
                colors = results$summaryData()$color, 
                labels = levels(results$summaryData()$value),
                opacity = 0.8,
                layerId = "legend"
            )
            
          }
          
        } 
        
      }
      
    })

# Add popups
observe({
      
      currentMap <- leafletProxy("mapDescriptives", data = results$descriptiveShapeData()) 
      currentMap %>% clearPopups()
      
      event <- input$mapDescriptives_shape_click
      
      if(!is.null(event)){
        
        textSelected <- results$textPopup()[as.numeric(event$id)]
        
        isolate({
              
              currentMap %>% 
                  addPopups(event$lng, event$lat, popup = textSelected)
              
            }) 
        
      }
      
    })



# Add kriging results
observe({
      
      if(input$calculate == "kriging"){
        
        validate(need(input$variableKriging, "") %then%
                need(input$timeUnitKriging, "") %then%
                need(inherits(results$summaryData(), "kriging"), 
                    "Please push the button 'Calculate'"))
        
        palette <- colorNumeric(palette = input$colorPaletteKriging,
            domain = results$summaryData()[[input$timeUnitKriging]]$
            krigingPredictions[[input$variableKriging]])
        
        ids <- as.character(seq_along(results$summaryData()[[input$timeUnitKriging]]$
                krigingPredictions[[input$variableKriging]]))
        
        leafletProxy("mapDescriptives", data = results$descriptiveShapeData()) %>%
            
            clearShapes() %>%
            
            addPolygons(
                weight = 1,
                color = "white",
                fillColor = ~palette(results$summaryData()[[input$timeUnitKriging]]$
                    krigingPredictions[[input$variableKriging]]),
                fillOpacity = 0.8,
                layerId = ids,
                group = "region"
            )
      }
      
    })


output$titleMapDescriptives <- renderUI({
      
      if (is.null(input$showMap))
        return(NULL)
      
      if(input$showMap == 0)
        return(NULL)
     
      
      startLegend <- switch(input$calculate,
          "none" = "Descriptive classes",
          "moran" = "Local Moran's I", 
          "geary" = "Local Geary's C",
          "kriging" = "Ordinary kriging")
      
      if(input$calculate == "kriging") {
        
        h4(startLegend, "for", input$variable1, "in", input$timeUnitKriging)
  
      } else {
        
        h4(startLegend, "for", input$variable1)
        
      }
      
    })


# TODO save leaflet map with leafletProxy adaptations
finalMap <- reactive({
      
      addWorldMap(map = results$mapDescriptives(), input$providerTiles)
      
#      leaflet(results$descriptiveShapeData()) %>% addProviderTiles("Hydda.Full")
      
    })


output$downloadMapDescriptives <- downloadHandler("mapDescriptives.png",
    content = function(file) {
            
      saveWidget(widget = finalMap(), 
          file = file.path(tempdir(), "mapDescriptives.html"), selfcontained = FALSE)
      webshot(file.path(tempdir(), "mapDescriptives.html"), file = file, 
          cliprect = "viewport")
      
    }
)