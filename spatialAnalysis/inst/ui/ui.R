
library(shinysky) # for hotable and busy indicator
library(leaflet) # for maps
library(shinyjs) # for colourInput()
library(rCharts) #for interactive plots


spatialLevels <- c("NUTS0" = 1, "NUTS1" = 2, "NUTS2" = 3, "NUTS3" = 4, 
    "LAU1" = 5, "LAU2" = 6)
timeLevels <- c("Year", "Month", "Week", "Day")


shinyUI(
    
    
    fluidPage(
        
        #for debugging
        verbatimTextOutput("print"),
        actionButton("example", "Example", styleclass = "success"),
                
        
        helpText(
            h5(a(href="https://efsa-models.openanalytics.eu/projects/spatial/wiki", 
                    target="_blank", "About"), align = "right"),
            h5(a(href="https://efsa-models.openanalytics.eu/projects/spatial/issues/new", 
                    target="_blank", "Report new issue"), align = "right")
        ),
        
       
        navbarPage(id = "navbar", title = div(img(src = "EFSA_logo.JPG",
                    float = "top", height = "50px", hspace = "60px",
                    style = 'margin-top:-15px'),
                "Spatial Analysis"),
            windowTitle = "Spatial Analysis",
            
            
            tabPanel("Load Data",
                
                fluidRow(
                    
                    column(6, 
                        p("The following variables at lowest time and space level will be created:",
                            tags$ul(
                                tags$li(tags$b("numberPositive: "), "the number of positive responses"),
                                tags$li(tags$b("numberObservations: "), "the number of observations"), 
                                tags$li(tags$b("isPositive: "), "whether any of the responses is positive"),
                                tags$li(tags$b("percentagePositive: "), "the percentage of positive responses")
                            )
                        )
                    ),
                    column(4,
                        selectInput("timeResolution", "Temporal resolution (lowest level)",
                            choices = c("Year" = 1, "Month" = 2, "Day" = 3, "Date" = 4)),
                        selectInput("spaceResolution", "Spatial resolution (lowest level)",
                            choices = c("NUTS0" = 1, "NUTS1" = 2, "NUTS2" = 3, "NUTS3" = 4, 
                                "LAU1" = 5, "LAU2" = 6))
                    ),
                    column(2, conditionalPanel("input.timeResolution == '4'",
                            selectInput("dateFormat", "with date format",
                                choices = c("dd/mm/yyyy" = "%d/%m/%Y",
                                    "ddmmyyyy" = "%d%m%Y",
                                    "mm/dd/yyyy" = "%m/%d/%Y",
                                    "mmddyyyy" = "%m%d%Y",
                                    "yyyy/mm/dd" = "%Y/%m/%d", 
                                    "yyyymmdd" = "%Y%m%d"))
                        )
                    )
                
                ),
                
                
                tags$br(),
                tags$br(),
                
                
                tabsetPanel(id = "dataTabs",
                    
                    tabPanel("Temporal Data",
                        
                        tags$br(),
                        
                        fluidRow(
                            
                            column(5, wellPanel(
                                    
                                    fileInput('infoData', 'Load data',
                                        accept = c('text/csv', 
                                            'text/comma-separated-values, text/plain', 
                                            '.csv')),
                                    
                                    radioButtons('sep', 'Separator for data',
                                        c("Comma (.csv file)" = ',',
                                            "Semicolon (.csv file)" = ';',
                                            "Tab (.txt file)" = '\t')),
                                    
                                    uiOutput("infoData")
                                
                                )),
                            
                            column(7, 
                                
                                tags$br(),
                                DT::dataTableOutput('showInfoData')
                            
                            )
                        )
                    ),
                    
                    
                    tabPanel("Spatial Data",
                        
                        tags$br(),
                        
                        fluidRow(
                            
                            column(5, wellPanel(
                                    
                                    radioButtons("shapeDataPolygons", "Load data with polygons for",
                                        choices = c("all spatial levels" = "all",
                                            "lowest spatial level only" = "lowest")),
                                    
                                    fileInput('shapeData', 
                                        'Load shape data (zipped)',
                                        accept = '.zip'),
                                    
                                    uiOutput("shapeData"),
                                    uiOutput("subsetShapeData")                                    
                                
                                )),
                            
                            column(7, 
                                
                                tags$br(),
                                DT::dataTableOutput('showShapeData'),
                                plotOutput("selectedPolygons")
                            
                            )
                        )
                    ),
                    
                    tabPanel("Covariates Data",
                        
                        tags$br(),
                        
                        fluidRow(
                            
                            column(5, wellPanel(
                                    
                                    fileInput('covariatesData', 'Load data',
                                        accept = c('text/csv', 
                                            'text/comma-separated-values, text/plain', 
                                            '.csv')),
                                    
                                    radioButtons('sepCovariates', 'Separator for data',
                                        c("Comma (.csv file)" = ',',
                                            "Semicolon (.csv file)" = ';',
                                            "Tab (.txt file)" = '\t')),
                                    
                                    uiOutput("covariatesData")
                                
                                )
                            
                            ),
                            
                            column(7, 
                                
                                tags$br(),
                                DT::dataTableOutput('showCovariatesData')
                            
                            ))
                    ),
                    
                    tabPanel("Define Neighbours",
                        
                        tags$br(),
                        
                        fluidRow(
                            
                            column(5, wellPanel(
                                    
                                    radioButtons("copyShape", "",
                                        choices = c("Use spatial data" = 'yes',
                                            "Load neighbours data" = 'no')),
                                    
                                    conditionalPanel("input.dataTabs == 'Define Neighbours' & 
                                            input.copyShape == 'no'",
                                        
                                        fileInput('neighboursData', 'Load all neighbours data (.rds)',
                                            accept = '.rds')
                                    
                                    ),
                                    
                                    conditionalPanel("input.dataTabs == 'Define Neighbours' & 
                                            input.copyShape == 'yes'",
                                        actionButton("calculateNeighbours", "Calculate all neighbours", 
                                            styleclass = "primary", size = "mini"),
                                        tags$br()
                                    
                                    ),
                                    
                                    tags$hr(),
                                    
                                    uiOutput("neighboursSpatialLevel"),
                                    numericInput("maxDistance", "Maximum distance between neighbours (km)",
                                        value = 100),
                                    fluidRow(
                                        column(6, uiOutput("defineRegion1")),
                                        column(6, uiOutput("defineRegion2"))
                                    ),
                                    actionButton("makeNeighbours", "Define as neigbours",
                                        styleclass = "primary", size = "mini"),
                                    downloadButton("downloadNeighbours", "Download")
                                
                                )
                            ), 
                            column(7, 
                                
                                plotOutput("plotShapeData")
                            
                            ),
                            
                            busyIndicator("In progress", wait = 0)
                        
                        )
                    )
                ),
                
                
                tags$hr(),
                
                busyIndicator("In progress", wait = 0),
                actionButton("mergeData", "Show Data for Analysis", 
                    styleclass = "primary", size = "mini"),
                
                tags$br(),
                tags$br(),
                
                textOutput("totalNumberObservations"),
                tags$br(),
                DT::dataTableOutput('showMergedData')
            
            ),
            
            tabPanel("Descriptives",
                
                fluidRow(
                    
                    column(4, wellPanel(
                            
                            selectInput("calculate", "Calculate",
                                choices = c("Descriptive classes" = "none", 
                                    "Local Moran's I" = "moran", 
                                    "Local Geary's C" = "geary",
                                    "Ordinary kriging" = "kriging",
                                    "Linear/Logistic regression" = "regression")),
                            
                            conditionalPanel("input.calculate == 'kriging'",
                                
                                list(
                                    
                                    selectInput("variogramModel", "Variogram model",
                                        choices = c("Automatic selection" = "auto",
                                            "Spherical" = "Sph", 
                                            "Exponential" = "Exp", 
                                            "Gaussian" = "Gau")),
                                    
                                    conditionalPanel("input.calculate == 'kriging' &
                                            input.variogramModel != 'auto'",
                                        hotable("variogramParameters")
                                    ),
                                    
                                    tags$br()
                                )
                            ),
                            
                            busyIndicator("In progress", wait = 0),
                            actionButton(inputId = "doCalculate", label = "Calculate", 
                                styleclass = "primary", size = "mini"),
                            
                            
                            uiOutput("descriptiveLevels"),
                            
                            fluidRow(
                                
                                column(6, uiOutput("selectVariable1")),
                                column(6, 
                                    conditionalPanel("input.calculate != 'kriging'",
                                        uiOutput("selectVariable2")
                                    )
                                )
                            ),
                            
                            actionLink(inputId = "helpVariables", label = "Variables",
                                icon = icon("info-circle")),
                            
                            conditionalPanel("input.helpVariables % 2 == 1",
                                
                                list(
                                    
                                    conditionalPanel("input.calculate != 'kriging' & input.calculate != 'regression'",
                                        
                                        fluidRow(
                                            column(6, p(strong("Variable 1:"), "region color")),
                                            column(6, p(strong("Variable 2:"), "color opacity"))
                                        )
                                    
                                    ),
                                    
                                    conditionalPanel("input.calculate == 'kriging'",
                                        
                                        p(strong("Variable 1:"), "response for prediction")
                                    
                                    ),
                                    
                                    conditionalPanel("input.calculate == 'regression'",
                                        
                                        fluidRow(
                                            column(6, p(strong("Variable 1:"), "response for prediction")),
                                            column(6, p(strong("Variable 2:"), "covariate for prediction"))
                                        )
                                    
                                    )
                                )
                            ),
                            
                            conditionalPanel("input.calculate != 'kriging' & 
                                    input.calculate != 'regression'",
                                
                                list(
                                    
                                    tags$hr(),  
                                    
                                    h4("Define classes for Variable 1"),
                                    
                                    conditionalPanel("input.optionBreaks != 'quadrant'",
                                        
                                        sliderInput("nClasses", label = "Number of classes",
                                            value = 5, min = 3, max = 10, step = 1)
                                    
                                    ),
                                    
                                    fluidRow(
                                        column(9,
                                            selectInput("optionBreaks", "Class type",
                                                choices = c("Quantiles" = "quantile",
                                                    "Natural (Jenks)" = "jenks",
                                                    "Manual input - lower bounds" = "fixedLower",
                                                    "Manual input - upper bounds" = "fixedUpper")),
                                            uiOutput("manualBreaks")
                                        ),
                                        column(3, 
                                            selectInput("colorPalette", "Colours",
                                                choices = c("Accent", "Reds", "Spectral", 
                                                    "Diverging" = "RdBu",
                                                    "Manual input" = "fixed")),
                                            uiOutput("manualColors")
                                        
                                        )
                                    ),
                                    
                                    conditionalPanel("input.optionBreaks == 'quadrant'",
                                        
                                        list(
                                            
                                            actionLink(inputId = "helpQuadrants", label = "Quadrants",
                                                icon = icon("info-circle")),
                                            
                                            conditionalPanel("input.helpQuadrants % 2 == 1",
                                                p(em("Observed response value vs Estimated local statistic"), 
                                                    br(), strong("Missing:"), "Missing local statistic value",
                                                    br(), strong("High-High:"), "potential cluster",
                                                    br(), strong("High-Low:"), "potential hotspot",
                                                    br(), strong("Low-High:"), "potential hotspot",
                                                    br(), strong("Low-Low:"), "potential cluster"
#                                            br(), strong("Non-significant:"), "p-value of local statistic larger than significance level",
                                                )
                                            )
                                        )
                                    
                                    ),
                                    
                                    conditionalPanel("input.optionBreaks != 'quadrant'",
                                        
                                        radioButtons("intervalClosure", "",
                                            choices = c("Left-closed intervals" = "left",
                                                "Right-closed intervals" = "right"))
                                    
                                    ),
                                    
                                    uiOutput("warningBreaks"),
                                    
                                    checkboxInput("classMissing", "Add class for missing values",
                                        value = TRUE),
                                    
                                    uiOutput("classMissing")
                                
                                )
                            
                            ),
                            
                            conditionalPanel("input.calculate == 'kriging'",
                                
                                tags$br(),
                                tags$hr(),  
                                
                                selectInput("colorPaletteKriging", "Color palette",
                                    choices = c("Reds", "Blues", "Greens", 
                                        "Diverging" = "RdBu"))                                
                            
                            )
                        
#                             textInput("missingValue", label = "Value/Text for missing observation"),
#                            column(11, radioButtons("optionMissings", label = "",
#                                    choices = c("Ignore missing values" = "ignore",
#                                        "Separate category for missing values" = "separate")),
#                                offset = 1),
                        
                        
                        )
                    ),
                    
                    column(8, 
                        
                        conditionalPanel("input.calculate == 'regression' |
                                input.calculate == 'kriging'", 
                            uiOutput("summaryAnalysis")
                        ),
                        
                        conditionalPanel("input.calculate == 'kriging'",
                            uiOutput("showKriging")
                        ),
                        
                        conditionalPanel("input.calculate != 'kriging'",
                            plotOutput("showVariable1")
                        ),
                        
                        conditionalPanel("input.calculate != 'kriging' & 
                                input.calculate != 'regression'",
                            plotOutput("showVariable2")
                        )
                    )
                ),
                
                tags$hr(),
                
                uiOutput("warnings"),
                
                tags$br(),
                
                fluidRow(
                    
                    column(4, uiOutput("controlMap")),
                    column(8, 
                        uiOutput("titleMapDescriptives"),
                        leafletOutput("mapDescriptives", height = "800px"),
                        downloadButton("downloadMapDescriptives", "Download")
                    
                    )
                
                )
            
            ),
            
            tabPanel("Analysis",
                
                fluidRow(
                    
                    column(4, wellPanel(
                            
                            h4("Define input parameters"),
                            
                            uiOutput("parametersInla"),
                            selectInput("timeEffect", "Time effect",
                                choices = c("<none>" = "none", "saturated", "linear", 
                                    "first-order random walk (RW1)" = "rw1", 
                                    "first-order autoregressive (AR1)" = "ar1", 
                                    "second-order random walk (RW2)" = "rw2", 
                                    "all")),
                            
                            selectInput("interactionEffect", "Interaction effect",
                                choices = c("<none>" = "none", 
                                    "Type I (unstructured space x unstructured time)" = "type1", 
                                    "Type II (unstructured space x structured time)" = "type2", 
                                    "Type III (structured space x unstructured time)" = "type3", 
                                    "Type IV (structured space x structured time)" = "type4")),
                            
                            selectInput("colorPaletteInla", "Color palette",
                                choices = c("Reds", "Blues", "Greens", 
                                    "Diverging" = "RdBu")),    
                            
                            busyIndicator("In progress", wait = 0),
                            actionButton("submitInla", "Perform INLA",
                                styleclass = "primary", size = "mini")
                        
                        )
                    ),
                    
                    column(8, 
                        
                        uiOutput("summaryInla")
                    
                    )
                ),
                
                tags$hr(),
                
                uiOutput("warningsInla"),
                
                tags$br(),
                tags$br(),
                
                uiOutput("controlMapInla"),
                
                fluidRow(
                    column(6,
                        renderText("titleInteractivePlot"),
                        leafletOutput("mapInla", height = "500px")
                    ),
                    column(6, 
                        showOutput("interactiveTime", "highcharts")
                        
                    )
                ),
                
                tags$br(),
                uiOutput("downloadInla"),
                tags$br()
            
            )
        
        )      
    
    )

)