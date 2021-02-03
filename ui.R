shinyUI(fluidPage(theme= "yeti.css",
                  navbarPage("CEDS Benthic Data Query Tool",
                             
                             tabPanel('How To',
                                      htmlOutput("BenthicQueryToolHowTo") ),
                             
                             tabPanel("Single Station Query (Live CEDS Connection)",
                                      tabsetPanel(
                                        tabPanel("Station Data",
                                                 sidebarPanel(
                                                   helpText("Query will pull directly from CEDS. Data is refreshed nightly."),
                                                   textInput('station', 'DEQ Station ID', placeholder = "DEQ Station ID"),
                                                   br(),
                                                   actionButton('begin', 'Pull Station',class='btn-block')),
                                                 mainPanel(
                                                   #verbatimTextOutput('test'),
                                                   leafletOutput('stationMap'),
                                                   br(),
                                                   h4('Station Information'),
                                                   DT::dataTableOutput('stationInfoTable'),
                                                   br(),
                                                   h4('Sampling Summary'),
                                                   helpText('The records listed below are limited to records with associated sample codes. Additional 
                                                            (older) samples could be lacking the sample code but have benthic data for exploration 
                                                            in subsequent sections of the application.'),
                                                   DT::dataTableOutput('stationInfoSampleCodeMetrics'),
                                                   br(),
                                                   #helpText('Eventually maybe some stats on n sampling events or raw field data?'),
                                                   br(), br(), br() # a little breathing room
                                                 )),
                                        tabPanel("Benthic Data",
                                                 sidebarPanel(
                                                   uiOutput('dateRange_'),
                                                   radioButtons('SCIchoice', "SCI Choice", choices = c('VSCI', 'VCPMI + 63', 'VCPMI - 65')),
                                                   #helpText("Or we could run SCI by ecoregion location???"),
                                                   checkboxInput('rarifiedFilter', "Only Include Target Count = 110", value=TRUE),
                                                   checkboxInput('boatableFilter', "Only Include Wadeable Methods", value=TRUE),
                                                   checkboxGroupInput('repFilter', "Filter Reps (if none are selected then all are included)", 
                                                                      choices = c('1','2'), selected = NULL)   ),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Sampling Metrics', 
                                                              h4('SCI Averages'),
                                                              DT::dataTableOutput('averageSamplingMetrics'),
                                                              br(),
                                                              fluidRow(column(6,h4('Collector Information'),
                                                                              DT::dataTableOutput('collectorMetrics')),
                                                                       column(6,h4("Taxonomist Information"),
                                                                              DT::dataTableOutput('taxonomistMetrics'))),
                                                              hr(),
                                                              h4('Benthic Sample Information'),
                                                              DT::dataTableOutput('samplingInformation') ,
                                                              br(), br(), br()), # a little breathing room),
                                                     tabPanel('Detailed SCI Results',
                                                              h4("SCI Interactive Plot"),
                                                              helpText("Note: Only rarified samples are plotted."),
                                                              plotlyOutput('SCIplot'),
                                                              h4("SCI Metrics"),
                                                              DT::dataTableOutput('SCIresultsTable'),
                                                              br(), br(), br()), # a little breathing room
                                                     tabPanel('Raw Benthic Data',
                                                              tabsetPanel(
                                                                tabPanel('Crosstab View',
                                                                         DT::dataTableOutput('rawBenthicCrosstab'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Long View',
                                                                         DT::dataTableOutput('rawBenthic'),
                                                                         br(), br(), br())) # a little breathing room
                                                     ),
                                                     tabPanel('Benthic Visualization Tools',
                                                              helpText('This tab combines various analysis tools from regional biologists to share data visualization
                                                                       techniques. Tools can be added to this section by contacting Emma Jones (emma.jones@deq.virginia.gov).'),
                                                              br(),
                                                              #h4('SCI Seasonal Crosstab'),
                                                              #uiOutput('metric_UI'),
                                                              #DT::dataTableOutput('SCIseasonalCrosstab'),
                                                              h4('Benthic Individuals BenSamp Crosstab'),
                                                              radioButtons('genusOrFamily', "Analyze by Genus or Family", choices = c('Genus', 'Family')),
                                                              DT::dataTableOutput('benthicIndividualsBensampCrosstab'),
                                                              br(), br(), br())) # a little breathing room
                                                     )),
                                        tabPanel("Habitat Data",
                                                 sidebarPanel(
                                                   radioButtons('useBenthicDateRange', "Filter Habitat Data by Date", 
                                                                choices = c("Use Benthic Data Date Filter", "Filter Custom Date Range")),
                                                   #checkboxInput('useBenthicDateRange', "Use Benthic Data Date Filter", value=TRUE),
                                                   conditionalPanel(condition = "input.useBenthicDateRange == 'Filter Custom Date Range'",
                                                                    #conditionalPanel(condition = "input.useBenthicDateRange == FALSE",
                                                                    uiOutput('habitatDateRange_')),
                                                   checkboxGroupInput('gradientFilter', "Filter Gradient (if none are selected then all are included)", 
                                                                      choices = c('High', 'Low'))),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Sampling Metrics',
                                                              h4('Total Habitat Averages'),
                                                              DT::dataTableOutput('averageTotalHabitatMetrics'),
                                                              br(),
                                                              fluidRow(column(6,h4('Field Team Information'),
                                                                              DT::dataTableOutput('fieldTeamMetrics')),
                                                                       column(6,h4("Habitat Observation Metrics"),
                                                                              DT::dataTableOutput('habObsMetrics'))),
                                                              hr(),
                                                              h4('Habitat Sample Information'),
                                                              DT::dataTableOutput('habitatSamplingInformation'),
                                                              br(), br(), br() # a little breathing room
                                                     ),
                                                     tabPanel('Detailed Habitat Results',
                                                              plotlyOutput('totalHabitatPlot'),
                                                              h4("Habitat Metrics"),
                                                              DT::dataTableOutput('habitatResultsTable'),
                                                              br(), br(), br()), # a little breathing room),
                                                     tabPanel('Raw Habitat Data',
                                                              tabsetPanel(
                                                                tabPanel('Crosstab View',
                                                                         h4('Habitat Values'),
                                                                         DT::dataTableOutput('habitatValuesCrosstab'),
                                                                         h4('Habitat Observation'),
                                                                         DT::dataTableOutput('habitatObservationsCrossTab'),
                                                                         br(), br(), br() ),# a little breathing room )
                                                                tabPanel('Long View',
                                                                         h4('Habitat Values'),
                                                                         DT::dataTableOutput('habitatValues'),
                                                                         h4('Habitat Observation'),
                                                                         DT::dataTableOutput('habitatObservations'),
                                                                         br(), br(), br() )# a little breathing room )
                                                              ) )))))),
                             
                             tabPanel('Multiple Station Query (Archived Data Refreshed Weekly)',
                                      tabsetPanel(
                                        tabPanel("Station Query",
                                                 sidebarPanel(
                                                   helpText("Query pulls data stored on R server. Data is refreshed weekly."),
                                                   radioButtons('queryType', "How would you like to query stations?", 
                                                                choices = c('Spatial Filters', 'Wildcard Selection', 'Manually Specify Stations')),
                                                   
                                                   # Spatial filters
                                                   conditionalPanel(
                                                     condition = "input.queryType == 'Spatial Filters'",
                                                     uiOutput('spatialFilters_assessmentRegion'),
                                                     uiOutput('spatialFilters_subbasin'),
                                                     uiOutput('spatialFilters_VAHU6'),
                                                     hr(),
                                                     uiOutput('spatialFilters_Ecoregion'),
                                                     uiOutput('dateRange_multistationUI'),
                                                     br(),
                                                     actionButton('begin_multistation_spatial', 'Pull Stations',class='btn-block')),
                                                   
                                                   
                                                   # Wildcard Selection
                                                   conditionalPanel(
                                                     condition = "input.queryType == 'Wildcard Selection'",
                                                     uiOutput('wildcardSelection'),
                                                     #helpText('Please reload the application if you want to return to the spatial filtering method.'),
                                                     br(),
                                                     actionButton('begin_multistation_wildcard', 'Pull Stations',class='btn-block')),
                                                   
                                                   # Manually Specify Stations Selection
                                                   conditionalPanel(
                                                     condition = "input.queryType == 'Manually Specify Stations'",
                                                     uiOutput('manualSelection'),
                                                     br(),
                                                     actionButton('begin_multistation_manual', 'Pull Stations',class='btn-block')) ),
                                                 
                                                 mainPanel(
                                                  # verbatimTextOutput('test'),
                                                   
                                                   leafletOutput('multistationMap'),
                                                   helpText('Stations identified in the spatial filter are displayed below unless user further refines 
                                                          selected stations with polygon and rectangle drawing tools in map.'),
                                                   br(),
                                                   h4('Station Information'),
                                                   DT::dataTableOutput('multistationInfoTable'),
                                                   br(),
                                                   h4('Sampling Summary'),
                                                   helpText('The records listed below are limited to records with associated sample codes. Additional 
                                                            (older) samples could be lacking the sample code but have benthic data for exploration 
                                                            in subsequent sections of the application.'),
                                                   DT::dataTableOutput('multistationInfoSampleMetrics'),
                                                   br(), br(), br() # a little breathing room
                                                 )),
                                        tabPanel("Benthic Data",
                                                 sidebarPanel(
                                                   helpText('The default date range is set to the first and last benthic sample in the dataset. Further
                                                            date filtering may be performed in the date range filter below.'),
                                                   uiOutput('dateRange_benSamps_multistationUI'),
                                                   checkboxInput('multistationRarifiedFilter', "Only Include Target Count = 110", value=TRUE),
                                                   checkboxInput('multistationBoatableFilter', "Only Include Wadeable Methods", value=TRUE),
                                                   checkboxGroupInput('multistationRepFilter', "Filter Reps (if none are selected then all are included)", 
                                                                      choices = c('1','2'), selected = NULL)   ),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Sampling Metrics', 
                                                              h4('SCI Averages by Selection'),
                                                              DT::dataTableOutput('averageSamplingMetrics_bySelection'),
                                                              br(),
                                                              h4('SCI Averages by Station'),
                                                              DT::dataTableOutput('averageSamplingMetrics_byStation'),
                                                              br(),
                                                              fluidRow(column(6,h4('Collector Information'),
                                                                              DT::dataTableOutput('collectorMetrics_multistation')),
                                                                       column(6,h4("Taxonomist Information"),
                                                                              DT::dataTableOutput('taxonomistMetrics_multistation'))),
                                                              br(),
                                                              h4('Benthic Sample Information'),
                                                              DT::dataTableOutput('multistationBenthicSampleData'),
                                                              br()),
                                                     tabPanel('SCI Results',
                                                              h4('SCI Results'),
                                                              DT::dataTableOutput('multistationSCIresult'),
                                                              br(), hr(), br(),
                                                              helpText('The above SCI results are recommended based on station location (Level III Ecoregion) information.
                                                                       If users need to adjust the SCI used based on best professional judgement, the table below allows
                                                                       users to see additional SCI scores.'),
                                                              selectInput('sciChanger', 'SCI Choice', choices = c('VSCI', 'VCPMI + 63', 'VCPMI - 65')),
                                                              DT::dataTableOutput('SCIresultsAdjusted')),
                                                     tabPanel('Raw Benthic Data',
                                                              tabsetPanel(
                                                                tabPanel('Crosstab View',
                                                                         DT::dataTableOutput('rawMultistationBenthicCrosstab'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Long View',
                                                                         DT::dataTableOutput('rawMultistationBenthic'),
                                                                         br(), br(), br())) # a little breathing room
                                                     ),
                                                     tabPanel('Benthic Visualization Tools',
                                                              helpText('This tab combines various analysis tools from regional biologists to share data visualization
                                                                       techniques. Tools can be added to this section by contacting Emma Jones (emma.jones@deq.virginia.gov).'),
                                                              br(),
                                                              h4('SCI Seasonal Crosstab'),
                                                              uiOutput('metric_UI_multistation'),
                                                              DT::dataTableOutput('SCIseasonalCrosstab_multistation'),
                                                              br(), 
                                                              h4('Benthic Individuals BenSamp Crosstab'),
                                                              radioButtons('genusOrFamily_multistation', "Analyze by Genus or Family", choices = c('Genus', 'Family')),
                                                              DT::dataTableOutput('benthicIndividualsBensampCrosstab_multistation'),
                                                              br(), br(), br() # a little breathing room
                                                              )))),
                                        tabPanel("Habitat Data",
                                                 sidebarPanel(
                                                   helpText("Habitat Data Filtered to Benthic Data Date Range."), 
                                                   checkboxGroupInput('multistationGradientFilter', "Filter Gradient (if none are selected then all are included)", 
                                                                      choices = c('High', 'Low'))),
                                                 mainPanel(
                                                   tabsetPanel(
                                                     tabPanel('Sampling Metrics',
                                                              h4('Total Habitat Averages'),
                                                              DT::dataTableOutput('averageTotalHabitatMetrics_multistation'),
                                                              br(),
                                                              fluidRow(column(6,h4('Field Team Information'),
                                                                              DT::dataTableOutput('fieldTeamMetrics_multistation')),
                                                                       column(6,h4("Habitat Observation Metrics"),
                                                                              DT::dataTableOutput('habObsMetrics_multistation'))),
                                                              hr(),
                                                              h4('Habitat Sample Information'),
                                                              DT::dataTableOutput('habitatSamplingInformation_multistation'),
                                                              br(), br(), br() # a little breathing room
                                                     ),
                                                     tabPanel('Detailed Habitat Results',
                                                              # plotlyOutput('totalHabitatPlot'),
                                                              h4("Habitat Metrics"),
                                                              DT::dataTableOutput('habitatResultsTable_multistation'),
                                                              br(), br(), br()), # a little breathing room),
                                                     tabPanel('Raw Habitat Data',
                                                              tabsetPanel(
                                                                tabPanel('Crosstab View',
                                                                         h4('Habitat Values'),
                                                                         DT::dataTableOutput('habitatValuesCrosstab_multistation'),
                                                                         h4('Habitat Observation'),
                                                                         DT::dataTableOutput('habitatObservationsCrossTab_multistation'),
                                                                         br(), br(), br() ),# a little breathing room )
                                                                tabPanel('Long View',
                                                                         h4('Habitat Values'),
                                                                         DT::dataTableOutput('habitatValues_multistation'),
                                                                         h4('Habitat Observation'),
                                                                         DT::dataTableOutput('habitatObservations_multistation'),
                                                                         br(), br(), br() )# a little breathing room )
                                                              ))))))) 
                             
                            
                               
                               
                            
                                      
                            )
                  
))
                                                 