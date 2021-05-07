shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
                  navbarPage("CEDS Benthic Data Query Tool", id = 'someID',  # key for passing URL to specific Tab
                             
                             tabPanel('How To',
                                      includeMarkdown("BenthicQueryToolHowToNew.md")),
                                      #includeHTML("BenthicQueryToolHowTo.html")),#htmlOutput("BenthicQueryToolHowTo") ), # this was a hot mess. when trying to update the BenthicQueryHowTo rmd and rendering to html, the app would not take any user inputs. so weird and wasted hours on this problem. ultimately had to go with rendering the .md in the app to have any semblance of a solution
                             
                             tabPanel("Single Station Query (Live CEDS Connection)", value = 'SingleStation', # key for passing URL to specific Tab
                                      tabsetPanel(
                                        tabPanel("Station Data",
                                                 sidebarPanel(
                                                   helpText("Query will pull directly from CEDS. Data is refreshed nightly."),
                                                   helpText(strong('Note: Wildcard searches are only available in the Multiple 
                                                                   Station Query section of this application')),
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
                                                     tabPanel('Benthic Visualization Tools',
                                                              helpText('This tab combines various analysis tools from DEQ staff to share data visualization
                                                                       techniques. Tools can be added to this section by contacting Emma Jones (emma.jones@deq.virginia.gov).'),
                                                              br(),
                                                              tabsetPanel(
                                                                tabPanel('Benthic Individuals BenSamp Crosstab',
                                                                         radioButtons('genusOrFamily', "Analyze by Genus or Family", choices = c('Genus', 'Family')),
                                                                         DT::dataTableOutput('benthicIndividualsBensampCrosstab'), br(), br(), br()),# a little breathing room
                                                                tabPanel('BCG Attribute Information',
                                                                         helpText('The table below joins the taxa collected at the selected station and collection window
                                                                       with various BCG attribute levels from different regional BCG projects. For more information 
                                                                       on the BCG process', 
                                                                                  span(strong(a('click here.', href='https://www.deq.virginia.gov/home/showpublisheddocument?id=4303',
                                                                                                target='_blank')))), 
                                                                         DT::dataTableOutput('BCGattributes'),br(), br(), br()),# a little breathing room
                                                                tabPanel('FFG Stacked Bar Chart',
                                                                         helpText('Radiobuttons control what taxonomic level the Functional Feeding Group (FFG) stacked bar chart
                                                                                  plot describes. Note missing FFG designations in the master taxa list can result in missing
                                                                                  data from plot. The table below reflects data in the plot and can be downloaded for further
                                                                                  analysis.'),
                                                                         fluidRow(column(6, radioButtons('FFGgenusOrFamily', strong("Analyze by Genus or Family"), choices = c('Genus', 'Family'))),
                                                                                  column(6, radioButtons('FFGxAxis', strong("Plot x axis as:"), choices = c('BenSampID', 'Date (removes Rep 2 samples by default)')))),
                                                                         plotOutput('FFGplot'), br(), 
                                                                         DT::dataTableOutput('FFGdataTable'),br(), br(), br()) )# a little breathing room
                                                              ),
                                                              # 
                                                              # h4('Benthic Individuals BenSamp Crosstab'),
                                                              # radioButtons('genusOrFamily', "Analyze by Genus or Family", choices = c('Genus', 'Family')),
                                                              # DT::dataTableOutput('benthicIndividualsBensampCrosstab'), br(),
                                                              # h4('BCG Attribute Information'),
                                                              # helpText('The table below joins the taxa collected at the selected station and collection window
                                                              #          with various BCG attribute levels from different regional BCG projects. For more information 
                                                              #          on the BCG process', 
                                                              #          span(strong(a('click here.', href='https://www.deq.virginia.gov/home/showpublisheddocument?id=4303',
                                                              #                        target='_blank')))), 
                                                              # DT::dataTableOutput('BCGattributes'),
                                                              
                                                     tabPanel('Raw Data Download Formats',
                                                              tabsetPanel(
                                                                tabPanel('Raw Genus Level Data Crosstab View',
                                                                         DT::dataTableOutput('rawBenthicCrosstabGenus'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Raw Genus Level Data Long View',
                                                                         DT::dataTableOutput('rawBenthicGenus'),
                                                                         br(), br(), br()),
                                                                tabPanel('Raw Family Level Data Crosstab View',
                                                                         DT::dataTableOutput('rawBenthicCrosstabFamily'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Raw Family Level Data Long View',
                                                                         DT::dataTableOutput('rawBenthicFamily'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Benthic Stressor Analysis Data Format',
                                                                         helpText('This table displays benthic data for the selected station and filters for upload into
                                                                                  the Benthic Stressor Analysis Tool.'),
                                                                         h5(strong('The BSA tool only accepts .xlsx data format for this dataset.')),
                                                                         DT::dataTableOutput('BSAbenthicData')))) # a little breathing room
                                                     ))),
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
                                                     tabPanel('Raw Data Download Formats',
                                                              tabsetPanel(
                                                                tabPanel('Raw Habitat Data Crosstab View',
                                                                         h4('Habitat Values'),
                                                                         DT::dataTableOutput('habitatValuesCrosstab'),
                                                                         h4('Habitat Observation'),
                                                                         DT::dataTableOutput('habitatObservationsCrossTab'),
                                                                         br(), br(), br() ),# a little breathing room )
                                                                tabPanel('Raw Habitat Data Long View',
                                                                         h4('Habitat Values'),
                                                                         DT::dataTableOutput('habitatValues'),
                                                                         h4('Habitat Observation'),
                                                                         DT::dataTableOutput('habitatObservations'),
                                                                         br(), br(), br() ),
                                                                tabPanel("Benthic Stressor Analysis Data Format",
                                                                         helpText('This table displays habitat data for the selected station and filters for upload into
                                                                                  the Benthic Stressor Analysis Tool.'),
                                                                         h5(strong('The BSA tool only accepts .xlsx data format for this dataset.')),
                                                                         DT::dataTableOutput('BSAhabitatData'),
                                                                         br(), br(), br())# a little breathing room )
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
                                                     uiOutput('spatialFilters_County'),
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
                                                                tabPanel('Genus Level Crosstab View',
                                                                         DT::dataTableOutput('rawMultistationBenthicCrosstabGenus'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Genus Level Long View',
                                                                         DT::dataTableOutput('rawMultistationBenthicGenus'),
                                                                         br(), br(), br()),
                                                                tabPanel('Family Level Crosstab View',
                                                                         DT::dataTableOutput('rawMultistationBenthicCrosstabFamily'),
                                                                         br(), br(), br()), # a little breathing room
                                                                tabPanel('Family Level Long View',
                                                                         DT::dataTableOutput('rawMultistationBenthicFamily'),
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
                                                              DT::dataTableOutput('benthicIndividualsBensampCrosstab_multistation'), br(),
                                                              h4('BCG Attribute Information'),
                                                              helpText('The table below joins the taxa collected at the selected station and collection window
                                                                       with various BCG attribute levels from different regional BCG projects. For more information 
                                                                       on the BCG process', 
                                                                       span(strong(a('click here.', href='https://www.deq.virginia.gov/home/showpublisheddocument?id=4303',
                                                                                     target='_blank')))),
                                                              DT::dataTableOutput('multiBCGattributes'),
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
                                                 