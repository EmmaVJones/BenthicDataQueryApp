## this script exists because the app will not run properly if you update the html document

library(shiny)

pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

ui <- fluidPage(theme= "yeti.css",
            navbarPage("CEDS Benthic Data Query Tool",
                       tabPanel('About', uiOutput('markdown')), #includeHTML("BenthicQueryToolHowToNew.html")),
                       # tabPanel('How To',
                       #          uiOutput("BenthicQueryToolHowToNew") ),
                       # 
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
                                                        verbatimTextOutput('test'))))))))
)

server <- function(input, output, session) {
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  
  ###----------------------------------------Single Station Query ---------------------------------------------------------------------------------------------------
  
  # Master Taxa Data
  observe({
    reactive_objects$masterTaxaGenus <- pool %>% tbl("Edas_Benthic_Master_Taxa_View") %>%
      as_tibble() %>%
      # make columns match expected format
      rename('Phylum' = 'PHYLUM_NAME',
             'Class' = 'CLASS_NAME',
             'Subclass' = 'SUBCLASS_NAME',
             'Order' = 'ORDER_NAME',
             'Suborder' = 'SUBORDER_NAME',
             'Superfamily' = 'SUPERFAMILY_NAME',
             'Family' = 'FAMILY_NAME',
             'Subfamily' = 'SUBFAMILY_NAME',
             'Tribe' = 'TRIBE_NAME',
             'Genus' = 'GENUS_NAME',
             'Species' = 'SPECIES_NAME',
             "Final VA Family ID" =  "WBMT_FINAL_FAMILY_ID",
             "FinalID" = "WBMT_FINAL_ID",
             "TolVal" = "WBMT_TOLERANCE_VALUE",
             "FFG" =   "FEEDING_GROUP", 
             "Habit" = "HABIT", 
             "FamFFG" =  "FAMILY_FEEDING_GROUP",
             "FamTolVal" = "WBMT_FAM_TOLERANCE_VALUE",
             "FamHabit" ="FAMILY_HABIT") %>% 
      dplyr::select(Phylum, Class, Subclass, Order, Suborder, Superfamily, Family, Subfamily, Tribe, 
                    Genus, Species, `Final VA Family ID`, FinalID, TolVal, FFG, 
                    Habit, FamFFG, FamTolVal, FamHabit) # keep EDAS Master Taxa list names
    
    if(nrow(reactive_objects$masterTaxaGenus) == 0 ){
      showNotification("CEDS connection not available at present.")   } })
  
  # test station viability in separate object
  observeEvent(input$begin, {
    ## Station Information
    reactive_objects$stationInfo <- pool %>% tbl( "Wqm_Stations_View") %>%
      filter(Sta_Id %in% !! toupper(input$station)) %>%
      as_tibble() 
    
    if(nrow(reactive_objects$stationInfo) == 0){
      showNotification("Not a valid StationID.")   }  })
  
  ## Pull station info from REST service
  WQM_Station_Full_REST <- reactive({
    req(input$begin, nrow(reactive_objects$stationInfo) != 0)
    
    WQM_Station_Full_REST <- suppressWarnings(
      geojson_sf(
        paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
               toupper(input$station),"%27&outFields=*&f=geojson")))
    
    if(nrow(WQM_Station_Full_REST ) > 0){
      WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA))
      WQM_Station_Full_REST <- bind_cols(WQM_Station_Full_REST, st_coordinates(WQM_Station_Full_REST) %>% as.tibble()) %>%
        mutate(Latitude = Y, Longitude = X) # add lat/lng in DD
    } else { # station doesn't yet exist in WQM full dataset
      # get what we can from CEDS
      stationGISInfo <- pool %>% tbl( "WQM_Sta_GIS_View") %>%
        filter(Station_Id %in% !! toupper(input$station)) %>%
        as_tibble() 
      # pull a known station to steal data structure
      WQM_Station_Full_REST <- suppressWarnings(
        geojson_sf(
          paste0("http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
        mutate(WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) %>%
        st_drop_geometry()
      WQM_Station_Full_REST <- bind_rows(WQM_Station_Full_REST[0,],
                                         tibble(STATION_ID = stationGISInfo$Station_Id, 
                                                Latitude = stationGISInfo$Latitude,
                                                Longitude = stationGISInfo$Longitude,
                                                BASINS_HUC_8_NAME = stationGISInfo$Huc6_Huc_8_Name, 
                                                BASINS_VAHU6 = stationGISInfo$Huc6_Vahu6) ) %>%
        st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
                 remove = F, # don't remove these lat/lon cols from df
                 crs = 4326)    }
    
    return(WQM_Station_Full_REST)})
  
  
  ## Pull Station Information 
  observeEvent(nrow(reactive_objects$stationInfo) > 0, {
    ## update Station Information after ensuring station valid
    reactive_objects$stationInfoFin <- left_join(pool %>% tbl("Wqm_Stations_View") %>%  # need to repull data instead of calling stationInfo bc app crashes
                                                   filter(Sta_Id %in% !! toupper(input$station)) %>%
                                                   as_tibble() %>%
                                                   # add link to data and add link to internal GIS web app with WQS layer on there
                                                   mutate(`CEDS Station View Link` = paste0("<b><a href='https://ceds.deq.virginia.gov/ui#wqmStations/",
                                                                                            Sta_Id,"'", 
                                                                                            " target= '_blank'> View Monitoring Station in CEDS</a></b>"),
                                                          `DEQ GIS Web App Link` =  paste0("<b><a href='https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,",
                                                                                           Sta_Id, 
                                                                                           "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);", 
                                                                                           ";2020%20Draft%20ADB%20WQA%20Layers;2020%20Rivers%20(Any%20Use)&level=14' target='_blank'>View Monitoring Station in DEQ Staff App</a></b>" )) %>%
                                                   dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, everything()), 
                                                 #filter(WQM_Station_View, Sta_Id %in% toupper(input$station)), # need to filter instead of calling stationInfo bc app crashes
                                                 dplyr::select(WQM_Station_Full_REST(), #WQM_STATIONS_FINAL, 
                                                               STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                                               EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                                               WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III),
                                                 by = c('Sta_Id' = 'STATION_ID')) %>%
      dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                    EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                    WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())
    
    ## Station Geospatial Information
    reactive_objects$stationInfo_sf <- WQM_Station_Full_REST()#filter(WQM_STATIONS_FINAL, STATION_ID %in% toupper(input$station) )
    
    ## Station Sampling Information
    reactive_objects$stationInfoSampleMetrics <- reactive_objects$stationInfo_sf %>%
      group_by(STATION_ID) %>%
      mutate(`Years Sampled` = WQM_YRS_YEAR) %>% #paste0(year(WQM_YRS_YEAR))) %>% # for when column coming in as date
      dplyr::select(STATION_ID, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`,WQM_SPG_DESCRIPTION) %>%
      st_drop_geometry() %>%
      group_by(STATION_ID, `Years Sampled`) %>%
      summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))
    
    ## Benthic Information
    reactive_objects$stationBenthics <- pool %>% tbl("Edas_Benthic_View") %>%
      filter(STA_ID %in% !! toupper(input$station)) %>%
      as_tibble() %>%
      rename( "StationID" = "STA_ID",
              "BenSampID"  = "WBS_SAMP_ID",
              "RepNum" = "WBS_REP_NUM",
              "FinalID" = "WBMT_FINAL_ID",
              "Individuals" = "WBE_INDIVIDUALS",
              "ID Comments" = "WBE_COMMENT",
              "Entered By" = "WBE_INSERTED_BY", # not in EDAS table but good info
              "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
              "Entered Date" = "WBE_INSERTED_DATE") %>%
      mutate(`Excluded Taxa` = ifelse(WBE_EXCLUDED_TAXA_YN == "Y", -1, 0)) %>%
      dplyr::select(StationID, BenSampID, RepNum, FinalID, Individuals, `Excluded Taxa`, `ID Comments`, Taxonomist, `Entered By`, `Entered Date`)
    
    
    ## Benthic Sampling Information
    reactive_objects$stationInfoBenSamps <- pool %>% tbl("Edas_Benthic_Sample_View") %>%
      filter(STA_ID %in% !! toupper(input$station)) %>%
      as_tibble() %>%
      # fix names
      rename( "StationID" = "STA_ID",
              "BenSampID"  = "WBS_SAMP_ID",
              "RepNum" = "WBS_REP_NUM",
              "Number of Grids" = "WBS_GRID_NUM",
              "Sample Comments" = "WBS_COMMENT",
              "Entered By" = "WBS_INSERTED_BY", # not in EDAS table but good info
              "Collected By" = "COLLECTOR_NAME",  # not in EDAS table but good info
              "Entered Date" = "WBS_INSERTED_DATE",
              "Gradient" = "WBCM_DESCRIPTION",
              "Taxonomist" = "TAXONOMIST_NAME",  # not in EDAS table but good info
              "Target Count" = "WBS_TARGET_COUNT",
              "Field Team" = "WBS_FIELD_TEAM",
              "Collection Date" = "FDT_DATE_TIME") %>%
      # Add sample season 
      mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                          sprintf("%02d",day(`Collection Date`)))),
             Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                                monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                                TRUE ~ as.character("Outside Sample Window"))) %>%
      left_join(dplyr::select(reactive_objects$stationInfoFin, Sta_Id, Sta_Desc)  %>% distinct(Sta_Id, .keep_all = T), by = c('StationID' = 'Sta_Id')) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, RepNum, `Collection Date`, `Sample Comments`, `Collected By`, `Field Team`, `Entered By`,
                    Taxonomist, `Entered Date`, Gradient, `Target Count`, `Number of Grids`, Season) %>%
      arrange(`Collection Date`)
    
    
    
    ## Habitat data must be reactive to adjusted to benthic or habitat date filter
    reactive_objects$habSampleStation <- pool %>% tbl("Edas_Habitat_Sample_View") %>%
      filter(STA_ID %in% !! toupper(input$station)) %>%
      as_tibble() %>%
      rename("StationID" = "STA_ID",
             "HabSampID" = "WHS_SAMP_ID",
             "Entered Date" = "WHS_INSERTED_DATE",
             "Entered By" = "WHS_INSERTED_BY",
             "Field Team" = "WHS_FIELD_TEAM",
             "HabSample Comment" = "WHS_COMMENT",
             "Gradient" = "WSGC_DESCRIPTION",
             "Collection Date" = "FDT_DATE_TIME") %>%
      # Add sample season 
      mutate(monthday = as.numeric(paste0(sprintf("%02d",month(`Collection Date`)),
                                          sprintf("%02d",day(`Collection Date`)))),
             Season = case_when(monthday >= 0215 & monthday <= 0615 ~ 'Spring',
                                monthday >= 0815 & monthday <= 1215 ~ 'Fall',
                                TRUE ~ as.character("Outside Sample Window"))) %>%
      dplyr::select(HabSampID, StationID, `Collection Date`, `Entered By`, `Entered Date`, `Field Team`, `HabSample Comment`, Gradient, Season)
    
    # other hab values have to be reactive
    
    
    # Tolerance Value Stuff
    reactive_objects$vmast <- reactive_objects$masterTaxaGenus %>%
      # get Family level tolerance value, FFG
      rename('GenusTolVal' = 'TolVal',
             'TolVal' = 'FamTolVal',
             'GenusFFG' = 'FFG',
             'FFG' = 'FamFFG',
             'GenusHabit' = 'Habit',
             'Habit' = 'FamHabit') %>%
      mutate(e=ifelse(Order=="Ephemeroptera", 1, 0),
             p=ifelse(Order=="Plecoptera",1,0),
             t=ifelse(Order=="Trichoptera", 1, 0),
             tmin=ifelse((Order=="Trichoptera" & Family != "Hydropsychidae") | 
                           (Order=="Trichoptera" & is.na(Family)) , 1, 0), 
             ept=ifelse(e+p+t>0,1,0), 
             scraper = ifelse(FFG=="Scraper", 1, 0),
             chiro = ifelse(Family=="Chironomidae",1, 0),
             ptmin = ifelse(p + tmin > 0,1,0),
             `clinger-HS` = ifelse(Habit == 'Clinger' & ! Family %in% c("Hydropsychidae","Simuliidae"), 1, 0)) %>%
      # Then put it in long format so it can be merged to and input taxa list
      select(`Final VA Family ID`,TolVal, e,p,t, ept,ptmin, scraper, chiro,`clinger-HS`) %>% 
      distinct(`Final VA Family ID`, .keep_all = T) %>% # drop multiple rows bc working back to family level data from genus
      filter(!is.na(`Final VA Family ID`)) %>%
      pivot_longer(-`Final VA Family ID`, names_to = 'metric', values_to = 'metric_val') %>%
      #  pivot_longer(-`Final VA Family ID`, names_to = 'metric', values_to = 'metric_val') %>%
      filter(!is.na(metric_val))  })
  
  # Habitat pull after initial hab samps data available
  observe({
    req(nrow(reactive_objects$habSampleStation) >0)
    reactive_objects$habValuesStation <- pool %>% tbl("Edas_Habitat_Values_View") %>%
      filter(WHS_SAMP_ID %in% !! reactive_objects$habSampleStation$HabSampID) %>%
      as_tibble() %>%
      rename("HabSampID" = "WHS_SAMP_ID",
             "HabParameter" = "WHVP_CODE",
             "HabParameterDescription" = "WHVP_DESCRIPTION",
             "HabValue" = "WHV_HAB_VALUE",
             "HabValue Comment" = "WHV_COMMENT") %>%
      dplyr::select(HabSampID, HabParameter, HabParameterDescription, HabValue, `HabValue Comment`)
    
    reactive_objects$habObsStation <- pool %>% tbl("Edas_Habitat_Observations_View") %>%
      filter(WHS_SAMP_ID %in% !! reactive_objects$habSampleStation$HabSampID) %>%
      as_tibble() %>%
      rename("HabSampID" = "WHS_SAMP_ID",
             "ObsParameter" = "WOBP_CODE",
             "ObsParameterDescription" = "WOBP_DESCRIPTION",
             "ObsValue" = "WOB_OBS_VALUE") %>%
      dplyr::select(HabSampID, ObsParameter, ObsParameterDescription, ObsValue)  })
  
  
  ## Display Station Information
  output$stationInfoTable <- DT::renderDataTable({
    req(reactive_objects$stationInfoFin)
    datatable(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX= TRUE, scrollY = '100px',
                             pageLength = nrow(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T)),
                             buttons=list('copy','colvis')))  })
  
  output$stationInfoSampleCodeMetrics <- DT::renderDataTable({
    req(reactive_objects$stationInfoFin)
    datatable(reactive_objects$stationInfoSampleMetrics, 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$stationInfoSampleMetrics),
                             buttons=list('copy','colvis')) ) })
  
  
  ## Map Station Information
  output$stationMap <- renderLeaflet({
    req(reactive_objects$stationInfo)
    # color palette for assessment polygons
    pal <- colorFactor(
      palette = topo.colors(7),
      domain = assessmentRegions$ASSESS_REG)
    pal2 <- colorFactor(
      palette = rainbow(7),
      domain = ecoregion$US_L3NAME)
    
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20,
                                         preferCanvas = TRUE)) %>%
      setView(-79.1, 37.7, zoom=7)  %>% 
      addPolygons(data= ecoregion,  color = 'gray', weight = 1,
                  fillColor= ~pal2(ecoregion$US_L3NAME), fillOpacity = 0.5,stroke=0.1,
                  group="Level III Ecoregions",label = ~US_L3NAME) %>% hideGroup('Level III Ecoregions') %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions", label = ~ASSESS_REG) %>% hideGroup('Assessment Regions') %>% 
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')    
  })
  
  map_proxy <- leafletProxy("stationMap")
  
  # Add layers to map as requested
  #observeEvent(nrow(reactive_objects$stationInfo) > 0, {
  observe({
    req(nrow(reactive_objects$stationInfo) > 0)
    map_proxy %>%
      addCircleMarkers(data = reactive_objects$stationInfo_sf,
                       color='blue', fillColor='yellow', radius = 6,
                       fillOpacity = 0.5,opacity=0.8,weight = 4,stroke=T, group="Selected Station(s)",
                       label = ~STATION_ID, layerId = ~STATION_ID) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Selected Station(s)", "Level III Ecoregions", 'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  })
  
  
  ## Benthic Data Tab
  
  ## Benthics Data Date Range 
  output$dateRange_ <- renderUI({
    req(reactive_objects$stationInfoBenSamps)
    dateRangeInput('dateRange',
                   label = 'Filter Bethic Information By Date Range (YYYY-MM-DD)',
                   start = min(reactive_objects$stationInfoBenSamps$`Collection Date`), 
                   end = Sys.Date()) })  #max(reactive_objects$stationInfoBenSamps$`Collection Date`))  })
  
  output$test <- renderPrint({reactive_objects$stationInfoBenSamps})
  
  ### -------------------------------------How To----------------------------------------------------------------------------------------------------------------------
  
  #output$BenthicQueryToolHowToNew <- renderUI({includeHTML("BenthicQueryToolHowToNew.html")})
  # output$about <- render
  # output$BenthicQueryToolHowToNew <- renderUI({includeHTML("BenthicQueryToolHowToNew.html")})
  
  output$markdown <- renderUI({includeHTML("BenthicQueryToolHowToNew.html")})
    #renderUI({HTML(markdown::markdownToHTML(knit('BenthicQueryToolHowToNew.html', quiet = TRUE)))  })
}

shinyApp(ui, server)