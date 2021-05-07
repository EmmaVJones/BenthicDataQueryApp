source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
county <- st_read('data/GIS/VACountyBoundaries.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN)))
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) %>%
  dplyr::select(SUBBASIN, SubbasinVAHU6code)





shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  
  ###----------------------------------------Single Station Query ---------------------------------------------------------------------------------------------------
  
  
  ## Pass URL to app to autofill input$station from other DEQ applications
  observe({
    # this was crucial to figuring load on certain tab: https://stackoverflow.com/questions/33021757/externally-link-to-specific-tabpanel-in-shiny-app
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['StationID']])) {
      updateTabsetPanel(session,  inputId = "someID", 'SingleStation')  # key for passing URL to specific Tab
      updateTextInput(session, "station", value = query[['StationID']])
    }
  })
  
  
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
    
   # show_modal_spinner(spin = 'flower')
    
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
        filter(!is.na(metric_val))  
      
     # remove_modal_spinner()   
      })
    
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
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollX= TRUE, scrollY = '100px',
                               pageLength = nrow(reactive_objects$stationInfoFin %>% distinct(Sta_Id, .keep_all = T)),
                               buttons=list('copy','colvis')))  })
    
    output$stationInfoSampleCodeMetrics <- DT::renderDataTable({
      req(reactive_objects$stationInfoFin)
      datatable(reactive_objects$stationInfoSampleMetrics, 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$stationInfoSampleMetrics),
                               buttons=list('copy','colvis')) ) })
    
    
    ## Map Station Information
    output$stationMap <- renderLeaflet({
      #req(reactive_objects$stationInfo)
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
    
    ### Filter by user input
    observe({
      req(reactive_objects$stationInfoBenSamps, input$dateRange)
      reactive_objects$stationInfoBenSampsDateRange <- filter(reactive_objects$stationInfoBenSamps, as.Date(`Collection Date`) >= input$dateRange[1] & as.Date(`Collection Date`) <= input$dateRange[2]) %>%
        {if(input$rarifiedFilter)
          filter(.,  `Target Count` == 110)
          #filter(., str_detect(BenSampID, 'R110$')) # must end with R110
          ###filter(., grepl( 'R110', BenSampID))
          else . } %>%
        {if(!is.null(input$repFilter))
          filter(., RepNum %in% input$repFilter)
          else . }  %>%
        {if(input$boatableFilter)
          filter(., Gradient != 'Boatable')
          else . } })
    
    stationBenthicsDateRange <- reactive({ 
      req(reactive_objects$stationInfoBenSampsDateRange)
      filter(reactive_objects$stationBenthics, BenSampID %in% reactive_objects$stationInfoBenSampsDateRange$BenSampID) %>%
        left_join(dplyr::select(reactive_objects$stationInfoBenSampsDateRange, BenSampID, `Collection Date`)) %>%
        dplyr::select(`Collection Date`, everything()) %>%
        arrange(`Collection Date`)})
    
    SCIresults <- reactive({
      req( stationBenthicsDateRange())
      SCIresults <- SCI(stationBenthicsDateRange(), input$SCIchoice, reactive_objects$stationInfoBenSampsDateRange,  reactive_objects$masterTaxaGenus, reactive_objects$vmast) %>%
        mutate_if(is.numeric, round, digits=2) %>%# rounds all numeric columns to 2 decimal places
        arrange(`Collection Date`)
      SCIresults$Season <-  factor(SCIresults$Season,levels=c("Spring","Outside Sample Window","Fall"))#,ordered=T)
      return(SCIresults)})
    
    observe({
      req(reactive_objects$stationInfoBenSampsDateRange, SCIresults())
      reactive_objects$SCIresults <- SCIresults()  # for report, creates endless loop if not in separate reactive
      reactive_objects$avgSCI <- averageSCI(reactive_objects$stationInfoBenSampsDateRange, SCIresults()) 
      reactive_objects$sciTable <- dplyr::select(SCIresults(), StationID, Sta_Desc, `Collection Date`, RepNum, `Target Count`, `Number of Grids`, Season, BenSampID:`SCI Threshold`) %>% # inclusive of different column types 
        arrange(`Collection Date`)}) # inclusive of different column types 
    
    
    
    ## Sampling Metrics Tab
    output$averageSamplingMetrics <- renderDataTable({
      req(reactive_objects$avgSCI)
      datatable(reactive_objects$avgSCI, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bit', scrollY = '250px', pageLength = nrow(reactive_objects$avgSCI),buttons=list('copy','colvis'))) })
    
    output$collectorMetrics <- renderDataTable({
      req(reactive_objects$stationInfoBenSampsDateRange,stationBenthicsDateRange())
      z <- uniqueCollector(reactive_objects$stationInfoBenSampsDateRange)
      datatable(z, rownames = F, escape= F,extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollY = '75px',
                               pageLength = nrow(z),buttons=list('copy'))) })
    
    output$taxonomistMetrics <- renderDataTable({
      req(reactive_objects$stationInfoBenSampsDateRange,stationBenthicsDateRange())
      z <- uniqueTaxonomist(reactive_objects$stationInfoBenSampsDateRange)
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollY = '75px',
                               pageLength = nrow(z),buttons=list('copy'))) })
    
    output$samplingInformation <- renderDataTable({
      req(reactive_objects$stationInfoBenSampsDateRange,stationBenthicsDateRange())
      datatable(reactive_objects$stationInfoBenSampsDateRange  %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`),
                         `Entered Date` = as.Date(`Entered Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',
                               pageLength = nrow(reactive_objects$stationInfoBenSampsDateRange), buttons=list('copy','colvis')))})
    
    
    ## SCI Results Tab
    
    ### SCI plotly
    output$SCIplot <- renderPlotly({
      req(SCIresults())
      if(unique(SCIresults()$SCI) == 'VSCI'){sciLimit <- 60} else {sciLimit <- 40}
      
      SCIresults <- SCIresults() %>%
        mutate(SeasonGradient = as.factor(paste0(Season, " (",Gradient,")")),
               SeasonGradientColor = case_when(SeasonGradient == "Spring (Riffle)" ~  "#66C2A5",
                                               SeasonGradient == "Spring (Boatable)" ~  "#66C2A5",
                                               SeasonGradient == "Spring (MACS)" ~  "#66C2A5",
                                               SeasonGradient == "Outside Sample Window (Riffle)" ~ "#FC8D62",
                                               SeasonGradient == "Outside Sample Window (Boatable)" ~ "#FC8D62",
                                               SeasonGradient == "Outside Sample Window (MACS)" ~ "#FC8D62",
                                               SeasonGradient == "Fall (Riffle)" ~ "#8DA0CB",
                                               SeasonGradient == "Fall (Boatable)" ~ "#8DA0CB",
                                               SeasonGradient == "Fall (MACS)" ~ "#8DA0CB",
                                               TRUE ~ as.character(NA)) )
      SCIresults$SeasonGradient <- factor(SCIresults$SeasonGradient,levels=c("Spring (Riffle)",
                                                                             "Spring (Boatable)",
                                                                             "Spring (MACS)",
                                                                             "Outside Sample Window (Riffle)",
                                                                             "Outside Sample Window (Boatable)",
                                                                             "Outside Sample Window (MACS)",
                                                                             "Fall (Riffle)",
                                                                             "Fall (Boatable)",
                                                                             "Fall (MACS)"))#,ordered=T)
      # organize reps into separate objects so traces don't stack, drop extra levels so colors turn out correctly
      rep1s <- SCIresults %>% filter(RepNum == 1) %>% droplevels()
      rep2s <- SCIresults %>% filter(RepNum == 2) %>% droplevels()
      plot_ly(rep1s,
              x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
              color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
              stroke = list(color = 'rgb(0, 0, 0)', width = 3),
              hoverinfo="text", text=~paste(sep="<br>",
                                            paste("StationID: ", StationID),
                                            paste("Collection Date: ", as.Date(`Collection Date`)),
                                            paste('Replicate: ', RepNum),
                                            paste("Collector ID: ",`Collected By`),
                                            paste("BenSampID: ", BenSampID),
                                            paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                            paste("Gradient: ", Gradient)),
              name = ~paste('Rep 1', SeasonGradient)) %>%
        {if(nrow(rep2s) > 0)
          add_trace(., data = rep2s, inherit = FALSE,
                    x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar',
                    color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
                    stroke = list(color = 'rgb(0, 0, 0)', width = 3),
                    hoverinfo="text", text=~paste(sep="<br>",
                                                  paste("StationID: ", StationID),
                                                  paste("Collection Date: ", as.Date(`Collection Date`)),
                                                  paste('Replicate: ', RepNum),
                                                  paste("Collector ID: ",`Collected By`),
                                                  paste("BenSampID: ", BenSampID),
                                                  paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                                  paste("Gradient: ", Gradient)),
                    name = ~paste('Rep 2', SeasonGradient)) 
          else .} %>%
        
        layout(#showlegend=FALSE,
          shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
          yaxis=list(title="SCI"),
          xaxis=list(title="Sample Date",tickfont = list(size = 10),
                     type = 'date',tickformat = "%Y"))    })
    
    
    
    ### SCI table
    output$SCIresultsTable <- DT::renderDataTable({
      req(reactive_objects$sciTable)
      datatable(reactive_objects$sciTable  %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bit', scrollX= TRUE, scrollY = '250px', 
                               pageLength= nrow(reactive_objects$sciTable), buttons=list('copy','colvis')))  })
    
    
    
    
    # Benthic Data Visualization Tab
    
    # SCI Seasonal Crosstab Table
    
    #    output$metric_UI <- renderUI({
    #      req(reactive_objects$SCIresults)
    #      selectInput('metric', 'Select Metric to Analyze', 
    #                  choices = names(dplyr::select(reactive_objects$SCIresults, `Family Total Taxa`:`SCI Score`) %>%
    #                                    dplyr::select(`SCI Score`, everything()))) }) # crazy way to select unknown columns
    #    
    #    output$SCIseasonalCrosstab <- DT::renderDataTable({
    #      req(reactive_objects$SCIresults, input$metric)
    #      z <- SCI_crosstab_Billy(crosstabTemplate, reactive_objects$SCIresults,
    #                              WQM_Station_Full_REST() %>% st_drop_geometry(), input$metric)#`SCI Score`)
    #      
    #      datatable(dplyr::select(z, StationID:`Family Total Taxa`) %>% dplyr::select(-c(`Family Total Taxa`)), # crazy way to include unexpected columns
    #                rownames = F, escape= F, extensions = 'Buttons',
    #                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
    #                               pageLength = nrow(z),
    #                               buttons=list('copy','colvis')))  })
    
    
    
    # Benthic Individuals BenSamp Crosstab
    
    output$benthicIndividualsBensampCrosstab <- DT::renderDataTable({ req(stationBenthicsDateRange(), input$genusOrFamily)
      z <- benthics_crosstab_Billy(stationBenthicsDateRange(), reactive_objects$masterTaxaGenus, genusOrFamily = input$genusOrFamily)
      #print(z)
      
      datatable(z, #dplyr::select(z, StationID:`Collection Date`) %>% dplyr::select(-c(`Collection Date`)), # crazy way to include unexpected columns
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),
                               buttons=list('copy','colvis')))  })
    
    
    
    ## BCG Attributes Table
    output$BCGattributes <-  DT::renderDataTable({ req(stationBenthicsDateRange())
      BCGtable <- stationBenthicsDateRange() %>% 
        dplyr::select(`Collection Date`:Individuals) %>% 
        mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
        left_join(dplyr::select(BCGattVal, FinalID, `Taxonomic Notes`: `BCGatt Comment`), by = 'FinalID')
      datatable(BCGtable,  rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(BCGtable), buttons=list('copy','colvis'))) %>% 
        formatStyle(names(BCGtable)[c(8:21, 23:29)], backgroundColor = styleInterval(bcgAttributeColors $brks, bcgAttributeColors$clrs)) })
    
    
    ## FFG Data and Plot
    FFGdata <- reactive({req(stationBenthicsDateRange(), input$FFGgenusOrFamily)
      left_join(stationBenthicsDateRange(), 
                dplyr::select(reactive_objects$masterTaxaGenus, FinalID,`Final VA Family ID`:FamHabit), 
                         by = 'FinalID') %>% 
        dplyr::select(-c(Taxonomist:`Entered Date`)) %>% 
        {if(input$FFGgenusOrFamily == 'Genus')
          group_by(., StationID, `Collection Date`, RepNum, BenSampID, FFG) %>% 
            summarise(Count = sum(Individuals, na.rm = T)) %>% 
            mutate(TotalCount = sum(Count),
                   Percent = Count / TotalCount * 100) 
          else group_by(., StationID, `Collection Date`, RepNum, BenSampID, FamFFG) %>% 
            summarise(Count = sum(Individuals, na.rm = T)) %>% 
            mutate(TotalCount = sum(Count),
                   Percent = Count / TotalCount * 100) } })
    
    output$FFGplot <- renderPlot({req(FFGdata(), input$FFGgenusOrFamily)
      FFGstackedBarPlotFunction(FFGdata(), input$FFGgenusOrFamily, input$FFGxAxis)})
    
    output$FFGdataTable <-  DT::renderDataTable({ req(FFGdata())
      datatable(FFGdata(),  rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(FFGdata()), buttons=list('copy',
                                                                        list(extend='csv',filename=paste0('FFGdata',input$station, Sys.Date())),
                                                                        list(extend='excel',filename=paste0('FFGdata',input$station, Sys.Date())),
                                                                        'colvis'))) })
    
    
    
    
    
    ### Benthics Data Download Tab
    rawBenthicsCrosstab <- reactive({
      req(stationBenthicsDateRange())
      stationBenthicsDateRange() %>%
        group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
        arrange(FinalID) %>%
        pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = FinalID, values_from = Individuals) %>%
        arrange(`Collection Date`) })
    
    output$rawBenthicCrosstabGenus <- DT::renderDataTable({
      req(rawBenthicsCrosstab())
      datatable(rawBenthicsCrosstab() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(rawBenthicsCrosstab()), buttons=list('copy','colvis')))  })
    
    
    output$rawBenthicGenus <- DT::renderDataTable({ req(stationBenthicsDateRange())
      z <- stationBenthicsDateRange() %>% 
        mutate(`Collection Date` = as.Date(`Collection Date`),
               `Entered Date` = as.Date(`Entered Date`)) %>% 
        dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, everything()) 
      datatable(z, rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(z), buttons=list('copy','colvis')))  })
    
    
    output$rawBenthicCrosstabFamily <- DT::renderDataTable({req(stationBenthicsDateRange())
      z <- stationBenthicsDateRange() %>%
        left_join(dplyr::select(reactive_objects$masterTaxaGenus, `Final VA Family ID`,`FinalID`), by = 'FinalID') %>% 
        group_by(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`) %>% 
        mutate(Individuals = sum(Individuals, na.rm = T)) %>% 
        distinct(`Final VA Family ID`, .keep_all = T) %>% 
        dplyr::select(-FinalID) %>% 
        ungroup() %>% 
        group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
        arrange( `Final VA Family ID`) %>%
        pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = `Final VA Family ID`, values_from = Individuals) %>%
        arrange(`Collection Date`)
      datatable(z %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(z), buttons=list('copy','colvis')))  })
    
    
    output$rawBenthicFamily <- DT::renderDataTable({req(stationBenthicsDateRange())
      z <- stationBenthicsDateRange() %>%
        left_join(dplyr::select(reactive_objects$masterTaxaGenus, `Final VA Family ID`,`FinalID`), by = 'FinalID') %>% 
        group_by(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`) %>% 
        mutate(Individuals = sum(Individuals, na.rm = T)) %>% 
        distinct(`Final VA Family ID`, .keep_all = T) %>% 
        dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, Individuals, 
                      Taxonomist, `Entered By`, `Entered Date`) %>% 
        arrange(`Collection Date`)
      datatable(z %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(z), buttons=list('copy','colvis')))  })
    
    
    ## BSA benthics Data download tab
    output$BSAbenthicData <- DT::renderDataTable({req(stationBenthicsDateRange())
      z <- BSAbenthicOutputFunction(input$SCIchoice, SCIresults(), WQM_Station_Full_REST())
      datatable(z, rownames = F, escape= F, extensions = 'Buttons',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),  buttons=list('copy',
                                                                   list(extend='csv',filename=paste0('BSAbenthicTemplateData',input$station, Sys.Date())),
                                                                   list(extend='excel',filename=paste0('BSAbenthicTemplateData',input$station, Sys.Date())),
                                                                   'colvis')), selection = 'none') })
    

    
    
    
    ## Habitat Tab
    
    ### Filter by user input
    output$habitatDateRange_ <- renderUI({
      req(reactive_objects$stationInfoBenSamps, input$useBenthicDateRange == 'Filter Custom Date Range',
          nrow(reactive_objects$habSampleStation) > 0)
      dateRangeInput('habitatDateRange',
                     label = 'Filter Habitat Information By Date Range (YYYY-MM-DD)',
                     start = min(reactive_objects$habSampleStation$`Collection Date`), 
                     end = max(reactive_objects$habSampleStation$`Collection Date`))  })
    
    habitatSampleDateRange <- reactive({ 
      req(reactive_objects$habSampleStation)
      if(input$useBenthicDateRange == "Use Benthic Data Date Filter"){
        z <- filter(reactive_objects$habSampleStation, as.Date(`Collection Date`) >= input$dateRange[1] & as.Date(`Collection Date`) <= input$dateRange[2])  
      } else {
        z <- filter(reactive_objects$habSampleStation, as.Date(`Collection Date`) >= input$habitatDateRange[1] & as.Date(`Collection Date`) <= input$habitatDateRange[2])  }
      # filter by user decisions
      z %>%
        {if(!is.null(input$gradientFilter))
          filter(., Gradient %in% input$gradientFilter)
          else . } 
    })
    
    # these need to be reactives to not bomb out app on input custom date range
    habValuesStationDateRange <- reactive({
      req(habitatSampleDateRange())
      filter(reactive_objects$habValuesStation, HabSampID %in%  habitatSampleDateRange()$HabSampID) %>%
        left_join(dplyr::select(habitatSampleDateRange(), HabSampID, `Collection Date`)) %>%
        dplyr::select(`Collection Date`, everything())  }) 
    habObsStationDateRange <-  reactive({
      req(habitatSampleDateRange())
      filter(reactive_objects$habObsStation, HabSampID %in%  habitatSampleDateRange()$HabSampID) %>%
        left_join(dplyr::select(habitatSampleDateRange(), HabSampID, `Collection Date`)) %>%
        dplyr::select(`Collection Date`, everything()) })
    
    habValues_totHab <- reactive({
      req(habitatSampleDateRange())
      habitatSampleDateRange() %>%
        group_by(HabSampID) %>%
        # get total habitat values
        left_join(totalHabScore(reactive_objects$habValuesStation), by = 'HabSampID') %>%
        mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
        arrange(`Collection Date`) })
    
    avgTotalHab <- reactive({
      req(habValues_totHab())
      totalHabScoreAverages(habValues_totHab())  })
    
    ## Habitat Sampling Metrics Tab
    output$averageTotalHabitatMetrics <-  renderDataTable({
      req(avgTotalHab())
      datatable(avgTotalHab(), rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bit', scrollY = '250px', pageLength = nrow(avgTotalHab()), buttons=list('copy','colvis'))) })
    
    output$fieldTeamMetrics <- renderDataTable({
      req(habitatSampleDateRange())
      z <- uniqueFieldTeam(habitatSampleDateRange())
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })
    
    output$habObsMetrics <- renderDataTable({
      req(habObsStationDateRange())
      z <- habObsMetrics(habObsStationDateRange())
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })
    
    output$habitatSamplingInformation <- renderDataTable({
      req(habitatSampleDateRange())
      datatable(habitatSampleDateRange() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`),
                         `Entered Date` = as.Date(`Entered Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bit', scrollX = TRUE, scrollY = '150px',
                               pageLength = nrow(habitatSampleDateRange()), buttons=list('copy','colvis'))) })
    
    ## Detailed Habitat Results Tab
    
    output$totalHabitatPlot <- renderPlotly({
      req(habValues_totHab())
      habValues_totHab() %>%
        plot_ly( x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', 
                 color = ~Season, width = 0.5, stroke = list(color = 'rgb(0, 0, 0)', width = 3),
                 #marker = list(line = list(width = 1.5)),
                 hoverinfo="text", text=~paste(sep="<br>",
                                               paste("StationID: ", StationID),
                                               paste("Collection Date: ", as.Date(`Collection Date`)),
                                               #paste('Replicate: ', RepNum),
                                               paste("Field Team: ",`Field Team`),
                                               paste("HabSampID: ", HabSampID),
                                               paste("Total Habitat Score: ", `Total Habitat Score`))) %>%
        layout(#showlegend=FALSE,
          #shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
          yaxis=list(title="SCI"),
          xaxis=list(title="Sample Date",tickfont = list(size = 10),
                     type = 'date',tickformat = "%Y")) })
    
    output$habitatResultsTable <- DT::renderDataTable({
      req(habValues_totHab())
      datatable(habValues_totHab() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`),
                         `Entered Date` = as.Date(`Entered Date`)),
                rownames = F, escape= F, extensions = 'Buttons',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habValues_totHab()), buttons=list('copy','colvis')))  })
    
    
    
    ## Habitat Data download Tab
    habitatValuesCrosstab <- reactive({
      req(habValuesStationDateRange())
      habValuesStationDateRange() %>%
        group_by(HabSampID, `Collection Date`) %>%
        arrange(HabParameterDescription) %>%
        pivot_wider(id_cols = c('HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
        left_join(dplyr::select(habValues_totHab(), HabSampID, `Total Habitat Score`), by = 'HabSampID')})
    
    output$habitatValuesCrosstab <- DT::renderDataTable({
      req(habitatValuesCrosstab())
      datatable(habitatValuesCrosstab() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons',  selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habitatValuesCrosstab()), buttons=list('copy','colvis')))  })
    
    habitatObservationsCrossTab <- reactive({
      req(habObsStationDateRange())
      habObsStationDateRange() %>%
        group_by(HabSampID, `Collection Date`) %>%
        arrange(ObsParameterDescription) %>%
        pivot_wider(id_cols = c('HabSampID','Collection Date'), names_from = ObsParameterDescription, values_from = ObsValue) })
    
    output$habitatObservationsCrossTab <- DT::renderDataTable({
      req(habitatObservationsCrossTab())
      datatable(habitatObservationsCrossTab() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons',  selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habitatObservationsCrossTab()), buttons=list('copy','colvis')))  })
    
    
    
    
    output$habitatValues <- DT::renderDataTable({
      req(habValuesStationDateRange())
      datatable(habValuesStationDateRange() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons',  selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habValuesStationDateRange()), buttons=list('copy','colvis')))  })
    
    output$habitatObservations <- DT::renderDataTable({
      req(habObsStationDateRange())
      datatable(habObsStationDateRange() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(`Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habObsStationDateRange()), buttons=list('copy','colvis') )) })
    
    
    ### BSA habitat data download tab
    output$BSAhabitatData <- DT::renderDataTable({req(habValuesStationDateRange())
      z <- BSAhabitatOutputFunction(habValues_totHab(), habValuesStationDateRange())
      datatable(z, rownames = F, escape= F, extensions = 'Buttons',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),  buttons=list('copy',
                                                                   list(extend='csv',filename=paste0('BSAhabitatTemplateData',input$station, Sys.Date())),
                                                                   list(extend='excel',filename=paste0('BSAhabitatTemplateData',input$station, Sys.Date())),
                                                                   'colvis')), selection = 'none') })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ### -------------------------------Multistation Query-------------------------------------------------------------------------------------------------------
    
    
    # Query by spatial filters
    output$spatialFilters_assessmentRegion <- renderUI({req(input$queryType == 'Spatial Filters')
      list(helpText('Interactive cross validation between filters applies in this section.'),
           selectInput('assessmentRegionFilter','Assessment Region', choices = unique(subbasins$ASSESS_REG), multiple = T)) })
    
    output$spatialFilters_subbasin <- renderUI({req(input$queryType == 'Spatial Filters')
      choices <- if(is.null(input$assessmentRegionFilter)){unique(subbasins$SUBBASIN)
      }else{
        filter(subbasins, ASSESS_REG %in% input$assessmentRegionFilter) %>%
          distinct(SUBBASIN) %>% st_drop_geometry() %>%  pull()  }
      selectInput('subbasinFilter','Basin', choices = choices, multiple = T) })
    
    output$spatialFilters_VAHU6 <- renderUI({  req(input$queryType == 'Spatial Filters')
      if(is.null(input$assessmentRegionFilter) & is.null(input$subbasinFilter)){choices <- unique(assessmentLayer$VAHU6)}
      if(is.null(input$assessmentRegionFilter) & !is.null(input$subbasinFilter)){
        choices <- filter(st_drop_geometry(subbasins), SUBBASIN %in% input$subbasinFilter) %>%
          left_join(subbasinVAHU6crosswalk, by='SUBBASIN') %>%
          left_join(st_drop_geometry(assessmentLayer), by=c('SubbasinVAHU6code'='VAHUSB') ) %>%
          distinct(VAHU6) %>% pull() }
      if(!is.null(input$assessmentRegionFilter) & is.null(input$subbasinFilter)){
        choices <- filter(st_drop_geometry(assessmentLayer), ASSESS_REG %in% input$assessmentRegionFilter) %>%
          distinct(VAHU6) %>% pull()  }
      if(!is.null(input$assessmentRegionFilter) & !is.null(input$subbasinFilter)){
        choices <- filter(st_drop_geometry(subbasins), SUBBASIN %in% input$subbasinFilter) %>%
          left_join(subbasinVAHU6crosswalk, by='SUBBASIN') %>%
          left_join(st_drop_geometry(assessmentLayer), by=c('SubbasinVAHU6code'='VAHUSB') ) %>%
          distinct(VAHU6) %>% pull() }
      selectInput('VAHU6Filter','VAHU6', choices = choices, multiple = T) })
    
    output$spatialFilters_Ecoregion <- renderUI({req(input$queryType == 'Spatial Filters')
      list(helpText("Additional filter(s) applied on 'Pull Stations' request. "),
           selectInput('ecoregionFilter','Level 3 Ecoregion', choices = unique(ecoregion$US_L3NAME), multiple = T)) })
    
    output$spatialFilters_County <- renderUI({req(input$queryType == 'Spatial Filters')
      selectInput('countyFilter','County/City', choices = sort(unique(county$NAME)), multiple = T) })
    
    output$dateRange_multistationUI <- renderUI({
      req(input$queryType == 'Spatial Filters')
      dateRangeInput('dateRange_multistation',
                     label = 'Filter Stations By Last Sample Date Range (YYYY-MM-DD)',
                     start = as.Date("1970-01-01"), 
                     end = as.Date(Sys.Date()- 7))   })
    
    # Query by wildcard selection
    
    output$wildcardSelection <- renderUI({
      list(
        helpText('Remember, use % as your wildcard, not *'),
        textInput('wildcardText', 'Filter by StationID LIKE', value = NULL, placeholder = '2A%') )     })
    
    observeEvent(input$begin_multistation_wildcard,{
      #     if(!is.null(input$wildcardText)){# != ""){
      show_modal_spinner(spin = 'flower')
      
      reactive_objects$wildcardResults <- sqldf(paste0('SELECT * FROM benSamps WHERE StationID like "',
                                                       input$wildcardText, '"')) # need to use benSamps bc sqldf doesn't like sf objects
      if(nrow(reactive_objects$wildcardResults) > 0 ){
        reactive_objects$WQM_Stations_Filter <- filter(benSampsStations, StationID %in% reactive_objects$wildcardResults$StationID) 
      } else {
        # if(input$wildcardText != "" & is.null(input$VAHU6Filter) & is.null(input$subbasinFilter) & 
        #      is.null(input$assessmentRegionFilter) & is.null(input$ecoregionFilter)){
        showNotification("No stations found with entered text.")        }
      remove_modal_spinner()    
      })
    #    }
    #    else {reactive_objects$wildcardResults <- NULL 
    #    showNotification("No stations found with entered text.")}
    
    #      print(is.null(input$wildcardText))
    #      print(reactive_objects$wildcardResults)
    #      print(length(reactive_objects$wildcardResults))
    
    #      if(is.null(reactive_objects$wildcardResults)){
    #    if(is.null(input$wildcardText)){
    
    # Query by spatial filter selection
    observeEvent(input$begin_multistation_spatial,{
      show_modal_spinner(spin = 'flower')
      
      reactive_objects$spatialFilter <- benSampsStations %>%
        left_join(WQM_Stations_Spatial, by = 'StationID') %>% 
        # go small to large spatial filters
        {if(!is.null(input$VAHU6Filter))
          filter(., VAHU6 %in% input$VAHU6Filter)
          #st_intersection(., filter(assessmentLayer, VAHU6 %in% VAHU6Filter))
          else .} %>%
        {if(is.null(input$VAHU6Filter) & !is.null(input$subbasinFilter))
          #filter(., Basin_Code %in% subbasinFilter)
          # keeping with spatial filter here bc the subbasin filter options would have to change otherwise
          st_intersection(., filter(subbasins, SUBBASIN %in% input$subbasinFilter))
          else .} %>%
        {if(is.null(input$VAHU6Filter) & !is.null(input$assessmentRegionFilter)) # don't need assessment region filter if VAHU6 available
          filter(., ASSESS_REG %in% input$assessmentRegionFilter)
          #st_intersection(., filter(assessmentRegions, ASSESS_REG %in% assessmentRegionFilter))
          else .} %>%
        {if(!is.null(input$ecoregionFilter))
          filter(., US_L3NAME %in% input$ecoregionFilter)
          #st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
          else .}  %>%
        {if(!is.null(countyFilter))
          filter(., CountyCityName %in% input$countyFilter)
          else .} %>% 
        {if(!is.null(input$dateRange_multistation))
          filter(., StationID %in% filter(benSamps, as.Date(`Collection Date`) >= input$dateRange_multistation[1] & as.Date(`Collection Date`) <= input$dateRange_multistation[2])$StationID)
          else .} %>%
        rename(., `Total Station Visits (Not Sample Reps)` = "Total.Station.Visits..Not.Sample.Reps.") %>%
        dplyr::select(StationID, `Total Station Visits (Not Sample Reps)`) 
      remove_modal_spinner()   
      
      if(nrow(reactive_objects$spatialFilter) > 0 ){
        reactive_objects$WQM_Stations_Filter <- reactive_objects$spatialFilter
      } else {
        # if(input$wildcardText != "" & is.null(input$VAHU6Filter) & is.null(input$subbasinFilter) & 
        #      is.null(input$assessmentRegionFilter) & is.null(input$ecoregionFilter)){
        showNotification("No stations found with entered text.")  }
      })
    
    
    
    
    
    # Query by manual selection
    output$manualSelection <- renderUI({req(input$queryType == 'Manually Specify Stations')
      list(helpText('Begin typing station names and the app will filter available data by input text. Multiple stations are allowed.'),
           selectInput('manualSelection','Station ID', choices = sort(unique(benSampsStations$StationID)), multiple = T)) })
    
    observeEvent(input$begin_multistation_manual, {
      show_modal_spinner(spin = 'flower')
      reactive_objects$WQM_Stations_Filter <- filter(benSampsStations, StationID %in% as.character(input$manualSelection))   
      remove_modal_spinner() 
      })
    
      

    
    
    observe({ req(reactive_objects$WQM_Stations_Filter)
      
      ## Basic Station Info
      reactive_objects$multistationInfoFin <- left_join(Wqm_Stations_View %>%  # need to repull data instead of calling stationInfo bc app crashes
                                                          filter(Sta_Id %in% reactive_objects$WQM_Stations_Filter$StationID) %>%
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
                                                        ########filter(WQM_Station_View, Sta_Id %in% toupper(input$station)), # need to filter instead of calling stationInfo bc app crashes
                                                        dplyr::select(WQM_Station_Full, 
                                                                      STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                                                      EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                                                      WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                                        by = c('Sta_Id' = 'STATION_ID')) %>%
        dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                      EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                      WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything()) 
      
      # Empty station user selection to start with
      reactive_objects$selectedSites <- NULL
      
    })
    
    
    output$multistationMap <- renderLeaflet({
      #req(reactive_objects$WQM_Stations_Filter)
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
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
          circleOptions = FALSE, 
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Level III Ecoregions", 'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')        })
    
    map_proxy_multi <- leafletProxy("multistationMap")
    
    # Add layers to map as requested
    assessmentLayerFilter <- reactive({
      assessmentLayer %>%
        {if(!is.null(input$subbasinFilter))
          filter(., VAHUSB %in% (filter(st_drop_geometry(subbasins), SUBBASIN %in% input$subbasinFilter) %>%
                                   left_join(subbasinVAHU6crosswalk, by='SUBBASIN') %>% distinct(SubbasinVAHU6code) %>% pull()))
          else .} %>%
        {if(!is.null(input$assessmentRegionFilter))
          filter(., ASSESS_REG %in% input$assessmentRegionFilter)
          else .} })
    
    observe({
      req(nrow(reactive_objects$WQM_Stations_Filter) > 0)
      map_proxy_multi %>%
        addCircleMarkers(data = reactive_objects$WQM_Stations_Filter,
                         color='blue', fillColor='gray', radius = 4,
                         fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                         label = ~StationID, layerId = ~StationID,
                         popup = leafpop::popupTable(reactive_objects$WQM_Stations_Filter, zcol=c('StationID', 'Total Station Visits (Not Sample Reps)'))) %>%
        addPolygons(data= assessmentLayerFilter(),  color = 'black', weight = 1,
                    fillColor= 'gray', fillOpacity = 0.5,stroke=0.1,
                    group="VAHU6", label = ~VAHU6) %>% hideGroup('VAHU6') %>% 
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Spatial Filter Station(s)", "VAHU6","Level III Ecoregions", 'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')  })
    
    ## User polygon selection feature
    observeEvent(input$multistationMap_draw_new_feature,{
      
      shape = input$multistationMap_draw_new_feature
      
      # derive polygon coordinates and feature_type from shape input
      polygon_coordinates <- shape$geometry$coordinates
      feature_type <- shape$properties$feature_type
      
      if(feature_type %in% c("rectangle","polygon")) {
        # change user coordinates into sf multipolygon
        poly <- st_sf(what = 'user selected polygon', 
                      geom = st_sfc(st_cast(st_polygon(list(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))), 'MULTIPOLYGON') )) 
        st_crs(poly) <- 4326 # set crs (can't do in one step???)
        
        # select sites inside polygon
        if(is.null(reactive_objects$selectedSites)){
          reactive_objects$selectedSites <- st_intersection(reactive_objects$WQM_Stations_Filter,poly)
        } else {
          reactive_objects$selectedSites <- rbind(reactive_objects$selectedSites, st_intersection(reactive_objects$WQM_Stations_Filter,poly))
        }       
      } })
    
    # Highlight selected sites from polygon
    observe({req(nrow(reactive_objects$selectedSites) > 0)
      map_proxy_multi %>%
        addCircleMarkers(data = reactive_objects$selectedSites,
                         color='blue', fillColor='yellow', radius = 4,
                         fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="User Selected Station(s)",
                         label = ~StationID, layerId = ~StationID,
                         popup = leafpop::popupTable(benSampsStations, zcol=c('StationID', 'Total Station Visits (Not Sample Reps)'))) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("User Selected Station(s)","Spatial Filter Station(s)","VAHU6", "Level III Ecoregions", 'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')  })
    
    # redraw all sites if user selection deleted
    observeEvent(input$multistationMap_draw_deleted_features,{
      reactive_objects$selectedSites <- NULL
      
      map_proxy_multi %>%
        clearGroup(group="User Selected Station(s)") %>%
        addCircleMarkers(data = reactive_objects$WQM_Stations_Filter,
                         color='blue', fillColor='gray', radius = 4,
                         fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Spatial Filter Station(s)",
                         label = ~StationID, layerId = ~StationID,
                         popup = leafpop::popupTable(benSampsStations, zcol=c('StationID', 'Total Station Visits (Not Sample Reps)'))) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Spatial Filter Station(s)","VAHU6","Level III Ecoregions", 'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')    })
    
    # Update "final" site selection after user input
    observe({req(reactive_objects$multistationInfoFin)
      # "final sites"
      reactive_objects$multistationSelection <- reactive_objects$multistationInfoFin %>% 
        {if(!is.null(reactive_objects$selectedSites))
          filter(., Sta_Id %in% reactive_objects$selectedSites$StationID)
          else . }  
      
      ## Station Sampling Information
      reactive_objects$multistationInfoSampleMetrics <- reactive_objects$multistationSelection %>%
        group_by(Sta_Id) %>%
        mutate(`Years Sampled` = paste0(year(WQM_YRS_YEAR))) %>%
        dplyr::select(Sta_Id, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`) %>%
        group_by(Sta_Id, `Years Sampled`) %>%
        summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))           })
    
    ## Display Station Information
    output$multistationInfoTable <- DT::renderDataTable({
      req(reactive_objects$multistationSelection)
      datatable(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T) %>% arrange(Sta_Id), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T)),
                               buttons=list('copy','colvis')))  })
    
    output$multistationInfoSampleMetrics <- DT::renderDataTable({
      req(reactive_objects$multistationInfoSampleMetrics)
      datatable(reactive_objects$multistationInfoSampleMetrics, 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$multistationInfoSampleMetrics),
                               buttons=list('copy','colvis')) ) })
    
    
    # Multistation Benthic Results Tab
    
    # Compile benthic data based on stations selected
    observe({
      req(reactive_objects$multistationSelection)
      reactive_objects$benSamps_Filter <- filter(benSamps, StationID %in% reactive_objects$multistationSelection$Sta_Id) %>%
        filter(as.Date(`Collection Date`) >= input$dateRange_multistation[1] & as.Date(`Collection Date`) <= input$dateRange_multistation[2]) %>%
        # bring in ecoregion info
        left_join(dplyr::select(WQM_Station_Full, WQM_STA_ID, EPA_ECO_US_L3NAME, EPA_ECO_US_L3CODE) %>%
                    distinct(WQM_STA_ID, .keep_all = T),
                  by = c("StationID" = "WQM_STA_ID")) %>%
        # filter by user decisions
        {if(input$multistationRarifiedFilter)
          filter(.,  `Target Count` == 110)
          #filter(., str_detect(BenSampID, 'R110$')) # must end with R110
          ###filter(., grepl( 'R110', BenSampID))
          else . } %>%
        {if(!is.null(input$multistationRepFilter))
          filter(., RepNum %in% input$multistationRepFilter)
          else . } %>%
        {if(input$multistationBoatableFilter)
          filter(., Gradient != 'Boatable')
          else . } 
    })
    
    
    output$dateRange_benSamps_multistationUI <- renderUI({
      req(reactive_objects$benSamps_Filter)
      dateRangeInput('dateRange_benSamps_multistation',
                     label = 'Filter Benthic Data By Date Range (YYYY-MM-DD)',
                     start = as.Date(min(reactive_objects$benSamps_Filter$`Collection Date`)), 
                     end = as.Date(max(reactive_objects$benSamps_Filter$`Collection Date`)))
    })
    

    observe({req(input$dateRange_benSamps_multistation)
      reactive_objects$benSamps_Filter_UserFilter <- filter(reactive_objects$benSamps_Filter, as.Date(`Collection Date`) >= input$dateRange_benSamps_multistation[1] & as.Date(`Collection Date`) <= input$dateRange_benSamps_multistation[2])
      # WQM_Station_Full missing Ecoregion info in some places so need to spatially join some
      if(nrow(filter(reactive_objects$benSamps_Filter_UserFilter, is.na(EPA_ECO_US_L3CODE))) > 0){
        reactive_objects$benSamps_Filter_fixed <- filter(benSampsStations, StationID %in% filter(reactive_objects$benSamps_Filter_UserFilter, is.na(EPA_ECO_US_L3CODE))$StationID) %>%
          st_intersection(ecoregion) %>% 
          st_drop_geometry() %>%
          left_join(ecoregionCrosswalk, by = "US_L3NAME") %>%
          dplyr::select(StationID, US_L3CODE, US_L3NAME)
      }else{reactive_objects$benSamps_Filter_fixed <- NULL}
      reactive_objects$benSamps_Filter_fin <- reactive_objects$benSamps_Filter_UserFilter %>%
        {if(!is.null(reactive_objects$benSamps_Filter_fixed))
          left_join(., reactive_objects$benSamps_Filter_fixed, by = 'StationID') %>%
            mutate(EPA_ECO_US_L3NAME = ifelse(is.na(EPA_ECO_US_L3NAME), as.character(US_L3NAME),as.character(EPA_ECO_US_L3NAME)),
                   EPA_ECO_US_L3CODE = ifelse(is.na(EPA_ECO_US_L3CODE), as.character(US_L3CODE),as.character(EPA_ECO_US_L3CODE))) %>%
            dplyr::select(-c(US_L3CODE,US_L3NAME)) 
          else .} %>%
        left_join(dplyr::select(reactive_objects$multistationSelection, Sta_Id , Sta_Desc)  %>% distinct(Sta_Id, .keep_all = T), by = c('StationID'= 'Sta_Id')) %>%
        dplyr::select(StationID, Sta_Desc, everything())
      # raw benthics data
      reactive_objects$benthics_Filter <- filter(benthics, BenSampID %in% reactive_objects$benSamps_Filter_fin$BenSampID) %>%
        left_join(dplyr::select(reactive_objects$benSamps_Filter_fin, BenSampID, `Collection Date`)) %>%
        dplyr::select(StationID, `Collection Date`, everything()) %>%
        arrange(StationID, `Collection Date`)
      reactive_objects$benthics_Filter_crosstab <- reactive_objects$benthics_Filter %>%
        group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
        arrange(FinalID) %>%
        pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = FinalID, values_from = Individuals) %>%
        arrange(StationID, `Collection Date`)
      # SCI results
      reactive_objects$SCI_filter <- filter(VSCIresults, BenSampID %in% filter(reactive_objects$benSamps_Filter_fin, ! EPA_ECO_US_L3CODE %in% c(63,65))$BenSampID) %>%
        bind_rows(
          filter(VCPMI63results, BenSampID %in% filter(reactive_objects$benSamps_Filter_fin,  EPA_ECO_US_L3CODE %in% c(63))$BenSampID)  ) %>%
        bind_rows(
          filter(VCPMI65results, BenSampID %in% filter(reactive_objects$benSamps_Filter_fin,  EPA_ECO_US_L3CODE %in% c(65))$BenSampID)  ) %>%
        mutate(SeasonGradient = as.factor(paste0(Season, " (",Gradient,")")),
               SeasonGradientColor = case_when(SeasonGradient == "Spring (Riffle)" ~  "#66C2A5",
                                               SeasonGradient == "Spring (Boatable)" ~  "#66C2A5",
                                               SeasonGradient == "Spring (MACS)" ~  "#66C2A5",
                                               SeasonGradient == "Outside Sample Window (Riffle)" ~ "#FC8D62",
                                               SeasonGradient == "Outside Sample Window (Boatable)" ~ "#FC8D62",
                                               SeasonGradient == "Outside Sample Window (MACS)" ~ "#FC8D62",
                                               SeasonGradient == "Fall (Riffle)" ~ "#8DA0CB",
                                               SeasonGradient == "Fall (Boatable)" ~ "#8DA0CB",
                                               SeasonGradient == "Fall (MACS)" ~ "#8DA0CB",
                                               TRUE ~ as.character(NA)) ) %>%
        left_join(dplyr::select(reactive_objects$multistationSelection, Sta_Id , Sta_Desc) %>% 
                    distinct(Sta_Id, .keep_all = T), 
                  by = c('StationID'= 'Sta_Id')) %>%
        dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, SCI, `SCI Score`, `SCI Threshold`,`Sample Comments`:Season, everything())
      # Basic Sampling metrics by total selection
      reactive_objects$avgSCIselection <- averageSCI_multistationAVG(reactive_objects$benSamps_Filter_fin, reactive_objects$SCI_filter) %>%
        arrange(SCI)
      # Basic Sampling metrics by Station
      reactive_objects$avgSCIstations <- averageSCI_multistation(reactive_objects$benSamps_Filter_fin, reactive_objects$SCI_filter) %>%
        arrange(StationID)
      
      
      # Raw Multistation Habitat Data
      reactive_objects$habSamps_Filter <- filter(habSamps, StationID %in% reactive_objects$benSamps_Filter_fin$StationID) %>%
        filter(as.Date(`Collection Date`) >= input$dateRange_benSamps_multistation[1] & as.Date(`Collection Date`) <= input$dateRange_benSamps_multistation[2]) %>%
        # filter by user decisions
        {if(!is.null(input$multistationGradientFilter))
          filter(., Gradient %in% input$multistationGradientFilter)
          else . } 
      reactive_objects$habObs_Filter <- filter(habObs, HabSampID %in% reactive_objects$habSamps_Filter$HabSampID)
      reactive_objects$habValues_Filter <- filter(habValues, HabSampID %in% reactive_objects$habSamps_Filter$HabSampID)
      
      
      # Total Habitat Multistation metrics
      reactive_objects$habValues_totHab_multistation <- reactive_objects$habSamps_Filter %>%
        group_by(HabSampID) %>%
        # get total habitat values
        left_join(totalHabScore(reactive_objects$habValues_Filter), by = 'HabSampID') %>%
        mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
        dplyr::select(StationID, HabSampID, everything()) %>%
        arrange(`Collection Date`)
      
      reactive_objects$avgTotalHab_multistation <- reactive_objects$habValues_totHab_multistation %>% ungroup() %>%
        summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                  `n Samples` = n()) %>%
        mutate(StationID = 'User Selected Stations',
               Window = 'User Selected Window') %>%
        dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`) %>%
        bind_rows(
          reactive_objects$habValues_totHab_multistation %>% ungroup() %>%
            group_by(Season) %>%
            summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                      `n Samples` = n()) %>%
            mutate(StationID = 'User Selected Stations', 
                   Window = Season) %>%
            dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`)  ) %>%
        bind_rows(
          totalHabScoreAverages(reactive_objects$habValues_totHab_multistation) %>%
            arrange(StationID))
      
    })
    
    # Sampling Metrics 
    output$averageSamplingMetrics_bySelection  <- DT::renderDataTable({
      req(reactive_objects$avgSCIselection)
      datatable(reactive_objects$avgSCIselection, 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '100px',
                               pageLength = nrow(reactive_objects$avgSCIselection),
                               buttons=list('copy','colvis')))  })
    
    output$averageSamplingMetrics_byStation  <- DT::renderDataTable({
      req(reactive_objects$avgSCIstations)
      datatable(reactive_objects$avgSCIstations, 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$avgSCIstations),
                               buttons=list('copy','colvis')))  }) 
    
    output$collectorMetrics_multistation <- renderDataTable({
      req(reactive_objects$benSamps_Filter_fin)
      z <- uniqueCollector(reactive_objects$benSamps_Filter_fin)
      datatable(z, rownames = F, escape= F,extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollY = '150px',
                               pageLength = nrow(z),buttons=list('copy'))) })
    
    output$taxonomistMetrics_multistation <- renderDataTable({
      req(reactive_objects$benSamps_Filter_fin)
      z <- uniqueTaxonomist(reactive_objects$benSamps_Filter_fin)
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollY = '150px',
                               pageLength = nrow(z),buttons=list('copy'))) })
    
    output$multistationBenthicSampleData <- DT::renderDataTable({
      req(reactive_objects$benSamps_Filter_fin)
      datatable(reactive_objects$benSamps_Filter_fin %>% mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(StationID, `Collection Date`, RepNum), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$benSamps_Filter_fin),
                               buttons=list('copy','colvis')))  })
    
    
    # SCI results
    output$multistationSCIresult <- DT::renderDataTable({
      req(reactive_objects$SCI_filter)
      datatable(reactive_objects$SCI_filter %>% mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(StationID, `Collection Date`, RepNum), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$SCI_filter),
                               buttons=list('copy','colvis')))  })
    
    
    output$SCIresultsAdjusted <- DT::renderDataTable({req(input$sciChanger)
      # change SCI results based on user choice
      if(input$sciChanger == 'VSCI'){z <- filter(VSCIresults, BenSampID %in% reactive_objects$benSamps_Filter_fin$BenSampID)}
      if(input$sciChanger == 'VCPMI + 63'){z <- filter(VCPMI63results, BenSampID %in% reactive_objects$benSamps_Filter_fin$BenSampID)}
      if(input$sciChanger == 'VCPMI - 65'){z <- filter(VCPMI65results, BenSampID %in% reactive_objects$benSamps_Filter_fin$BenSampID)}
      z <- dplyr::select(z, StationID:`Collection Date`, BenSampID, everything()) %>%
        arrange(StationID, `Collection Date`, RepNum)
      
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z), buttons=list('copy','colvis')))  })
    
    
    
    # Raw benthics data crosstab and long
    output$rawMultistationBenthicCrosstabGenus <- DT::renderDataTable({req(reactive_objects$benthics_Filter_crosstab)
      datatable(reactive_objects$benthics_Filter_crosstab %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(StationID, `Collection Date`, RepNum), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$benthics_Filter_crosstab),
                               buttons=list('copy','colvis')))  })
    
    output$rawMultistationBenthicGenus <- DT::renderDataTable({req(reactive_objects$benthics_Filter)
      datatable(reactive_objects$benthics_Filter %>%
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(StationID, `Collection Date`, RepNum), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$benthics_Filter),
                               buttons=list('copy','colvis')))  })
    
    output$rawMultistationBenthicCrosstabFamily <- DT::renderDataTable({req(reactive_objects$benthics_Filter_crosstab)
      z <- reactive_objects$benthics_Filter %>%
        left_join(dplyr::select(masterTaxaGenus, `Final VA Family ID`,`FinalID`), by = 'FinalID') %>% 
        group_by(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`) %>% 
        mutate(Individuals = sum(Individuals, na.rm = T)) %>% 
        distinct(`Final VA Family ID`, .keep_all = T) %>% 
        dplyr::select(-FinalID) %>% 
        ungroup() %>% 
        group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
        arrange(`Final VA Family ID`) %>%
        pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = `Final VA Family ID`, values_from = Individuals) %>%
        mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
        arrange(StationID, `Collection Date`, RepNum)
      
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),
                               buttons=list('copy','colvis')))  })
    
    output$rawMultistationBenthicFamily <- DT::renderDataTable({req(reactive_objects$benthics_Filter)
      z <- filter(reactive_objects$benthics_Filter, BenSampID %in% reactive_objects$benSamps_Filter_fin$BenSampID) %>%
        left_join(dplyr::select(masterTaxaGenus, `Final VA Family ID`,`FinalID`), by = 'FinalID') %>% 
        group_by(StationID, BenSampID, RepNum, `Final VA Family ID`) %>% 
        mutate(Individuals = sum(Individuals, na.rm = T)) %>% 
        distinct(`Final VA Family ID`, .keep_all = T) %>% 
        left_join(dplyr::select(reactive_objects$benSamps_Filter_fin, BenSampID, `Collection Date`)) %>%
        dplyr::select(StationID, `Collection Date`, BenSampID, RepNum, `Final VA Family ID`, 
                      Individuals, Taxonomist, `Entered By`, `Entered Date`) %>%
        mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
        arrange(StationID, `Collection Date`, RepNum)
      
      
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),
                               buttons=list('copy','colvis')))  })

    
    # SCI Seasonal Crosstab Table
    
    output$metric_UI_multistation <- renderUI({
      req(reactive_objects$SCI_filter)
      selectInput('metric_multistation', 'Select Metric to Analyze', 
                  choices = names(dplyr::select(reactive_objects$SCI_filter, `SCI Score`, `Family Total Taxa`:`%Scrap`))) })
    
    output$SCIseasonalCrosstab_multistation <- DT::renderDataTable({
      req(reactive_objects$benthics_Filter, reactive_objects$SCI_filter, input$metric_multistation)
      z <- SCI_crosstab_Billy(crosstabTemplate, reactive_objects$SCI_filter, WQM_Station_Full, input$metric_multistation) %>%#`%Ephem`)#`SCI Score`)
        arrange(StationID, RepNum)
      
      datatable(z, 
        #dplyr::select(z, StationID:`Collection Date`) %>% dplyr::select(-c(`Collection Date`)), # crazy way to include unexpected columns
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),
                               buttons=list('copy','colvis')))  })
    
    # Benthic Individuals BenSamp Crosstab
    
    output$benthicIndividualsBensampCrosstab_multistation <- DT::renderDataTable({
      req(reactive_objects$benthics_Filter, reactive_objects$SCI_filter, input$genusOrFamily_multistation)
      z <- benthics_crosstab_Billy(reactive_objects$benthics_Filter, masterTaxaGenus, genusOrFamily = input$genusOrFamily_multistation)
      
      datatable(z, #dplyr::select(z, StationID:`Collection Date`) %>% dplyr::select(-c(`Collection Date`)), # crazy way to include unexpected columns
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(z),
                               buttons=list('copy','colvis')))  })
    
    
    ## BCG Attributes Multistation
    output$multiBCGattributes <-  DT::renderDataTable({ req(reactive_objects$benthics_Filter)
      multiBCGtable <- reactive_objects$benthics_Filter%>%
        dplyr::select(StationID:Individuals) %>%
        mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
        arrange(StationID, `Collection Date`, RepNum) %>%
        left_join(dplyr::select(BCGattVal, FinalID, `Taxonomic Notes`: `BCGatt Comment`), by = 'FinalID')
      
      datatable(multiBCGtable,  rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                               pageLength=nrow(multiBCGtable), buttons=list('copy','colvis'))) %>% 
        formatStyle(names(multiBCGtable)[c(8:21, 23:29)], backgroundColor = styleInterval(bcgAttributeColors $brks, bcgAttributeColors$clrs)) })
    
  
    
    
    
    ## Habitat Sampling Metrics Multistation Tab
    
    output$averageTotalHabitatMetrics_multistation <-  renderDataTable({
      req(reactive_objects$avgTotalHab_multistation)
      datatable(reactive_objects$avgTotalHab_multistation, rownames = F, escape= F,  extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollY = '250px', pageLength = nrow(reactive_objects$avgTotalHab_multistation), buttons=list('copy','colvis'))) })
    
    output$fieldTeamMetrics_multistation <- renderDataTable({
      req(reactive_objects$habSamps_Filter)
      z <- uniqueFieldTeam(reactive_objects$habSamps_Filter)
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bft', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })
    
    output$habObsMetrics_multistation <- renderDataTable({
      req(reactive_objects$habObs_Filter)
      z <- habObsMetrics(reactive_objects$habObs_Filter) %>% rename('n Observations' = 'Observations') # make formatting match field team to line up tables
      datatable(z, rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bft', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })
    
    output$habitatSamplingInformation_multistation <- renderDataTable({
      req(reactive_objects$habSamps_Filter)
      datatable(dplyr::select(reactive_objects$habSamps_Filter, StationID, HabSampID, everything()) %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`),
                         `Entered Date` = as.Date(`Entered Date`)) %>%
                  arrange(StationID), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX = TRUE, scrollY = '150px',
                               pageLength = nrow(reactive_objects$habSamps_Filter), buttons=list('copy','colvis'))) })
    
    ## Detailed Habitat Results Tab
    
    output$habitatResultsTable_multistation <- DT::renderDataTable({
      req(reactive_objects$habValues_totHab_multistation)
      datatable(reactive_objects$habValues_totHab_multistation %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`),
                         `Entered Date` = as.Date(`Entered Date`)) %>%
                  arrange(StationID, `Collection Date`),
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$habValues_totHab_multistation), buttons=list('copy','colvis')))  })
    
    
    
    ## Raw Habitat Multistation Results Tab
    
    habitatValuesCrosstab_multistation <- reactive({
      req(reactive_objects$habValues_Filter)
      left_join(reactive_objects$habValues_Filter, 
                dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
                by = 'HabSampID') %>%
        group_by(StationID, HabSampID, `Collection Date`) %>%
        arrange(HabParameterDescription) %>% ungroup() %>%
        pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
        left_join(dplyr::select(reactive_objects$habValues_totHab_multistation, HabSampID, `Total Habitat Score`), by = 'HabSampID') %>%
        arrange(StationID, `Collection Date`)    })
    
    output$habitatValuesCrosstab_multistation <- DT::renderDataTable({
      req(habitatValuesCrosstab_multistation())
      datatable(habitatValuesCrosstab_multistation() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) , 
                rownames = F, escape= F, extensions = 'Buttons',  selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habitatValuesCrosstab_multistation()), buttons=list('copy','colvis')))  })
    
    habitatObservationsCrossTab_multistation <- reactive({
      req(reactive_objects$habObs_Filter)
      left_join(reactive_objects$habObs_Filter,
                dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
                by = 'HabSampID') %>%
        group_by(HabSampID) %>%
        arrange(ObsParameterDescription) %>%
        pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = ObsParameterDescription, values_from = ObsValue) %>%
        arrange(StationID, `Collection Date`)    })
    
    output$habitatObservationsCrossTab_multistation <- DT::renderDataTable({
      req(habitatObservationsCrossTab_multistation())
      datatable(habitatObservationsCrossTab_multistation() %>% 
                  mutate(`Collection Date` = as.Date(`Collection Date`)) , 
                rownames = F, escape= F, extensions = 'Buttons',  selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(habitatObservationsCrossTab_multistation()), buttons=list('copy','colvis')))  })
    
    
    
    
    output$habitatValues_multistation <- DT::renderDataTable({
      req(reactive_objects$habValues_Filter)
      datatable(left_join(reactive_objects$habValues_Filter, 
                          dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
                          by = 'HabSampID') %>%
                  dplyr::select(StationID, HabSampID, `Collection Date`, everything()) %>%
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(StationID, `Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons',  selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$habValues_Filter), buttons=list('copy','colvis')))  })
    
    output$habitatObservations_multistation <- DT::renderDataTable({
      req(reactive_objects$habObs_Filter)
      datatable(left_join(reactive_objects$habObs_Filter, 
                          dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
                          by = 'HabSampID') %>%
                  dplyr::select(StationID, HabSampID, `Collection Date`, everything()) %>%
                  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
                  arrange(StationID, `Collection Date`), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                               pageLength = nrow(reactive_objects$habObs_Filter), buttons=list('copy','colvis') )) })
    
    
    
    
    
    
    
    
    
    ### -------------------------------------How To----------------------------------------------------------------------------------------------------------------------
    
    #output$BenthicQueryToolHowTo <- renderUI({includeHTML("BenthicQueryToolHowTo.html")})
    
    
 })