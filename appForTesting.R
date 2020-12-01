source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')

WQM_STATIONS_FINAL <- st_read('data/GIS/WQM_STATIONS_FINAL.shp') %>%
  st_transform(4326)
names(WQM_STATIONS_FINAL) <- c("STATION_ID", "WQM_INSERTED_BY", "WQM_INSERTED_DATE", "WQM_CHANGED_BY", 
                               "WQM_CHANGED_DATE", "WQM_VERIFIEDBY", "WQM_VERIFYDATE", "WQM_REFERENCE_POINT", 
                               "WQM_STA_ID", "WQM_YRS_SPG_CODE", "WQM_YRS_YEAR", "WQM_SPG_DESCRIPTION", 
                               "WQM_CUR_YEAR", "WQM_STA_STREAM_NAME", "WQM_STA_DESC", "WQM_STA_HUC_CODE",
                               "WQM_STA_WAT_SHED_CODE", "WQM_STA_REC_CODE", "WQM_STA_STRAHER_ORDER", 
                               "WQM_STA_SHREVE_ORDER", "WQM_STA_WADABLE", "WQM_STA_LV1_CODE", "WQM_STA_LV2_CODE", 
                               "WQM_STA_LV3_CODE", "WQM_COL_17", "WQM_STA_COMMENT", "EPA_ECO_US_L4CODE", 
                               "EPA_ECO_US_L4NAME", "EPA_ECO_US_L3CODE", "EPA_ECO_US_L3NAME", "EPA_ECO_NA_L3CODE", 
                               "BASINS_VAHU6", "BASINS_STATES", "BASINS_HUMOD", "BASINS_TOHUC", "BASINS_META_ID", 
                               "BASINS_LOCATION", "BASINS_PC_WATER", "BASINS_TIDAL", "BASINS_VAHUSB", "BASINS_HUC10",
                               "BASINS_HUC_12", "BASINS_HU_12_NAME", "BASINS_NAME", "BASINS_HUC_10", "BASINS_HUC_8", 
                               "BASINS_VAHU5", "BASINS_HUC_8_NAME", "WQS_ID", "WQS_BUFFER_DISTANCE", "WQS_GNIS_NAME",
                               "WQS_BASIN", "WQS_COMMENT", "WQS_WATER_NAME", "WQS_SEC", "WQS_CLASS", "WQS_SPSTDS", 
                               "WQS_SECTION_DESCRIPTION", "WQS_BASIN_CODE", "WQS_PWS", "WQS_TROUT", "WQS_EDIT_DATE", 
                               "WQS_STREAMTYPE", "WQS_TIER_III", "WQS_BACKLOG", "WQS_CREATED_USER", "WQS_CREATED_DATE",
                               "WQS_LAST_EDITED_USER", "WQS_LAST_EDITED_DATE", "WQS_COMMEN", "WQS_LAKES_187B", "geometry")         



ui <- source('ui.R')


server <- shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
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
  
  
  ## Pull Station Information ( will eventually be ODS query)
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
                                                 dplyr::select(WQM_STATIONS_FINAL, STATION_ID, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                                               EPA_ECO_US_L3NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                                               WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III),
                                                 by = c('Sta_Id' = 'STATION_ID')) %>%
      dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                    EPA_ECO_US_L3NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                    WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())
    
    ## Station Geospatial Information
    reactive_objects$stationInfo_sf <- filter(WQM_STATIONS_FINAL, STATION_ID %in% toupper(input$station) )
    
    ## Station Sampling Information
    reactive_objects$stationInfoSampleMetrics <- reactive_objects$stationInfo_sf %>%
      group_by(STATION_ID) %>%
      mutate(`Years Sampled` = paste0(year(WQM_YRS_YEAR))) %>%
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
      dplyr::select(StationID, BenSampID, RepNum, `Collection Date`, `Sample Comments`, `Collected By`, `Field Team`, `Entered By`,
                    Taxonomist, `Entered Date`, Gradient, `Target Count`, Season)
    
    
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
                             buttons=list('copy')))  })
  
  output$stationInfoSampleCodeMetrics <- DT::renderDataTable({
    req(reactive_objects$stationInfoFin)
    datatable(reactive_objects$stationInfoSampleMetrics, 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$stationInfoSampleMetrics),
                             buttons=list('copy')) ) })
  
  
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
                   end = max(reactive_objects$stationInfoBenSamps$`Collection Date`))  })
  
  ### Filter by user input
  observe({
    req(reactive_objects$stationInfoBenSamps, input$dateRange)
    reactive_objects$stationInfoBenSampsDateRange <- filter(reactive_objects$stationInfoBenSamps, `Collection Date` >= input$dateRange[1] & `Collection Date` <= input$dateRange[2]) %>%
      {if(input$rarifiedFilter)
        filter(., grepl( 'R110', BenSampID))
        else . } %>%
      {if(!is.null(input$repFilter))
        filter(., RepNum %in% input$repFilter)
        else . } })
  
  stationBenthicsDateRange <- reactive({ 
    req(reactive_objects$stationInfoBenSampsDateRange)
    filter(reactive_objects$stationBenthics, BenSampID %in% reactive_objects$stationInfoBenSampsDateRange$BenSampID) %>%
      left_join(dplyr::select(reactive_objects$stationInfoBenSampsDateRange, BenSampID, `Collection Date`)) %>%
      dplyr::select(`Collection Date`, everything()) })
  
  SCIresults <- reactive({
    req( stationBenthicsDateRange())
    SCIresults <- SCI(stationBenthicsDateRange(), input$SCIchoice, reactive_objects$stationInfoBenSampsDateRange,  reactive_objects$masterTaxaGenus, reactive_objects$vmast) %>%
      mutate_if(is.numeric, round, digits=2) # rounds all numeric columns to 2 decimal places
    SCIresults$Season <-  factor(SCIresults$Season,levels=c("Spring","Outside Sample Window","Fall"))#,ordered=T)
    return(SCIresults)})
  
  observe({
    req(reactive_objects$stationInfoBenSampsDateRange, SCIresults())
    reactive_objects$SCIresults <- SCIresults()  # for report, creates endless loop if not in separate reactive
    reactive_objects$avgSCI <- averageSCI(reactive_objects$stationInfoBenSampsDateRange, SCIresults()) 
    reactive_objects$sciTable <- dplyr::select(SCIresults(), StationID, `Collection Date`, `Target Count`, Season, BenSampID:`SCI Score`) }) # inclusive of different column types 
  
  
  
  ## Sampling Metrics Tab
  output$averageSamplingMetrics <- renderDataTable({
    req(reactive_objects$avgSCI)
    datatable(reactive_objects$avgSCI, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollY = '250px', pageLength = nrow(reactive_objects$avgSCI),buttons=list('copy'))) })
  
  output$collectorMetrics <- renderDataTable({
    req(reactive_objects$stationInfoBenSampsDateRange,stationBenthicsDateRange())
    z <- uniqueCollector(reactive_objects$stationInfoBenSampsDateRange)
    datatable(z, rownames = F, escape= F,extensions = 'Buttons',
              options = list(dom = 'Bt', scrollY = '75px',
                             pageLength = nrow(z),buttons=list('copy'))) })
  
  output$taxonomistMetrics <- renderDataTable({
    req(reactive_objects$stationInfoBenSampsDateRange,stationBenthicsDateRange())
    z <- uniqueTaxonomist(reactive_objects$stationInfoBenSampsDateRange)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollY = '75px',
                             pageLength = nrow(z),buttons=list('copy'))) })
  
  output$samplingInformation <- renderDataTable({
    req(reactive_objects$stationInfoBenSampsDateRange,stationBenthicsDateRange())
    datatable(reactive_objects$stationInfoBenSampsDateRange  %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX = TRUE, scrollY = '350px',
                             pageLength = nrow(reactive_objects$stationInfoBenSampsDateRange), buttons=list('copy')))})
  
  
  ## SCI Results Tab
  
  ### SCI plotly
  output$SCIplot <- renderPlotly({
    req(SCIresults())
    if(unique(SCIresults()$SCI) == 'VSCI'){sciLimit <- 60} else {sciLimit <- 42}
    
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
    
    
    
    
    plot_ly(SCIresults %>% filter(grepl( 'R110', BenSampID)), 
            x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
            #plot_ly(SCIresults(), x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
            color = ~SeasonGradient, width = 0.5, marker = list(color = ~SeasonGradientColor),
            
            #color = ~Season, width = 0.5,
            #marker = list(line = list(width = 1.5)),
            hoverinfo="text", text=~paste(sep="<br>",
                                          paste("StationID: ", StationID),
                                          paste("Collection Date: ", as.Date(`Collection Date`)),
                                          paste("Collector ID: ",`Collected By`),
                                          paste("BenSampID: ", BenSampID),
                                          paste("SCI Score: ", format(`SCI Score`, digits=2)),
                                          paste("Gradient: ", Gradient))) %>%
      layout(#showlegend=FALSE,
        shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
        yaxis=list(title="SCI"),
        xaxis=list(title="Sample Date",tickfont = list(size = 10),
                   type = 'date',tickformat = "%Y"))
    
  })
  
  
  
  ### SCI table
  output$SCIresultsTable <- DT::renderDataTable({
    req(reactive_objects$sciTable)
    datatable(reactive_objects$sciTable  %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX= TRUE, scrollY = '250px', 
                             pageLength= nrow(reactive_objects$sciTable), buttons=list('copy')))  })
  
  
  ### Raw Benthic Tab
  output$rawBenthic <- DT::renderDataTable({
    req(stationBenthicsDateRange())
    datatable(stationBenthicsDateRange() %>% mutate(`Collection Date` = as.Date(`Collection Date`),
                                                    `Entered Date` = as.Date(`Entered Date`)), 
              rownames = F, escape= F,  extensions = 'Buttons',
              options = list(dom = 'Bft', scrollX= TRUE, scrollY = '500px',
                             pageLength=nrow(stationBenthicsDateRange()), buttons=list('copy')))  })
  
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
      filter(reactive_objects$habSampleStation, `Collection Date` >= input$dateRange[1] & `Collection Date` <= input$dateRange[2])  
    } else {
      filter(reactive_objects$habSampleStation, `Collection Date` >= input$habitatDateRange[1] & `Collection Date` <= input$habitatDateRange[2])  }
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
      mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall")))    })

  avgTotalHab <- reactive({
    req(habValues_totHab())
    totalHabScoreAverages(habValues_totHab())  })
  
  ## Habitat Sampling Metrics Tab
  output$averageTotalHabitatMetrics <-  renderDataTable({
    req(avgTotalHab())
    datatable(avgTotalHab(), rownames = F, escape= F,  extensions = 'Buttons',
              options = list(dom = 'Bt', scrollY = '250px', pageLength = nrow(avgTotalHab()), buttons=list('copy'))) })
  
  output$fieldTeamMetrics <- renderDataTable({
    req(habitatSampleDateRange())
    z <- uniqueFieldTeam(habitatSampleDateRange())
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })
  
  output$habObsMetrics <- renderDataTable({
    req(habObsStationDateRange())
    z <- habObsMetrics(habObsStationDateRange())
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })
  
  output$habitatSamplingInformation <- renderDataTable({
    req(habitatSampleDateRange())
    datatable(habitatSampleDateRange() %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bt', scrollX = TRUE, scrollY = '150px',
                             pageLength = nrow(habitatSampleDateRange()), buttons=list('copy'))) })
  
  ## Detailed Habitat Results Tab
  
  output$totalHabitatPlot <- renderPlotly({
    req(habValues_totHab())
    habValues_totHab() %>%
      plot_ly( x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', 
               color = ~Season, width = 0.5,
               #marker = list(line = list(width = 1.5)),
               hoverinfo="text", text=~paste(sep="<br>",
                                             paste("StationID: ", StationID),
                                             paste("Collection Date: ", as.Date(`Collection Date`)),
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
    datatable(habValues_totHab() %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(reactive_objects$habValues_totHab), buttons=list('copy')))  })
  
  
  
  ## Raw Habitat Results Tab
  
  output$habitatValues <- DT::renderDataTable({
    req(habValuesStationDateRange())
    datatable(habValuesStationDateRange() %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
              rownames = F, escape= F, extensions = 'Buttons', 
              options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(habValuesStationDateRange()), buttons=list('copy')))  })
  
  output$habitatObservations <- DT::renderDataTable({
    req(habObsStationDateRange())
    datatable(habObsStationDateRange() %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
              rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                             pageLength = nrow(habObsStationDateRange), buttons=list('copy') )) })
  
  #output$test <- renderPrint({habValuesStationDateRange()})
  
  
  
  
})

pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server",  # note the space between SQL and Server ( how MS named driver)
  Server= "WSQ04151,50000",
  dbname = "ODS_test"
)
onStop(function() {
  poolClose(pool)
})


shinyApp(ui, server)




