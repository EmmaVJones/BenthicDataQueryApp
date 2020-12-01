# Retrieve Pins
WQM_Station_Full <- pin_get("ejones/WQM-Station-Full", board = "rsconnect")
Wqm_Stations_View <- pin_get("ejones/WQM-Stations-View", board = "rsconnect")

VSCIresults <- pin_get("ejones/VSCIresults", board = "rsconnect")
VCPMI63results <- pin_get("ejones/VCPMI63results", board = "rsconnect")
VCPMI65results <- pin_get("ejones/VCPMI65results", board = "rsconnect")
WQM_Stations <- pin_get("ejones/WQM-Sta-GIS-View", board = "rsconnect")
benthics <- pin_get("ejones/benthics", board = "rsconnect")
benSamps <- pin_get("ejones/benSamps", board = "rsconnect")
habSamps <- pin_get("ejones/habSamps", board = "rsconnect")
habValues <- pin_get("ejones/habValues", board = "rsconnect")
habObs <- pin_get("ejones/habObs", board = "rsconnect")
masterTaxaGenus <- pin_get("ejones/masterTaxaGenus", board = "rsconnect")

benSampsStations <- benSamps %>%
  group_by(StationID) %>%
  mutate(`Total Station Visits (Not Sample Reps)` = n(),
         selectedLocationID = paste0(StationID, '_selectedLayer')) %>%
  ungroup() %>%
  distinct(StationID, .keep_all = T) %>%
  left_join(dplyr::select(WQM_Stations, Station_Id, Latitude, Longitude), by = c('StationID' = 'Station_Id')) %>%
  dplyr::select(StationID, `Total Station Visits (Not Sample Reps)`, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) 


### USE FROM OTHER SIDE OF APP
vmast <- masterTaxaGenus %>%
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

# make ecoregionCrosswalk
#eco <- st_read('C:/HardDriveBackup/GIS/VA_level3ecoregion.shp')
#ecoregionCrosswalk <- eco %>%
#  st_drop_geometry() %>%
#  group_by(US_L3CODE) %>%
#  summarize(US_L3NAME) %>%
#  distinct() %>%
#  filter(US_L3NAME %in% ecoregion$US_L3NAME)
#saveRDS(ecoregionCrosswalk, 'data/ecoregionCrosswalk.RDS')
ecoregionCrosswalk <- readRDS('data/ecoregionCrosswalk.RDS')










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
    textInput('wildcardText', 'Filter by StationID LIKE', placeholder = '2A%') )     })

observeEvent(input$begin_multistation,{
  if(input$wildcardText != ""){
    reactive_objects$wildcardResults <- sqldf(paste0('SELECT * FROM benSamps WHERE StationID like "',
                                                     input$wildcardText, '"')) # need to use benSamps bc sqldf doesn't like sf objects
    if(nrow(reactive_objects$wildcardResults) > 0 ){
      reactive_objects$WQM_Stations_Filter <- filter(benSampsStations, StationID %in% reactive_objects$wildcardResults$StationID) 
    } else{
      # if(input$wildcardText != "" & is.null(input$VAHU6Filter) & is.null(input$subbasinFilter) & 
      #    is.null(input$assessmentRegionFilter) & is.null(input$ecoregionFilter)){
      showNotification("No stations found with entered text.")        } 
  }
  #    else {reactive_objects$wildcardResults <- NULL 
  #    showNotification("No stations found with entered text.")}
  
  
  if(is.null(reactive_objects$wildcardResults)){
    reactive_objects$WQM_Stations_Filter <- benSampsStations %>%
      # go small to large spatial filters
      {if(!is.null(input$VAHU6Filter))
        st_intersection(., filter(assessmentLayer, VAHU6 %in% input$VAHU6Filter))
        else .} %>%
      {if(is.null(input$VAHU6Filter) & !is.null(input$subbasinFilter))
        st_intersection(., filter(subbasins, SUBBASIN %in% input$subbasinFilter))
        else .} %>%
      {if(is.null(input$VAHU6Filter) & !is.null(input$assessmentRegionFilter)) # don't need assessment region filter if VAHU6 available
        st_intersection(., filter(assessmentRegions, ASSESS_REG %in% input$assessmentRegionFilter))
        else .} %>%
      {if(!is.null(input$ecoregionFilter))
        st_intersection(., filter(ecoregion, US_L3NAME %in% input$ecoregionFilter))
        else .}  %>%
      {if(!is.null(input$dateRange_multistation))
        filter(., StationID %in% filter(benSamps, `Collection Date` >= input$dateRange_multistation[1] & `Collection Date` <= input$dateRange_multistation[2])$StationID)
        else .} %>%
      rename(., `Total Station Visits (Not Sample Reps)` = "Total.Station.Visits..Not.Sample.Reps.") %>%
      dplyr::select(StationID, `Total Station Visits (Not Sample Reps)`) 
  }
  
  # list to store the selections for tracking
  clickedMarker = list()  })


observe({
  req(reactive_objects$WQM_Stations_Filter)
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
                                                                  STATION_ID, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                                                  EPA_ECO_US_L3NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                                                  WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                                    by = c('Sta_Id' = 'STATION_ID')) %>%
    dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                  EPA_ECO_US_L3NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                  WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())  
  
  # Empty station user selection to start with
  reactive_objects$selectedSites <- NULL
  
})



output$multistationMap <- renderLeaflet({
  req(reactive_objects$WQM_Stations_Filter)
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

map_proxy <- leafletProxy("multistationMap")



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
  map_proxy %>%
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
  map_proxy %>%
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
  
  map_proxy %>%
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
  datatable(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$multistationSelection %>% distinct(Sta_Id, .keep_all = T)),
                           buttons=list('copy','colvis')))  })

output$multistationInfoSampleMetrics <- DT::renderDataTable({
  req(reactive_objects$multistationInfoSampleMetrics)
  datatable(reactive_objects$multistationInfoSampleMetrics, 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$multistationInfoSampleMetrics),
                           buttons=list('copy','colvis')) ) })


# Multistation Benthic Results Tab

# Compile benthic data based on stations selected
observe({
  req(reactive_objects$multistationSelection)
  reactive_objects$benSamps_Filter <- filter(benSamps, StationID %in% reactive_objects$multistationSelection$Sta_Id) %>%
    filter(`Collection Date` >= input$dateRange_multistation[1] & `Collection Date` <= input$dateRange_multistation[2]) %>%
    # bring in ecoregion info
    left_join(dplyr::select(WQM_Station_Full, WQM_STA_ID, EPA_ECO_US_L3NAME, EPA_ECO_US_L3CODE) %>%
                distinct(WQM_STA_ID, .keep_all = T),
              by = c("StationID" = "WQM_STA_ID")) %>%
    # filter by user decisions
    {if(input$multistationRarifiedFilter)
      filter(., grepl( 'R110', BenSampID))
      else . } %>%
    {if(!is.null(input$multistationRepFilter))
      filter(., RepNum %in% input$multistationRepFilter)
      else . } 
})


output$dateRange_benSamps_multistationUI <- renderUI({
  req(reactive_objects$benSamps_Filter)
  dateRangeInput('dateRange_benSamps_multistation',
                 label = 'Filter Benthic Data By Date Range (YYYY-MM-DD)',
                 start = as.Date(min(reactive_objects$benSamps_Filter$`Collection Date`)), 
                 end = as.Date(max(reactive_objects$benSamps_Filter$`Collection Date`)))
})

observe({
  req(input$dateRange_benSamps_multistation)
  reactive_objects$benSamps_Filter_UserFilter <- filter(reactive_objects$benSamps_Filter, `Collection Date` >= input$dateRange_benSamps_multistation[1] & `Collection Date` <= input$dateRange_benSamps_multistation[2])
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
      else .}
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
    dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, SCI, `SCI Score`, `SCI Threshold`,`Sample Comments`:Season, everything())
  # Basic Sampling metrics by total selection
  reactive_objects$avgSCIselection <- averageSCI_multistationAVG(reactive_objects$benSamps_Filter_fin, reactive_objects$SCI_filter) %>%
    arrange(SCI)
  # Basic Sampling metrics by Station
  reactive_objects$avgSCIstations <- averageSCI_multistation(reactive_objects$benSamps_Filter_fin, reactive_objects$SCI_filter) %>%
    arrange(StationID)
  
  
  # Raw Multistation Habitat Data
  reactive_objects$habSamps_Filter <- filter(habSamps, StationID %in% reactive_objects$WQM_Stations_Filter$StationID) %>%
    filter(`Collection Date` >= input$dateRange_multistation[1] & `Collection Date` <= input$dateRange_multistation[2])
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
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '100px',
                           pageLength = nrow(reactive_objects$avgSCIselection),
                           buttons=list('copy','colvis')))  })

output$averageSamplingMetrics_byStation  <- DT::renderDataTable({
  req(reactive_objects$avgSCIstations)
  datatable(reactive_objects$avgSCIstations, 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$avgSCIstations),
                           buttons=list('copy','colvis')))  }) 

output$collectorMetrics_multistation <- renderDataTable({
  req(reactive_objects$benSamps_Filter_fin)
  z <- uniqueCollector(reactive_objects$benSamps_Filter_fin)
  datatable(z, rownames = F, escape= F,extensions = 'Buttons',
            options = list(dom = 'Bt', scrollY = '150px',
                           pageLength = nrow(z),buttons=list('copy'))) })

output$taxonomistMetrics_multistation <- renderDataTable({
  req(reactive_objects$benSamps_Filter_fin)
  z <- uniqueTaxonomist(reactive_objects$benSamps_Filter_fin)
  datatable(z, rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bt', scrollY = '150px',
                           pageLength = nrow(z),buttons=list('copy'))) })

output$multistationBenthicSampleData <- DT::renderDataTable({
  req(reactive_objects$benSamps_Filter_fin)
  datatable(reactive_objects$benSamps_Filter_fin %>% mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
              arrange(StationID), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$benSamps_Filter_fin),
                           buttons=list('copy','colvis')))  })


# SCI results
output$multistationSCIresult <- DT::renderDataTable({
  req(reactive_objects$SCI_filter)
  datatable(reactive_objects$SCI_filter %>% mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
              arrange(StationID), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$SCI_filter),
                           buttons=list('copy','colvis')))  })


# Raw benthics data crosstab and long
output$rawMultistationBenthicCrosstab <- DT::renderDataTable({
  req(reactive_objects$benthics_Filter_crosstab)
  datatable(reactive_objects$benthics_Filter_crosstab %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$benthics_Filter_crosstab),
                           buttons=list('copy','colvis')))  })

output$rawMultistationBenthic <- DT::renderDataTable({
  req(reactive_objects$benthics_Filter)
  datatable(reactive_objects$benthics_Filter %>% mutate(`Collection Date` = as.Date(`Collection Date`)), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bift', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$benthics_Filter),
                           buttons=list('copy','colvis')))  })


## Habitat Sampling Metrics Multistation Tab

output$averageTotalHabitatMetrics_multistation <-  renderDataTable({
  req(reactive_objects$avgTotalHab_multistation)
  datatable(reactive_objects$avgTotalHab_multistation, rownames = F, escape= F,  extensions = 'Buttons',
            options = list(dom = 'Bft', scrollY = '250px', pageLength = nrow(reactive_objects$avgTotalHab_multistation), buttons=list('copy','colvis'))) })

output$fieldTeamMetrics_multistation <- renderDataTable({
  req(reactive_objects$habSamps_Filter)
  z <- uniqueFieldTeam(reactive_objects$habSamps_Filter)
  datatable(z, rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bft', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })

output$habObsMetrics_multistation <- renderDataTable({
  req(reactive_objects$habObs_Filter)
  z <- habObsMetrics(reactive_objects$habObs_Filter) %>% rename('n Observations' = 'Observations') # make formatting match field team to line up tables
  datatable(z, rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bft', scrollY = '150px', pageLength = nrow(z), buttons=list('copy'))) })

output$habitatSamplingInformation_multistation <- renderDataTable({
  req(reactive_objects$habSamps_Filter)
  datatable(dplyr::select(reactive_objects$habSamps_Filter, StationID, HabSampID, everything()) %>% 
              mutate(`Collection Date` = as.Date(`Collection Date`),
                     `Entered Date` = as.Date(`Entered Date`)) %>%
              arrange(StationID), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bft', scrollX = TRUE, scrollY = '150px',
                           pageLength = nrow(reactive_objects$habSamps_Filter), buttons=list('copy','colvis'))) })

## Detailed Habitat Results Tab

output$habitatResultsTable_multistation <- DT::renderDataTable({
  req(reactive_objects$habValues_totHab_multistation)
  datatable(reactive_objects$habValues_totHab_multistation %>% 
              mutate(`Collection Date` = as.Date(`Collection Date`),
                     `Entered Date` = as.Date(`Entered Date`)) %>%
              arrange(StationID, `Collection Date`),
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
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
    arrange(StationID)    })

output$habitatValuesCrosstab_multistation <- DT::renderDataTable({
  req(habitatValuesCrosstab_multistation())
  datatable(habitatValuesCrosstab_multistation() %>% 
              mutate(`Collection Date` = as.Date(`Collection Date`)) , 
            rownames = F, escape= F, extensions = 'Buttons', 
            options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(habitatValuesCrosstab_multistation()), buttons=list('copy','colvis')))  })

habitatObservationsCrossTab_multistation <- reactive({
  req(reactive_objects$habObs_Filter)
  left_join(reactive_objects$habObs_Filter,
            dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
            by = 'HabSampID') %>%
    group_by(HabSampID) %>%
    arrange(ObsParameterDescription) %>%
    pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = ObsParameterDescription, values_from = ObsValue) %>%
    arrange(StationID)    })

output$habitatObservationsCrossTab_multistation <- DT::renderDataTable({
  req(habitatObservationsCrossTab_multistation())
  datatable(habitatObservationsCrossTab_multistation() %>% 
              mutate(`Collection Date` = as.Date(`Collection Date`)) , 
            rownames = F, escape= F, extensions = 'Buttons', 
            options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(habitatObservationsCrossTab_multistation()), buttons=list('copy','colvis')))  })




output$habitatValues_multistation <- DT::renderDataTable({
  req(reactive_objects$habValues_Filter)
  datatable(left_join(reactive_objects$habValues_Filter, 
                      dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
                      by = 'HabSampID') %>%
              dplyr::select(StationID, HabSampID, `Collection Date`, everything()) %>%
              mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
              arrange(StationID), 
            rownames = F, escape= F, extensions = 'Buttons', 
            options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$habValues_Filter), buttons=list('copy','colvis')))  })

output$habitatObservations_multistation <- DT::renderDataTable({
  req(reactive_objects$habObs_Filter)
  datatable(left_join(reactive_objects$habObs_Filter, 
                      dplyr::select(reactive_objects$habSamps_Filter, HabSampID, StationID, `Collection Date`),
                      by = 'HabSampID') %>%
              dplyr::select(StationID, HabSampID, `Collection Date`, everything()) %>%
              mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
              arrange(StationID), 
            rownames = F, escape= F, extensions = 'Buttons',
            options = list(dom = 'Bft', scrollX= TRUE, scrollY = '300px',
                           pageLength = nrow(reactive_objects$habObs_Filter), buttons=list('copy','colvis') )) })





output$test <- renderPrint({
  class(WQM_Station_Full) })
#reactive_objects$WQM_Stations_Filter})

