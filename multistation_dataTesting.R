#source("global_multi.R")
#source("global.R")

## SQL FYI using library(sqldf)
#z <- sqldf('SELECT * FROM benSamps WHERE StationID like "2-JKS0%"')

#z <- '2-JKS02%'
#sqldf(paste0('SELECT * FROM benSamps WHERE StationID like "',
#      z,
#      '"'))

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')
assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('data/GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1') %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), as.character(BASIN_NAME), as.character(SUBBASIN)))
subbasinVAHU6crosswalk <- read_csv('data/basinAssessmentReg_clb_EVJ.csv') %>%
  filter(!is.na(SubbasinVAHU6code)) %>%
  mutate(SUBBASIN = ifelse(is.na(SUBBASIN), BASIN_NAME, SUBBASIN)) %>%
  dplyr::select(SUBBASIN, SubbasinVAHU6code)


# user inputs
queryType <- 'Manually Specify Stations'#'Spatial Filters' #Interactive Selection #'Manually Specify Stations'


# manually specify troubleshooting
manualSelection1 <- c('1BSMT001.53','1BSMT006.62','1BSMT009.08')#1AFOU002.06')
WQM_Stations_Filter <- filter(benSampsStations, StationID %in% as.character(manualSelection1))  
# skip down to multistationInfoFin




# Spatial filters troubleshooting
### begin
assessmentRegionFilter <- c("PRO")#NULL#c("PRO")#unique(subbasins$ASSESS_REG)
subbasinFilter <- c("James River - Middle",'Potomac River')#NULL# c("James River - Middle",'Potomac River')#NULL#"James River - Lower"
#filter(subbasins, ASSESS_REG %in% assessmentRegionFilter) %>%
#  distinct(SUBBASIN) %>% st_drop_geometry() %>%  pull()
VAHU6Filter <- NULL#'JU11'#NULL 
#filter(st_drop_geometry(subbasins), SUBBASIN %in% subbasinFilter[1:2]) %>%
#  left_join(subbasinVAHU6crosswalk, by='SUBBASIN') %>%
#  left_join(st_drop_geometry(assessmentLayer), by=c('SubbasinVAHU6code'='VAHUSB') ) %>%
#  distinct(VAHU6) %>% pull()
ecoregionFilter <- NULL#"Blue Ridge"#unique(ecoregion$US_L3NAME)
dateRange_multistation <- c(as.Date('2000-01-01'), as.Date(Sys.Date()- 7))

WQM_Stations_Filter <- benSampsStations %>%
  # go small to large spatial filters
  {if(!is.null(VAHU6Filter))
    st_intersection(., filter(assessmentLayer, VAHU6 %in% VAHU6Filter))
    else .} %>%
  {if(is.null(VAHU6Filter) & !is.null(subbasinFilter))
    st_intersection(., filter(subbasins, SUBBASIN %in% subbasinFilter))
    else .} %>%
  {if(is.null(VAHU6Filter) & !is.null(assessmentRegionFilter)) # don't need assessment region filter if VAHU6 available
    st_intersection(., filter(assessmentRegions, ASSESS_REG %in% assessmentRegionFilter))
    else .} %>%
  {if(!is.null(ecoregionFilter))
    st_intersection(., filter(ecoregion, US_L3NAME %in% ecoregionFilter))
    else .} %>%
  {if(!is.null(dateRange_multistation))
    filter(., StationID %in% filter(benSamps, `Collection Date` >= dateRange_multistation[1] & `Collection Date` <= dateRange_multistation[2])$StationID)
    else .} %>%
  rename(., `Total Station Visits (Not Sample Reps)` = "Total.Station.Visits..Not.Sample.Reps.") %>%
  dplyr::select(StationID, `Total Station Visits (Not Sample Reps)`)
### end  



multistationInfoFin <- left_join(Wqm_Stations_View %>%  # need to repull data instead of calling stationInfo bc app crashes
                                   filter(Sta_Id %in% WQM_Stations_Filter$StationID) %>%
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
                                 dplyr::select(filter(WQM_Station_Full, STATION_ID %in% WQM_Stations_Filter$StationID), 
                                               STATION_ID, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                               EPA_ECO_US_L3NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                               WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, WQM_YRS_YEAR, WQM_YRS_SPG_CODE),
                                 by = c('Sta_Id' = 'STATION_ID')) %>%
  dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                EPA_ECO_US_L3NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())

# filters down to user selection from map
multistationSelection <- multistationInfoFin

multistationInfoSampleMetrics <- multistationSelection %>%
  group_by(Sta_Id) %>%
  mutate(`Years Sampled` = paste0(year(WQM_YRS_YEAR))) %>%
  dplyr::select(Sta_Id, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`) %>%
  group_by(Sta_Id, `Years Sampled`) %>%
  summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))





#### Map select interactively 
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
  addCircleMarkers(data = benSampsStations,
                   color='black', fillColor='gray', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 2,stroke=T, group="Selected Station(s)",
                   label = ~StationID, layerId = ~StationID,
                   popup = leafpop::popupTable(benSampsStations, zcol=c('StationID', 'Total Station Visits (Not Sample Reps)'))) %>%
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
                   overlayGroups = c("Selected","Level III Ecoregions", 'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')    




# Do something with selected sites

# stations to be used
WQM_Stations_Filter

multistationRarifiedFilter <- FALSE#TRUE
multistationRepFilter <- NULL#c('1')

# filter BenSamps & HabSamps to the stations selected and date range
benSamps_Filter <- filter(benSamps, StationID %in% multistationSelection$Sta_Id) %>%
  filter(`Collection Date` >= dateRange_multistation[1] & `Collection Date` <= dateRange_multistation[2]) %>%
  # bring in ecoregion info
  left_join(dplyr::select(WQM_Station_Full, WQM_STA_ID, EPA_ECO_US_L3NAME, EPA_ECO_US_L3CODE) %>%
              distinct(WQM_STA_ID, .keep_all = T),
            by = c("StationID" = "WQM_STA_ID")) %>%
  # filter by user decisions
  {if(multistationRarifiedFilter)
    filter(., grepl( 'R110', BenSampID))
    else . } %>%
  {if(!is.null(multistationRepFilter))
    filter(., RepNum %in% multistationRepFilter)
    else . } 

# offer date range filter for benthics
dateRange_benSamps_multistation <- c(as.Date(min(benSamps_Filter$`Collection Date`)), #as.Date('2014-01-01'), 
                                     as.Date(max(benSamps_Filter$`Collection Date`)))# c(as.Date('2016-01-01'), as.Date(Sys.Date()- 7))
benSamps_Filter_UserFilter <- filter(benSamps_Filter, `Collection Date` >= dateRange_benSamps_multistation[1] & `Collection Date` <= dateRange_benSamps_multistation[2])

# WQM_Station_Full missing Ecoregion info in some places so need to spatially join some
if(nrow(filter(benSamps_Filter_UserFilter, is.na(EPA_ECO_US_L3CODE))) > 0){
  benSamps_Filter_fixed <- filter(benSampsStations, StationID %in% filter(benSamps_Filter_UserFilter, is.na(EPA_ECO_US_L3CODE))$StationID) %>%
    st_intersection(ecoregion) %>% 
    st_drop_geometry() %>%
    left_join(ecoregionCrosswalk, by = "US_L3NAME") %>%
    dplyr::select(StationID, US_L3CODE, US_L3NAME)
}else{benSamps_Filter_fixed <- NULL}
  
benSamps_Filter_fin <- benSamps_Filter_UserFilter %>%
  {if(!is.null(benSamps_Filter_fixed))
    left_join(., benSamps_Filter_fixed, by = 'StationID') %>%
      mutate(EPA_ECO_US_L3NAME = ifelse(is.na(EPA_ECO_US_L3NAME), as.character(US_L3NAME),as.character(EPA_ECO_US_L3NAME)),
        EPA_ECO_US_L3CODE = ifelse(is.na(EPA_ECO_US_L3CODE), as.character(US_L3CODE),as.character(EPA_ECO_US_L3CODE))) %>%
      dplyr::select(-c(US_L3CODE,US_L3NAME))
    else .} %>%
  left_join(dplyr::select(multistationSelection, Sta_Id , Sta_Desc) %>% distinct(Sta_Id, .keep_all = T), by = c('StationID'= 'Sta_Id')) %>%
  dplyr::select(StationID, Sta_Desc, everything())
    

  
benthics_Filter <- filter(benthics, BenSampID %in% benSamps_Filter_fin$BenSampID) %>%
  left_join(dplyr::select(benSamps_Filter_fin, BenSampID, `Collection Date`)) %>%
  dplyr::select(StationID, `Collection Date`, everything()) %>%
  arrange(StationID, `Collection Date`)
benthics_Filter_crosstab <- benthics_Filter %>%
  group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
  arrange(FinalID) %>%
  pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = FinalID, values_from = Individuals) %>%
  arrange(StationID, `Collection Date`)

habSamps_Filter <- filter(habSamps, StationID %in% WQM_Stations_Filter$StationID) %>%
  filter(`Collection Date` >= dateRange_benSamps_multistation[1] & `Collection Date` <= dateRange_benSamps_multistation[2])
habObs_Filter <- filter(habObs, HabSampID %in% habSamps_Filter$HabSampID)
habValues_Filter <- filter(habValues, HabSampID %in% habSamps_Filter$HabSampID)

# identify which SCI to use


SCI_filter <- filter(VSCIresults, BenSampID %in% filter(benSamps_Filter_fin, ! EPA_ECO_US_L3CODE %in% c(63,65))$BenSampID) %>%
  bind_rows(
    filter(VCPMI63results, BenSampID %in% filter(benSamps_Filter_fin,  EPA_ECO_US_L3CODE %in% c(63))$BenSampID)  ) %>%
  bind_rows(
    filter(VCPMI65results, BenSampID %in% filter(benSamps_Filter_fin,  EPA_ECO_US_L3CODE %in% c(65))$BenSampID)  ) %>%
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
  left_join(dplyr::select(multistationSelection, Sta_Id , Sta_Desc) %>% 
              distinct(Sta_Id, .keep_all = T), 
            by = c('StationID'= 'Sta_Id')) %>%
  #left_join(dplyr::select(WQM_Station_Full, WQM_STA_ID, WQM_STA_DESC), by = c('StationID'= 'WQM_STA_ID')) %>%
  dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, SCI, `SCI Score`, `SCI Threshold`,`Sample Comments`:Season, everything())
#SCI_filter$Season <- factor(SCI_filter$Season,levels=c("Spring","Outside Sample Window","Fall"))#,ordered=T)

# Basic sampling metrics
avgSCIselection <- averageSCI_multistationAVG(benSamps_Filter_fin, SCI_filter) %>%
  arrange(SCI)

avgSCIstations <- averageSCI_multistation(benSamps_Filter_fin, SCI_filter) %>%
  arrange(StationID)



### SCI Cross tab for Billy
#colOrder <- c('StationID', 'BenSampID', 'BASINS_HU_12_NAME', 'WQM_STA_DESC', 'RepNum', 'SCI',
#              paste(rep(1970:year(Sys.Date()), times=1, each = 3), c('Spring','Fall','Outside Sample Window')))
#crosstabTemplate <- as_tibble(sapply(colOrder, function(colOrder) numeric())) %>%
#  mutate_at(vars(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, SCI), as.character)

colOrder <- c('StationID', 'BASINS_HU_12_NAME', 'WQM_STA_DESC', 'RepNum', 'Gradient', 'SCI',
              paste(rep(1970:year(Sys.Date()), times=1, each = 3), c('Spring','Fall','Outside Sample Window')))

crosstabTemplate <- as_tibble(sapply(colOrder, function(colOrder) numeric())) %>%
  mutate_at(vars(StationID, BASINS_HU_12_NAME, WQM_STA_DESC, Gradient, SCI), as.character)



SCI_crosstab_Billy_old <- function(crosstabTemplate, SCI_filter, WQM_Station_Full, columnToPlot){
  crosstabTemplate %>%
    bind_rows(
      SCI_filter %>%
        mutate(Year = year(`Collection Date`)) %>%
        left_join(dplyr::select(WQM_Station_Full, STATION_ID, WQM_STA_DESC, BASINS_HU_12_NAME),
                  by = c('StationID' = 'STATION_ID')) %>%
        distinct(BenSampID, .keep_all = T) %>%
        mutate(`Year Season` = paste(Year, Season)) %>%
        #dplyr::select(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, SCI, {{ columnToPlot }}, `Year Season`) %>%
        group_by(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum) %>%
        pivot_wider(names_from = `Year Season`, values_from = {{ columnToPlot }} ) %>%
        ungroup()) %>% 
    purrr::discard(~all(is.na(.))) %>%
    
    
    arrange(StationID, SCI)
}
x <- SCI_crosstab_Billy(crosstabTemplate, SCI_filter, WQM_Station_Full, `%Ephem`)#`SCI Score`)
# can color code by SCI Threshold column 





SCI_crosstab_Billy <- function(crosstabTemplate, SCI_filter, WQM_Station_Full, columnToPlot){
  crosstabTemplate %>%
    bind_rows(
      SCI_filter %>%
        mutate(Year = year(`Collection Date`)) %>%
        left_join(dplyr::select(WQM_Station_Full, STATION_ID, WQM_STA_DESC, BASINS_HU_12_NAME),
                  by = c('StationID' = 'STATION_ID')) %>%
        distinct(BenSampID, .keep_all = T) %>%
        mutate(`Year Season` = paste(Year, Season)) %>%
        #dplyr::select(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, SCI, {{ columnToPlot }}, `Year Season`) %>%
        group_by(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum) %>%
        pivot_wider(names_from = `Year Season`, values_from = {{ columnToPlot }} ) %>%
        ungroup() %>%
        #group_by(StationID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, Gradient) %>%
        mutate(endColumn = NA) %>%
        dplyr::select(StationID, WQM_STA_DESC, RepNum, Gradient, SCI, BASINS_HU_12_NAME:endColumn) %>%
        dplyr::select(-endColumn) %>%
        pivot_longer(!c(StationID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, Gradient, SCI), 
                     names_to = 'Year Season', values_to = 'metricChosen', values_drop_na = TRUE) %>%
        pivot_wider(names_from = `Year Season`, values_from = metricChosen,
                    values_fn = list(metricChosen = mean) ) %>%
        dplyr::select(StationID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, Gradient, SCI, everything())) %>% 
    purrr::discard(~all(is.na(.))) %>%
    arrange(StationID, SCI) 
  }
  
x <- SCI_crosstab_Billy(crosstabTemplate, SCI_filter, WQM_Station_Full, `%Ephem`)#`SCI Score`) #

  





# Billy Station Sample benthics crosstab, all the data he wants
benthics_crosstab_Billy <- benthics_Filter %>%
  dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, FinalID, Individuals) %>%
  left_join(dplyr::select(masterTaxaGenus, Order, FinalID:FamHabit)) %>%
  group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
  pivot_wider(names_from = BenSampID, values_from = Individuals) 
             
genusOrFamily <- 'Family'#'Genus'#

benthics_crosstab_Billy <- benthics_Filter %>%
  dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, FinalID, Individuals) %>%
  left_join(dplyr::select(masterTaxaGenus, Order, `Final VA Family ID`, FinalID, TolVal, FamTolVal)) %>%
  {if(genusOrFamily == 'Family')
    group_by(., StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, FamTolVal, Order) %>%
      summarize(Individuals = sum(Individuals)) %>% 
      ungroup()
    else . } %>%
  arrange(StationID, `Collection Date`) %>%
  mutate(allTheThings = #paste0(rowNumber, "_",
           paste(StationID, as.Date(`Collection Date`), paste('RepNum:', RepNum), BenSampID, sep= '\n')) %>%#) %>%
  pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
  # have to be creative organizing columns bc don't know what to expect for most
  {if(genusOrFamily == 'Family')
    dplyr::select(., -c(StationID, BenSampID, `Collection Date`, RepNum))
    else dplyr::select(., -c(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, FamTolVal)) } %>%
  dplyr::select(Order, everything()) %>%
  {if(genusOrFamily == 'Family')
    pivot_longer(., !c(Order, `Final VA Family ID`, FamTolVal), names_to = 'allTheThings', 
                 values_to = 'Individuals', values_drop_na = TRUE) %>%
      pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
      arrange(`Final VA Family ID`) 
    else pivot_longer(., !c(Order, FinalID, TolVal), names_to = 'allTheThings', 
                      values_to = 'Individuals', values_drop_na = TRUE) %>%
      pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
      arrange(FinalID) }



benthics_crosstab_Billy <- function(benthics_Filter, masterTaxaGenus, genusOrFamily){
  benthics_Filter %>%
    dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, FinalID, Individuals) %>%
    left_join(dplyr::select(masterTaxaGenus, Order, `Final VA Family ID`, FinalID, TolVal, FamTolVal)) %>%
    {if(genusOrFamily == 'Family')
      group_by(., StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, FamTolVal, Order) %>%
        summarize(Individuals = sum(Individuals)) %>% 
        ungroup()
      else . } %>%
    arrange(StationID, `Collection Date`) %>%
    mutate(allTheThings = #paste0(rowNumber, "_",
             paste(StationID, as.Date(`Collection Date`), paste('RepNum:', RepNum), BenSampID, sep= '\n')) %>%#) %>%
    pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
    # have to be creative organizing columns bc don't know what to expect for most
    {if(genusOrFamily == 'Family')
      dplyr::select(., -c(StationID, BenSampID, `Collection Date`, RepNum))
      else dplyr::select(., -c(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, FamTolVal)) } %>%
    dplyr::select(Order, everything()) %>%
    {if(genusOrFamily == 'Family')
      pivot_longer(., !c(Order, `Final VA Family ID`, FamTolVal), names_to = 'allTheThings', 
                   values_to = 'Individuals', values_drop_na = TRUE) %>%
        pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
        arrange(`Final VA Family ID`) 
      else pivot_longer(., !c(Order, FinalID, TolVal), names_to = 'allTheThings', 
                        values_to = 'Individuals', values_drop_na = TRUE) %>%
        pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
        arrange(FinalID) }
}


benthics_crosstab_Billy(benthics_Filter, masterTaxaGenus, genusOrFamily = "Genus") %>%
  datatable()




### Habitat stuff

habValues_totHab_multistation <-habSamps_Filter %>%
  group_by(HabSampID) %>%
  # get total habitat values
  left_join(totalHabScore(habValues_Filter), by = 'HabSampID') %>%
  mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall"))) %>%
  arrange(`Collection Date`)

avgTotalHab_multistation <- habValues_totHab_multistation %>% ungroup() %>%
  summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
            `n Samples` = n()) %>%
  mutate(StationID = 'User Selected Stations',
         Window = 'User Selected Window') %>%
  dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`) %>%
  bind_rows(
    habValues_totHab_multistation %>% ungroup() %>%
      group_by(Season) %>%
      summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                `n Samples` = n()) %>%
      mutate(StationID = 'User Selected Stations', 
             Window = Season) %>%
      dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`)  ) %>%
  bind_rows(
    totalHabScoreAverages(habValues_totHab_multistation) %>%
      arrange(StationID))



left_join(habObs_Filter,
          dplyr::select(habSamps_Filter, HabSampID, StationID, `Collection Date`),
          by = 'HabSampID') %>%
  #group_by( HabSampID) %>%
  #arrange(ObsParameterDescription) %>%
  pivot_wider(id_cols = c('StationID','HabSampID','Collection Date'), names_from = ObsParameterDescription, values_from = ObsValue) %>%
  arrange(StationID)  











# plotting individual samples will be too much, averaging 

SCI_filter_plot <- SCI_filter %>% 
  filter(`Collection Date` >= dateRange_benSamps_multistation[1] & `Collection Date` <= dateRange_benSamps_multistation[2]) %>%
  group_by(StationID, SCI) %>%
  mutate(`Station SCI Average` = mean(`SCI Score`),
         `Station SCI n` = n()) %>% ungroup() %>%
  group_by(StationID, Season) %>%
  mutate(`Station SCI Season Average` = mean(`SCI Score`),
         `Station SCI Season n` = n()) %>% ungroup() %>%
  group_by(StationID, Gradient) %>%
  mutate(`Station SCI Gradient (Sample Method) Average` = mean(`SCI Score`),
         `Station SCI Gradient (Sample Method) n` = n()) %>% ungroup() #%>%
  #dplyr::select(StationID, BenSampID, Season, Gradient, `SCI Score`,`Station SCI Average`,`Station SCI n`,
  #              `Station SCI Season Average`,  `Station SCI Season n`,
  #              `Station SCI Gradient (Sample Method) Average`,`Station SCI Gradient (Sample Method) n`)
  
  


SCI_filter_plot_SCIchoice  <- SCI_filter_plot %>% 
  filter(SCI == 'VSCI')

if(unique(SCI_filter_plot_SCIchoice$SCI) == 'VSCI'){sciLimit <- 60} else {sciLimit <- 42}


x <- SCI_filter_plot_SCIchoice$`Collection Date`
y <- SCI_filter_plot_SCIchoice$`Station SCI Average`
z <- SCI_filter_plot_SCIchoice$StationID

SCI_filter_plot_SCIchoice %>%
  plot_ly(x = ~x, y = ~y, type = 'bar', 
          color = ~z, width = 0.5,
          marker = list(color = ~z), 
          #marker = list(line = list(width = 1.5)),
          hoverinfo="text", text=~paste(sep="<br>",
                                        paste("StationID: ", StationID),
                                        paste("Collection Date: ", `Collection Date`),
                                        paste("Collector ID: ",`Collected By`),
                                        paste("BenSampID: ", BenSampID),
                                        paste("SCI Score: ", format(`SCI Score`, digits=3)),
                                        paste("Gradient: ", Gradient))) %>% 
  layout(#showlegend=FALSE,
    shapes = list(sciLimit, 'red', text="SCI Limit"),
    yaxis=list(title="SCI"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%Y"))



## BCG Attribute table
multiBCGtable <- benthics_Filter %>%
  dplyr::select(StationID:Individuals) %>%
  mutate(`Collection Date` = as.Date(`Collection Date`)) %>%
  arrange(StationID, `Collection Date`, RepNum) %>%
  left_join(dplyr::select(BCGattVal, FinalID, `Taxonomic Notes`: `BCGatt Comment`), by = 'FinalID')

datatable(multiBCGtable,  rownames = F, escape= F,  extensions = 'Buttons',
          options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                         pageLength=nrow(multiBCGtable), buttons=list('copy','colvis'))) %>% 
  formatStyle(names(multiBCGtable)[c(8:21, 23:29)], backgroundColor = styleInterval(bcgAttributeColors $brks, bcgAttributeColors $clrs))







#






























%>%
  plot_ly(x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
        color = ~SeasonGradient, width = 0.5,
        marker = list(color = ~SeasonGradientColor), 
        #marker = list(line = list(width = 1.5)),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", `Collection Date`),
                                      paste("Collector ID: ",`Collected By`),
                                      paste("BenSampID: ", BenSampID),
                                      paste("SCI Score: ", format(`SCI Score`, digits=3)),
                                      paste("Gradient: ", Gradient))) %>% 
layout(#showlegend=FALSE,
  shapes = list(sciLimit, 'red', text="SCI Limit")),
  yaxis=list(title="SCI"),
  xaxis=list(title="Sample Date",tickfont = list(size = 10),
             type = 'date',tickformat = "%Y"))








st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
         remove = T, # don't remove these lat/lon cols from df
         crs = 4326) %>%
  st_intersection(filter(assessmentRegions, ASSESS_REG %in% input$WQSDEQregionSelection)) %>%
  st_drop_geometry() 