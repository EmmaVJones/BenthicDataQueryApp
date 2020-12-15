# new station issue debugging

#geojson_sf(
#  paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
#         toupper('4AROA217.38'),"%27&outFields=*&f=geojson"))

station <- '4AROA217.38'
station <- '2-JKS023.61'#

stationInfo <- pool %>% tbl( "Wqm_Stations_View") %>%
  filter(Sta_Id %in% !! toupper(station)) %>%
  as_tibble() 


WQM_Station_Full_REST <- suppressWarnings(
  geojson_sf(
    paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
           toupper(station),"%27&outFields=*&f=geojson"))) 
if(nrow(WQM_Station_Full_REST ) > 0){
  WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA))
  
  WQM_Station_Full_REST <- bind_cols(WQM_Station_Full_REST, st_coordinates(WQM_Station_Full_REST) %>% as.tibble()) %>%
    mutate(Latitude = Y, Longitude = X) # add lat/lng in DD
} else { # station doesn't yet exist in WQM full dataset
  # get what we can from CEDS
  stationGISInfo <- pool %>% tbl( "WQM_Sta_GIS_View") %>%
    filter(Station_Id %in% !! toupper(station)) %>%
    as_tibble() 
  # pull a known station to steal data structure
  WQM_Station_Full_REST <- suppressWarnings(
    geojson_sf(
      paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
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
             crs = 4326) 
  

}


stationInfoFin <- left_join(pool %>% tbl("Wqm_Stations_View") %>%  # need to repull data instead of calling stationInfo bc app crashes
                              filter(Sta_Id %in% !! toupper(station)) %>%
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
                            dplyr::select(WQM_Station_Full_REST1, #WQM_STATIONS_FINAL, 
                                          STATION_ID, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                          EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                          WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III),
                            by = c('Sta_Id' = 'STATION_ID')) %>%
  dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, Latitude, Longitude, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                EPA_ECO_US_L3NAME, BASINS_HUC_8_NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())

