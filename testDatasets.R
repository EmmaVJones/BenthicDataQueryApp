
# establish db connection locally
#con <- DBI::dbConnect(odbc::odbc(),
#                      Driver = "SQL Server",  # note the space between SQL and Server ( how MS named driver)
#                      Server= "WSQ04151,50000",
#                      Database = "ODS_test" )


### Production Environment
library(tidyverse)
library(pool)
library(config)
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#Driver = "SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

### Test environment
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQL Server",  # note the space between SQL and Server ( how MS named driver)
#  Server= "WSQ04151,50000",
#  dbname = "ODS_test"
#)
onStop(function() {
  poolClose(pool)
})


station <- '4AROA216.75' # '5ACHP002.03'#'2-CRL001.83'#'4AROA198.08'#'1AFOU002.06'#'2-JKS023.61'#'2-SPC002.12'#"IR2019V2151A"#"1ASAN001.45"#"1ASAN000.34"#'2-JKS023.61'

masterTaxaGenus <- pool %>% tbl(in_schema("wqm",  "Edas_Benthic_Master_Taxa_View")) %>%
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

# Used to pull data directly from REST service until it suddenly stopped working for the server
WQM_Station_Full_REST <- suppressWarnings(
  geojson_sf(
    paste0("https://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
           toupper(station),"%27&outFields=*&f=geojson"))) %>%
  mutate(WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA)) 





stationInfoFin <- left_join(pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%  # need to repull data instead of calling stationInfo bc app crashes
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
                            dplyr::select(WQM_Station_Full_REST,#WQM_STATIONS_FINAL, 
                                          STATION_ID, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                                          EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                                          WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III),
                            by = c('Sta_Id' = 'STATION_ID')) %>%
  dplyr::select(Sta_Id, Sta_Desc, `CEDS Station View Link`, `DEQ GIS Web App Link`, WQM_STA_STRAHER_ORDER, EPA_ECO_US_L3CODE,
                EPA_ECO_US_L3NAME, EPA_ECO_US_L4CODE, EPA_ECO_US_L4NAME, BASINS_VAHU6, WQS_WATER_NAME, WQS_SEC, WQS_CLASS, 
                WQS_SPSTDS, WQS_PWS, WQS_TROUT, WQS_TIER_III, everything())

stationInfo_sf <- WQM_Station_Full_REST#filter(WQM_STATIONS_FINAL, STATION_ID %in% station) 

stationInfoSampleMetrics <- stationInfo_sf %>%
  group_by(STATION_ID) %>%
  mutate(`Years Sampled` = WQM_YRS_YEAR) %>% #paste0(year(WQM_YRS_YEAR))) %>% # for when column coming in as date
  dplyr::select(STATION_ID, WQM_YRS_SPG_CODE,WQM_YRS_YEAR,`Years Sampled`,WQM_SPG_DESCRIPTION) %>%
  st_drop_geometry() %>%
  group_by(STATION_ID, `Years Sampled`) %>%
  summarise(`Sample Codes` = paste0(WQM_YRS_SPG_CODE, collapse = ' | '))

stationBenthics <- pool %>% tbl(in_schema("wqm",  "Edas_Benthic_View")) %>%
  filter(STA_ID %in% !! toupper(station)) %>%
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



stationInfoBenSamps <- pool %>% tbl(in_schema("wqm",  "Edas_Benthic_Sample_View")) %>%
  filter(STA_ID %in% !! toupper(station)) %>%
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
  left_join(dplyr::select(stationInfoFin, Sta_Id, Sta_Desc) %>% distinct(Sta_Id, .keep_all = T) , by = c('StationID' = 'Sta_Id')) %>%
  dplyr::select(StationID, Sta_Desc, BenSampID, RepNum, `Collection Date`, `Sample Comments`, `Collected By`, `Field Team`, `Entered By`,
                Taxonomist, `Entered Date`, Gradient, `Target Count`, `Number of Grids`, Season) %>%
  arrange(`Collection Date`)


## Habitat data must be reactive to adjusted to benthic or habitat date filter
habSampleStation <- pool %>% tbl(in_schema("wqm",  "Edas_Habitat_Sample_View")) %>%
  filter(STA_ID %in% !! toupper(station)) %>%
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


habValuesStation <- pool %>% tbl(in_schema("wqm",  "Edas_Habitat_Values_View")) %>%
  filter(WHS_SAMP_ID %in% !! habSampleStation$HabSampID) %>%
  as_tibble() %>%
  rename("HabSampID" = "WHS_SAMP_ID",
         "HabParameter" = "WHVP_CODE",
         "HabParameterDescription" = "WHVP_DESCRIPTION",
         "HabValue" = "WHV_HAB_VALUE",
         "HabValue Comment" = "WHV_COMMENT") %>%
  dplyr::select(HabSampID, HabParameter, HabParameterDescription, HabValue, `HabValue Comment`)

habObsStation <- pool %>% tbl(in_schema("wqm",  "Edas_Habitat_Observations_View")) %>%
  filter(WHS_SAMP_ID %in% !! habSampleStation$HabSampID) %>%
  as_tibble() %>%
  rename("HabSampID" = "WHS_SAMP_ID",
         "ObsParameter" = "WOBP_CODE",
         "ObsParameterDescription" = "WOBP_DESCRIPTION",
         "ObsValue" = "WOB_OBS_VALUE") %>%
  dplyr::select(HabSampID, ObsParameter, ObsParameterDescription, ObsValue)


# Display benthic sampling metrics
dateRange <- c(min(stationInfoBenSamps$`Collection Date`), #as.POSIXct("2010-04-30"), 
               max(stationInfoBenSamps$`Collection Date`))# Sys.Date())#
rarifiedFilter <- TRUE
repFilter <- NULL

stationInfoBenSampsDateRange <- filter(stationInfoBenSamps, `Collection Date` >= dateRange[1] & `Collection Date` <= dateRange[2]) %>%
  {if(rarifiedFilter)
    filter(.,  `Target Count` == 110)
    #filter(., grepl( 'R110', BenSampID))
    else . } %>%
  {if(!is.null(repFilter))
    filter(., RepNum %in% repFilter)
    else . }

stationBenthicsDateRange <- filter(stationBenthics, BenSampID %in% stationInfoBenSampsDateRange$BenSampID) %>%
  left_join(dplyr::select(stationInfoBenSampsDateRange, BenSampID, `Collection Date`)) %>%
  dplyr::select(`Collection Date`, everything())

crosstabBenthicsRaw <- stationBenthicsDateRange %>%
  group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
  arrange(FinalID) %>%
  pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = FinalID, values_from = Individuals) %>%
  arrange(`Collection Date`)

filter(stationBenthics, BenSampID %in% stationInfoBenSampsDateRange$BenSampID) %>%
  left_join(dplyr::select(stationInfoBenSampsDateRange, BenSampID, `Collection Date`)) %>%
  dplyr::select(`Collection Date`, everything()) %>%
  arrange(`Collection Date`)

# Raw benthic data worked back up to Family crosstab and long view
crosstabBenthicsRawFamily <- stationBenthicsDateRange %>%
  left_join(dplyr::select(masterTaxaGenus, `Final VA Family ID`,`FinalID`), by = 'FinalID') %>% 
  group_by(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`) %>% 
  mutate(Individuals = sum(Individuals, na.rm = T)) %>% 
  distinct(`Final VA Family ID`, .keep_all = T) %>% 
  dplyr::select(-FinalID) %>% 
  ungroup() %>% 
  group_by(StationID, BenSampID, `Collection Date`, RepNum) %>%
  arrange( `Final VA Family ID`) %>%
  pivot_wider(id_cols = c('StationID','BenSampID','Collection Date', 'RepNum'), names_from = `Final VA Family ID`, values_from = Individuals) %>%
  arrange(`Collection Date`)

benthicsRawFamily <- stationBenthicsDateRange %>%
  left_join(dplyr::select(masterTaxaGenus, `Final VA Family ID`,`FinalID`), by = 'FinalID') %>% 
  group_by(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`) %>% 
  mutate(Individuals = sum(Individuals, na.rm = T)) %>% 
  distinct(`Final VA Family ID`, .keep_all = T) %>% 
  dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, Individuals, 
                Taxonomist, `Entered By`, `Entered Date`) %>% 
  arrange(`Collection Date`)




glimpse(stationBenthicsFilterOptions)


# Run SCI
SCIresults <- SCI(stationBenthicsDateRange = stationBenthicsDateRange, SCIchoice = "VSCI", stationInfoBenSampsDateRange, masterTaxaGenus, vmast) %>%
  mutate_if(is.numeric, round, digits=2) # rounds all numeric columns to 2 decimal places
SCIresults <- SCI(stationBenthicsDateRange = stationBenthicsDateRange, SCIchoice = "VCPMI63 + Chowan", stationInfoBenSampsDateRange, masterTaxaGenus, vmast) %>%
  mutate_if(is.numeric, round, digits=2) # rounds all numeric columns to 2 decimal places
SCIresults <- SCI(stationBenthicsDateRange = stationBenthicsDateRange, SCIchoice = "VCPMI65 - Chowan", stationInfoBenSampsDateRange, masterTaxaGenus, vmast) %>%
  mutate_if(is.numeric, round, digits=2) # rounds all numeric columns to 2 decimal places


SCIresults$Season <- factor(SCIresults$Season,levels=c("Spring","Outside Sample Window","Fall"))#,ordered=T)


## BSA benthics output
BSAbenthicOutputFunction(SCIchoice = 'VCPMI65 - Chowan', SCIresults, WQM_Station_Full)






# test with single station data
SCI_crosstab_Billy(crosstabTemplate, SCIresults,
                   WQM_Station_Full_REST %>% st_drop_geometry(), `Family Total Taxa`)#`%Ephem`)#`SCI Score`)
glimpse(SCIresults)
glimpse(SCI_filter)

crosstabTemplate %>%
  bind_rows(
    SCIresults %>%
      mutate(Year = year(`Collection Date`)) %>%
      left_join(dplyr::select(WQM_Station_Full_REST %>% st_drop_geometry(), STATION_ID, WQM_STA_DESC, BASINS_HU_12_NAME),
                by = c('StationID' = 'STATION_ID')) %>%
      distinct(BenSampID, .keep_all = T) %>%
      mutate(`Year Season` = paste(Year, Season)) %>%
      #dplyr::select(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, SCI, {{ columnToPlot }}, `Year Season`) %>%
      group_by(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum) %>%
      pivot_wider(names_from = `Year Season`, values_from = `Family Total Taxa` ) )%>%# {{ columnToPlot }} ) #) %>% 
  purrr::discard(~all(is.na(.))) %>%
  arrange(StationID, SCI)



benthics_crosstab_Billy(stationBenthicsDateRange, #benthics_Filter,
                        masterTaxaGenus, genusOrFamily = "Genus")


# make plotly to barplot any of the metrics selected


if(unique(SCIresults$SCI) == 'VSCI'){sciLimit <- 60} else {sciLimit <- 40}


SCIresults1 <- SCIresults %>%
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
SCIresults1$SeasonGradient <- factor(SCIresults1$SeasonGradient,levels=c("Spring (Riffle)",
                                                                       "Spring (Boatable)",
                                                                       "Spring (MACS)",
                                                                       "Outside Sample Window (Riffle)",
                                                                       "Outside Sample Window (Boatable)",
                                                                       "Outside Sample Window (MACS)",
                                                                       "Fall (Riffle)",
                                                                       "Fall (Boatable)",
                                                                       "Fall (MACS)"))#,ordered=T)

SCIresults1 <- SCIresults1 %>% 
  droplevels()
levels(SCIresults1$SeasonGradient)

rep1s <- SCIresults1 %>% filter(RepNum == 1)%>% 
  droplevels()
levels(rep1s$SeasonGradient)

rep2s <-  SCIresults1 %>% ungroup %>% filter(RepNum == 2)%>% 
  droplevels()
levels(rep2s$SeasonGradient)

plot_ly(rep1s,
        x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
        color = ~SeasonGradient,  #marker = list(color = ~SeasonGradientColor,width = 0.5), # throws off color for some reason
        stroke = list(color = 'rgb(0, 0, 0)',
                      width = 3),
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
               type = 'date',tickformat = "%Y"))




sciTable <- dplyr::select(SCIresults, StationID, Sta_Desc, `Collection Date`, RepNum, `Target Count`, `Number of Grids`, Season, BenSampID:`SCI Threshold`) %>% # inclusive of different column types 
  arrange(`Collection Date`)








SCIresults %>%
  group_by(BenSampID, Gradient) %>%

plot_ly(#SCIresults,
  x = ~`Collection Date`, y = ~`SCI Score`, type = 'bar', 
          color = ~Season, width = 0.5, stroke = list(color = 'rgb(0, 0, 0)',
                                                      width = 3),
          #marker = list(line = list(width = 1.5)),
          hoverinfo="text", text=~paste(sep="<br>",
                                        paste("StationID: ", StationID),
                                        paste("Collection Date: ", `Collection Date`),
                                        paste('Replicate: ', RepNum),
                                        paste("Collector ID: ",`Collected By`),
                                        paste("BenSampID: ", BenSampID),
                                        paste("SCI Score: ", `SCI Score`))) %>%
  #add_segments(x = )  
  #add_lines(x= ~`Collection Date`,y= ~ `SCI Threshold`, mode='line',line = list(color = 'red'),
  #            hoverinfo = "text", text="SCI Limit", name="SCI Limit") %>%
    layout(#showlegend=FALSE,
      shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
      yaxis=list(title="SCI"),
      xaxis=list(title="Sample Date",tickfont = list(size = 10),
                 type = 'date',tickformat = "%Y"))




# deal with various methods
SCIresults <- SCIresults %>%
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
                                           TRUE ~ as.character(NA)) ) #,
#         `SCI Score R110` = case_when(grepl( 'R110', BenSampID) ~ `SCI Score`, TRUE ~ as.numeric(NA)),
#         `SCI Score notR110` = case_when(! grepl( 'R110', BenSampID) ~ `SCI Score`, TRUE ~ as.numeric(NA))
#         ) %>%
#  dplyr::select(StationID, BenSampID, `Collection Date`, `Collected By`, `SCI Score`, Gradient, `SCI Score R110`, `SCI Score notR110`,SeasonGradient, SeasonGradientColor)


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
#  add_trace(y = ~`SCI Score notR110`, type = 'bar', 
#            color = ~SeasonGradient, width = 0.5,
#            marker = list(color = ~SeasonGradientColor), name = 'Not R110',
#            #marker = list(line = list(width = 1.5)),
#            hoverinfo="text", text=~paste(sep="<br>",
#                                          paste("StationID: ", StationID),
#                                          paste("Collection Date: ", `Collection Date`),
##                                          paste("Collector ID: ",`Collected By`),
#                                          paste("BenSampID: ", BenSampID),
#                                          #paste("SCI Score: ", format(`SCI Score`, digits=3)),
#                                          paste("Gradient: ", Gradient))) %>% 
  layout(#showlegend=FALSE,
    shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
    yaxis=list(title="SCI"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%Y"))




# Stacked area plot for FFG
FFGdata <- left_join(stationBenthicsDateRange, 
                     dplyr::select(masterTaxaGenus, FinalID,`Final VA Family ID`:FamHabit), 
                     by = 'FinalID') %>% 
  dplyr::select(-c(Taxonomist:`Entered Date`)) %>% 
  {if(plotType == 'Genus')
    group_by(., StationID, `Collection Date`, RepNum, BenSampID, FFG) %>% 
      summarise(Count = sum(Individuals, na.rm = T)) %>% 
      mutate(TotalCount = sum(Count),
             Percent = Count / TotalCount * 100) 
    else group_by(., StationID, `Collection Date`, RepNum, BenSampID, FamFFG) %>% 
      summarise(Count = sum(Individuals, na.rm = T)) %>% 
      mutate(TotalCount = sum(Count),
             Percent = Count / TotalCount * 100) }  %>% 
  filter(BenSampID %in% c( "FAMROA3547",   "FAMROA4376",   "FAMROA6182",   "FAMROA6327",   "ROA8514R110",  "ROA8184R110" , "ROA5936R110" )) 
  

plotType <- 'Family'
xAxis <- 'BenSampID' #"Date (removes Rep 2 samples by default)"
FFGstackedBarPlotFunction <- function(FFGdata, plotType, xAxis){
  if(plotType == 'Genus'){
    if(xAxis == 'BenSampID'){
      ggplot(FFGdata, aes(x=BenSampID, y=Percent)) +
        geom_area(aes(colour = FFG, group = FFG, fill = FFG)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -90))
    } else {
      filter(FFGdata, RepNum == 1) %>% 
        ggplot(aes(x=`Collection Date`, y=Percent)) +
        geom_area(aes(colour = FFG, group = FFG, fill = FFG)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -90))    }
  } else {
    if(xAxis == 'BenSampID'){
      ggplot(FFGdata, aes(x=BenSampID, y=Percent)) +
        geom_area(aes(colour = FamFFG, group = FamFFG, fill = FamFFG))  +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -90))
    } else {
      filter(FFGdata, RepNum == 1) %>% 
        ggplot(aes(x=`Collection Date`, y=Percent)) +
        geom_area(aes(colour = FamFFG, group = FamFFG, fill = FamFFG)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -90))   }
      
  }
}
  
FFGstackedBarPlotFunction(FFGdata, 'Family', 'BenSampID')#"Date (removes Rep 2 samples by default)")
  
  
    {if(plotType == 'Genus')
       %>% 
        ggplot(aes(x=`Collection Date`, y=Percent)) +
        geom_area(aes(colour = FFG, group = FFG, fill = FFG)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -90))
      else group_by(., StationID, `Collection Date`, RepNum, BenSampID, FamFFG) %>% 
        summarise(Count = sum(Individuals, na.rm = T)) %>% 
        mutate(TotalCount = sum(Count),
               Percent = Count / TotalCount * 100,
               tot = sum(Percent)) %>% 
        ggplot(aes(x=BenSampID, y=Percent)) +
        geom_area(aes(colour = FamFFG, group = FamFFG, fill = FamFFG))  +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = -90)) } 
    
  
}

FFGstackedBarPlotFunction(FFGdata, masterTaxaGenus, "Genus")

## habitat data


useBenthicDateRange = "Use Benthic Data Date Filter"

if(useBenthicDateRange == "Use Benthic Data Date Filter"){
  habitatSampleDateRange <- filter(habSampleStation, `Collection Date` >= dateRange[1] & `Collection Date` <= dateRange[2])  
} else {
  habitatSampleDateRange <- filter(habSampleStation, `Collection Date` >= habitatDateRange[1] & `Collection Date` <= habitatDateRange[2])  }


dateRange <- c(#min(stationInfoBenSamps$`Collection Date`), 
  as.POSIXct("2014-04-30"), max(stationInfoBenSamps$`Collection Date`))

habitatSampleDateRange <- filter(habSampleStation, `Collection Date` >= dateRange[1] & `Collection Date` <= dateRange[2]) 





habValuesStationDateRange <- filter(habValuesStation, HabSampID %in%  habitatSampleDateRange$HabSampID) %>%
  left_join(dplyr::select(habitatSampleDateRange, HabSampID, `Collection Date`)) %>%
  dplyr::select(`Collection Date`, everything())
habObsStationDateRange <- filter(habObsStation, HabSampID %in%  habitatSampleDateRange$HabSampID) %>%
  left_join(dplyr::select(habitatSampleDateRange, HabSampID, `Collection Date`)) %>%
  dplyr::select(`Collection Date`, everything())
habValues_totHab <- habitatSampleDateRange %>%
  group_by(HabSampID) %>%
  # get total habitat values
  left_join(totalHabScore(habValuesStation), by = 'HabSampID') %>%
  mutate(Season = factor(Season,levels=c("Spring","Outside Sample Window","Fall")))



crosstabhabValuesStationDateRange <- habValuesStationDateRange %>%
  group_by(HabSampID, `Collection Date`) %>%
  arrange(HabParameterDescription) %>%
  pivot_wider(id_cols = c('HabSampID','Collection Date'), names_from = HabParameterDescription, values_from = HabValue) %>%
  left_join(dplyr::select(habValues_totHab, HabSampID, `Total Habitat Score`), by = 'HabSampID')

crosstabhabObsStationDateRange <- habObsStationDateRange %>%
  group_by(HabSampID, `Collection Date`) %>%
  arrange(ObsParameterDescription) %>%
  pivot_wider(id_cols = c('HabSampID','Collection Date'), names_from = ObsParameterDescription, values_from = ObsValue)


habValues_totHab <- habitatSampleDateRange %>%
  group_by(HabSampID) %>%
  # get total habitat values
  left_join(totalHabScore(habValuesStationDateRange), by = 'HabSampID') 
habValues_totHab$Season <- factor(habValues_totHab$Season,levels=c("Spring","Outside Sample Window","Fall"))#,ordered=T)


habValues_totHab %>%
  #distinct(HabSampID, .keep_all = T) %>%
  plot_ly( x = ~`Collection Date`, y = ~`Total Habitat Score`, type = 'bar', 
        color = ~Season, width = 0.5,
        #marker = list(line = list(width = 1.5)),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("StationID: ", StationID),
                                      paste("Collection Date: ", `Collection Date`),
                                      paste("Field Team: ",`Field Team`),
                                      paste("HabSampID: ", HabSampID),
                                      paste("Total Habitat Score: ", `Total Habitat Score`))) %>%
  #add_segments(x = )  
  #add_lines(x= ~`Collection Date`,y= ~ `SCI Threshold`, mode='line',line = list(color = 'red'),
  #            hoverinfo = "text", text="SCI Limit", name="SCI Limit") %>%
  layout(#showlegend=FALSE,
    #shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
    yaxis=list(title="SCI"),
    xaxis=list(title="Sample Date",tickfont = list(size = 10),
               type = 'date',tickformat = "%Y"))


#


### BSA habitat output
BSAhabitatOutputFunction <- function(habValues_totHab, habValuesStationDateRange){
  left_join(dplyr::select(habValuesStationDateRange, HabSampID, CollDate = `Collection Date`, HabParameter, HabValue),
            dplyr::select(habValues_totHab, HabSampID, StationID, `Total Habitat Score`),
            by = 'HabSampID') %>% 
    dplyr::select(StationID, CollDate, HabParameter, HabValue, TotHabSc = `Total Habitat Score`) %>% 
    arrange(CollDate)
}
BSAhabitatOutput <- BSAhabitatOutputFunction(habValues_totHab, habValuesStationDateRange)





avgTotalHab <- totalHabScoreAverages(habValues_totHab)


### BCG attribute Table

BCGtable <- stationBenthicsDateRange %>% 
  dplyr::select(`Collection Date`:Individuals) %>% 
  mutate(`Collection Date` = as.Date(`Collection Date`)) %>% 
  left_join(dplyr::select(BCGattVal, FinalID, `Taxonomic Notes`: `BCGatt Comment`), by = 'FinalID')


# Each parameter risk table and colors
bcgAttributeColors <- list(
  brks = c(1,2,3,4,5,6),
  clrs = c("#21a654","#55e607","#f7f720","#f2c602", "#f70000", "#c70610", 'gray')
)

names(BCGtable)[c(8:21, 23:29)]

datatable(BCGtable,  rownames = F, escape= F,  extensions = 'Buttons',
          options = list(dom = 'Bift', scrollX= TRUE, scrollY = '500px',
                         pageLength=nrow(BCGtable), buttons=list('copy','colvis'))) %>% 
  formatStyle(names(BCGtable)[c(8:21, 23:29)], backgroundColor = styleInterval(bcgAttributeColors $brks, bcgAttributeColors $clrs))
  


















###### trying to build plotly to plot everything but didn't work




barPlotly <- function(SCIresults, yvar){
  yvar_ <- enquo(yvar)
  
  rlang::eval_tidy(rlang::quo_squash(quo({
    plot_ly(SCIresults, x = ~`Collection Date`, y = ~ !!yvar_, type = 'bar', 
            #color = ~Season, width = 0.5,
            hoverinfo="text", text=~paste(sep="<br>",
                                          paste("StationID: ", StationID),
                                          paste("Collection Date: ", `Collection Date`),
                                          paste("Collector ID: ",`Collected By`),
                                          paste("BenSampID: ", BenSampID),
                                          paste("SCI Score: ", !!yvar))) #%>%
    #      layout(#showlegend=FALSE,
    #        shapes = list(hline(60 , 'red', text="SCI Limit")),
    #        yaxis=list(title="SCI"),
    #        xaxis=list(title="Sample Date",tickfont = list(size = 10),
    #                   type = 'date',tickformat = "%Y"))
  })
  )
  )
}

barPlotly(SCIresults, `SCI Score`)


barPlotly <- function(SCIresults, yvar){
  yvar_ <- enquo(yvar)
  sciLimit <- if(yvar == 'SCI Score'){60}else{42}
  
  SCIresults %>%
    mutate(yPlot = !!yvar_) %>%
    plot_ly(x = ~`Collection Date`, y = ~ yPlot, type = 'bar', 
            color = ~Season, width = 0.5,
            hoverinfo="text", text=~paste(sep="<br>",
                                          paste("StationID: ", StationID),
                                          paste("Collection Date: ", `Collection Date`),
                                          paste("Collector ID: ",`Collected By`),
                                          paste("BenSampID: ", BenSampID),
                                          paste("SCI Score: ", yPlot))) %>%
    #{if(as.character(yvar) == "SCI Score")
    layout(shapes = list(hline(sciLimit , 'red', text="SCI Limit")),
           yaxis=list(title="SCI"),
           xaxis=list(title="Sample Date",tickfont = list(size = 10),
                      type = 'date',tickformat = "%Y"))
  # else layout(shapes = list(hline(60 , 'red', text="SCI Limit")),
  #              yaxis=list(title="SCI"),
  #              xaxis=list(title="Sample Date",tickfont = list(size = 10),
  #                         type = 'date',tickformat = "%Y")) }
  #  
  
  
}

barPlotly(SCIresults[1:5,], `SCI Score`)

barPlotly(SCIresults[1:5,], `Fam %2Dom Score`)

barPlotly <- function(SCIresults, yvar){
  print(enquo(yvar))
}
barPlotly(SCIresults[1:5,], 'Taxonomist')




