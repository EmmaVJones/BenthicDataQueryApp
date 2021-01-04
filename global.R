library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(inlmisc)
library(DT)
library(DBI)
#library(measurements) #only necessary if don't use Rex's dataset for points
library(plotly)
library(lubridate)
library(pool)
library(geojsonsf)
library(pins)
library(sqldf)
library(config)


source('helperFunctions/VSCI_metrics_GENUS.R')
source('helperFunctions/VCPMI_metrics_GENUS.R')

# Register RStudio Connect, don't need to do multiple times
#board_register("rsconnect", server = "http://deq-rstudio-prod.cov.virginia.gov:3939")

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


# Set up pool connection to production environment
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQLServer",   # note the LACK OF space between SQL and Server ( how RStudio named driver)
  # Production Environment
#  Server= "DEQ-SQLODS-PROD,50000",
#  dbname = "ODS",
#  UID = conn$UID_prod, 
#  PWD = conn$PWD_prod,
  #UID = Sys.getenv("userid_production"), # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd_production")   # need to change in Connect {vars}
  # Test environment
  #Server= "WSQ04151,50000",
  #dbname = "ODS_test",
  #UID = Sys.getenv("userid"),  # need to change in Connect {vars}
  #PWD = Sys.getenv("pwd"),  # need to change in Connect {vars}
#  trusted_connection = "yes"
#)
onStop(function() {
  poolClose(pool)
})

## For testing: connect to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

## For testing: Connect to ODS_test
# establish db connection locally
#pool <- dbPool(
#  drv = odbc::odbc(),
#  Driver = "SQL Server",  # note the space between SQL and Server ( how MS named driver)
#  Server= "WSQ04151,50000",
#  dbname = "ODS_test"
#)
#onStop(function() {
#  poolClose(pool)
#})


#### Multistation Data 

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








### Functions for app


# SCI calculation

SCI <- function(stationBenthicsDateRange, SCIchoice, benSamps, masterTaxaGenus, vmast){
  edas_options <- select(masterTaxaGenus, Class, Subclass, Order, Suborder, Superfamily, Family, `Final VA Family ID`, FinalID) %>%
    mutate(across(where(is.factor), as.character))
  edas_list <- select(edas_options, `Final VA Family ID`,FinalID)
  # for Excluding taxa, need a list of all Family level designations that may end up as a FinalID
  # these are all unique Family names and the few taxa that are the only 
  GenusNames <- c(unique(edas_options$FinalID)[!is.na(unique(edas_options$FinalID))])
  FamilyNames <- unique(edas_options$Family)[!is.na(unique(edas_options$Family))]
  SuperfamilyNames <- unique(edas_options$Superfamily)[!is.na(unique(edas_options$Superfamily))]
  SuborderNames <- unique(edas_options$Suborder)[!is.na(unique(edas_options$Suborder))]
  OrderNames <- unique(edas_options$Order)[!is.na(unique(edas_options$Order))]
  SubclassNames <- unique(edas_options$Subclass)[!is.na(unique(edas_options$Subclass))]
  ClassNames <- unique(edas_options$Class)[!is.na(unique(edas_options$Class))]
  
  
  EDASrare <- stationBenthicsDateRange %>%
    ########## #filter(str_detect(BenSampID, 'R110') & RepNum == 1) %>% # keep only rarified data and Rep1's
    mutate(Count = Individuals) %>% # Rename to match formatting of functions
    ######`Excluded Taxa` = ifelse(`Excluded Taxa` == T, -1, 0)) %>% 
    select(BenSampID, FinalID, Count, `Excluded Taxa`) %>%
    mutate(GenusTaxaLevel = ifelse(FinalID %in% GenusNames, T, F),
           FamilyTaxaLevel = ifelse(FinalID %in% FamilyNames, T, F),
           SuperfamilyTaxaLevel = ifelse(FinalID %in% SuperfamilyNames, T, F),
           SuborderTaxaLevel = ifelse(FinalID %in% SuborderNames, T, F),
           OrderTaxaLevel = ifelse(FinalID %in% OrderNames, T, F),
           SubclassTaxaLevel = ifelse(FinalID %in% SubclassNames, T, F),
           ClassTaxaLevel = ifelse(FinalID %in% ClassNames, T, F))
  
  # Work FinalID back up to Family Level
  EDASrare2 <- left_join(EDASrare,edas_list, by="FinalID") %>%
    filter(!is.na(`Final VA Family ID`)) %>%
    rename( `Genus Level Excluded Taxa` = `Excluded Taxa`)
  
  # We also need to do a little data manipulation to incorporate biologist exclusion information appropriately.
  exclusionMath  <- EDASrare2 %>%
    mutate(`Family Level Excluded Taxa` = 
             ifelse(`Genus Level Excluded Taxa` == -1, 
                    ifelse(`SuperfamilyTaxaLevel` == TRUE | `SuborderTaxaLevel` == TRUE | `OrderTaxaLevel` == TRUE | 
                             `SubclassTaxaLevel` == TRUE | `ClassTaxaLevel` == TRUE , -1, 0), 0 )) %>%
    # had to get super ifelse nesty here to make this logic work, ugly but works
    group_by(BenSampID, `Final VA Family ID`) %>%
    summarise(`Family Level Count` = sum(Count), 
              #`Genus Level Excluded Taxa` = sum(`Genus Level Excluded Taxa`),
              `Family Level Taxa` = n(),
              `Family Level Excluded Taxa` = sum(`Family Level Excluded Taxa`),
              `Final Family Level Taxa` = `Family Level Taxa` + sum(`Family Level Excluded Taxa`) )
  
  # Join bug traits
  bugTraits <- left_join(exclusionMath,vmast,by=c('Final VA Family ID') )
  
  
  if(SCIchoice == 'VSCI'){SCI <- VSCIcalculation(bugTraits,exclusionMath,vmast) %>%
    mutate(SCI = 'VSCI',
           `SCI Threshold` = 60) %>% 
    rename("SCI Score" ="Fam SCI")}
  if(SCIchoice == 'VCPMI + 63'){SCI <- VCPMI63calculation(bugTraits,exclusionMath,vmast) %>%
    mutate(SCI = 'VCPMI + 63',
           `SCI Threshold` = 42) %>% 
    rename("SCI Score" ="CPMI63+CHOWAN")}
  if(SCIchoice == 'VCPMI - 65'){SCI <- VCPMI65calculation(bugTraits,exclusionMath,vmast) %>%
    mutate(SCI = 'VCPMI - 65',
           `SCI Threshold` = 42) %>% 
    rename("SCI Score" ="CPMI65-CHOWAN")}
  
  SCI <- left_join(SCI, benSamps, by = 'BenSampID')
  
  return(SCI) 
}


# horizontal line for plotly
hline <- function(y = 0, color = "blue", text = 'test') {
  list(
    type = "line", 
    x0 = 0, 
    x1 = 1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color),
    hoverinfo="text",
    text = text
  )
}





uniqueCollector <- function(stationBenthicsFilterOptions){
  stationBenthicsFilterOptions %>%
    group_by(StationID, `Collected By`) %>%
    summarise(`n Samples` = n()) 
}

uniqueTaxonomist <- function(stationBenthicsFilterOptions){
  stationBenthicsFilterOptions %>%
    group_by(StationID, Taxonomist) %>%
    summarise(`n Samples` = n()) 
}

nSeasonSamples <- function(stationBenthicsFilterOptions){
  stationBenthicsFilterOptions %>%
    group_by(StationID, Season) %>%
    summarise(n())
}

averageSCI <- function(stationBenthicsFilterOptions, SCIresults){
  dat <- left_join(stationBenthicsFilterOptions, dplyr::select(SCIresults, StationID, BenSampID, SCI, `SCI Score`),
                   by = c('StationID', 'BenSampID')) 
  dat %>%
    group_by(StationID) %>%
    summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = 'User Selected Window') %>%
    dplyr::select(StationID, Window, `SCI Average`, `n Samples`) %>%
    bind_rows(dat %>%
                group_by(StationID, Season) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) %>%
    bind_rows(dat %>%
                mutate(Window = year(`Collection Date`)) %>%
                group_by(StationID, Window) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character(Window))) %>%
    mutate(SCI = unique(SCIresults$SCI)[1]) %>%
    dplyr::select(StationID, SCI, Window, `SCI Average`, `n Samples`)
}


totalHabScore <- function(habValues){
  habValues %>%
    group_by(HabSampID) %>%
    summarise(`Total Habitat Score` = sum(HabValue, na.rm = T))
}


uniqueFieldTeam <- function(habitatSampleDateRange){
  habitatSampleDateRange %>%
    group_by(StationID, `Field Team`) %>%
    summarise(`n Samples` = n()) 
}


habObsMetrics <- function(habObsStation){
  habObsStation %>%
    group_by(ObsParameterDescription) %>%
    summarise(Observations = sum(ObsValue)) %>%
    arrange(desc(Observations)) %>%
    rename("Parameter" = "ObsParameterDescription")
}


totalHabScoreAverages <- function(habValues_totHab){
  habValues_totHab %>%
    group_by(StationID) %>%
    summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = 'User Selected Window') %>%
    dplyr::select(StationID, Window, `Total Habitat Average`, `n Samples`) %>%
    bind_rows(habValues_totHab %>%
                group_by(StationID, Season) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) %>%
    bind_rows(habValues_totHab %>%
                mutate(Window = year(`Collection Date`)) %>%
                group_by(StationID, Window) %>%
                summarise(`Total Habitat Average` = format(mean(`Total Habitat Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character(Window)))
}


averageSCI_multistationAVG <- function(benSamps_Filter_fin, SCI_filter){
  dat <- left_join(benSamps_Filter_fin, dplyr::select(SCI_filter, StationID, BenSampID, SCI, `SCI Score`),
                   by = c('StationID', 'BenSampID')) 
  dat %>%
    group_by(SCI) %>%
    summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = 'User Selected Window') %>%
    dplyr::select(SCI, Window, `SCI Average`, `n Samples`) %>%
    bind_rows(dat %>%
                group_by(SCI, Season) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) 
}


averageSCI_multistation <- function(benSamps_Filter_fin, SCI_filter){
  dat <- left_join(benSamps_Filter_fin, dplyr::select(SCI_filter, StationID, BenSampID, SCI, `SCI Score`),
                   by = c('StationID', 'BenSampID')) 
  dat %>%
    group_by(StationID, SCI) %>%
    summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
              `n Samples` = n()) %>% 
    mutate(Window = 'User Selected Window') %>%
    dplyr::select(StationID, SCI, Window, `SCI Average`, `n Samples`) %>%
    bind_rows(dat %>%
                group_by(StationID, SCI, Season) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits = 3),
                          `n Samples` = n()) %>%
                rename('Window' = 'Season') %>% ungroup()) %>%
    bind_rows(dat %>%
                mutate(Window = year(`Collection Date`)) %>%
                group_by(StationID, SCI, Window) %>%
                summarise(`SCI Average` = format(mean(`SCI Score`, na.rm = T), digits=3),
                          `n Samples` = n()) %>% ungroup() %>%
                mutate(Window = as.character(Window))) %>%
    #mutate(SCI = unique(SCIresults$SCI)) %>%
    dplyr::select(StationID, SCI, Window, `SCI Average`, `n Samples`) %>%
    arrange(StationID)
}


benthics_crosstab_Billy <- function(benthics_Filter, masterTaxaGenus, genusOrFamily){
  benthics_Filter %>%
    dplyr::select(StationID, BenSampID, `Collection Date`, RepNum, FinalID, Individuals) %>%
    left_join(dplyr::select(masterTaxaGenus, Order, `Final VA Family ID`, FinalID, TolVal, FamTolVal, FFG, FamFFG)) %>%
    {if(genusOrFamily == 'Family')
      group_by(., StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, FamTolVal, FamFFG, Order) %>%
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
      else dplyr::select(., -c(StationID, BenSampID, `Collection Date`, RepNum, `Final VA Family ID`, FamTolVal, FamFFG)) } %>%
    dplyr::select(Order, everything()) %>%
    {if(genusOrFamily == 'Family')
      pivot_longer(., !c(Order, `Final VA Family ID`, FamTolVal, FamFFG), names_to = 'allTheThings', 
                   values_to = 'Individuals', values_drop_na = TRUE) %>%
        pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
        arrange(`Final VA Family ID`) 
      else pivot_longer(., !c(Order, FinalID, TolVal, FFG), names_to = 'allTheThings', 
                        values_to = 'Individuals', values_drop_na = TRUE) %>%
        pivot_wider(names_from = allTheThings, values_from = Individuals) %>%
        arrange(FinalID) }
}
#benthics_crosstab_Billy(benthics_Filter, masterTaxaGenus, genusOrFamily = "Genus")


#colOrder <- c('StationID', 'BenSampID', 'BASINS_HU_12_NAME', 'WQM_STA_DESC', 'RepNum', 'SCI',
#              paste(rep(1970:year(Sys.Date()), times=1, each = 3), c('Spring','Fall','Outside Sample Window')))
#crosstabTemplate <- as_tibble(sapply(colOrder, function(colOrder) numeric())) %>%
#  mutate_at(vars(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, SCI), as.character)

colOrder <- c('StationID', 'BASINS_HU_12_NAME', 'WQM_STA_DESC', 'RepNum', 'Gradient', 'SCI',
              paste(rep(1970:year(Sys.Date()), times=1, each = 3), c('Spring','Fall','Outside Sample Window')))

crosstabTemplate <- as_tibble(sapply(colOrder, function(colOrder) numeric())) %>%
  mutate_at(vars(StationID, BASINS_HU_12_NAME, WQM_STA_DESC, Gradient, SCI), as.character)


#SCI_crosstab_Billy_old <- function(crosstabTemplate, SCI_filter, WQM_Station_Full, columnToPlot){
#  crosstabTemplate %>%
#    bind_rows(
#      SCI_filter %>%
#        mutate(Year = year(`Collection Date`)) %>%
#        left_join(dplyr::select(WQM_Station_Full, STATION_ID, WQM_STA_DESC, BASINS_HU_12_NAME),
#                  by = c('StationID' = 'STATION_ID')) %>%
#        distinct(BenSampID, .keep_all = T) %>%
#        mutate(`Year Season` = paste(Year, Season)) %>%
#        #dplyr::select(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum, SCI, {{ columnToPlot }}, `Year Season`) %>%
#        group_by(StationID, BenSampID, BASINS_HU_12_NAME, WQM_STA_DESC, RepNum) %>%
#        pivot_wider(names_from = `Year Season`, values_from = {{ columnToPlot }} ) %>%
#        ungroup()) %>% 
#    purrr::discard(~all(is.na(.))) %>%
#    arrange(StationID, SCI)
#}
#SCI_crosstab_Billy_old(crosstabTemplate, SCI_filter, WQM_Station_Full, `%Ephem`)#`SCI Score`)
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
