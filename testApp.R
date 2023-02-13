
httr::set_config(httr::config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

library(tidyverse)
library(sf)
library(shiny)
library(shinybusy)
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
library(readxl)
library(dbplyr)
server <- shinyServer(function(input, output, session) {
    
    # empty reactive objects list
    reactive_objects = reactiveValues() 
    
    # # test station viability in separate object
    # observeEvent(input$begin, {
    #   ## Station Information
    #   reactive_objects$stationInfo <- pool %>% tbl(in_schema("wqm",   "Wqm_Stations_View")) %>%
    #     filter(Sta_Id %in% !! toupper(input$station)) %>%
    #     as_tibble() 
    #   
    #   if(nrow(reactive_objects$stationInfo) == 0){
    #     showNotification("Not a valid StationID.")   }  })
    
    ## Pull station info from REST service
    WQM_Station_Full_REST <- reactive({
      req(input$begin)#, nrow(reactive_objects$stationInfo) != 0)
      
      WQM_Station_Full_REST <- suppressWarnings(
        geojson_sf(
          paste0("http://apps.deq.virginia.gov/arcgis/rest/services/public/WQM_STATIONS_ALL/MapServer/0/query?&where=STATION_ID%3D%27",
                 #"http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%27",
                 toupper(input$station),"%27&outFields=*&f=geojson")))
      
      if(nrow(WQM_Station_Full_REST ) > 0){
        WQM_Station_Full_REST <- mutate(WQM_Station_Full_REST, WQM_YRS_YEAR = ifelse(!is.na(WQM_YRS_YEAR), lubridate::year(as.Date(as.POSIXct(WQM_YRS_YEAR/1000, origin="1970-01-01"))), NA))
        WQM_Station_Full_REST <- bind_cols(WQM_Station_Full_REST, st_coordinates(WQM_Station_Full_REST) %>% as.tibble()) %>%
          mutate(Latitude = Y, Longitude = X) # add lat/lng in DD
      } else { # station doesn't yet exist in WQM full dataset
        # get what we can from CEDS
        stationGISInfo <- pool %>% tbl( in_schema("wqm",  "WQM_Sta_GIS_View")) %>%
          filter(Station_Id %in% !! toupper(input$station)) %>%
          as_tibble() 
        # pull a known station to steal data structure
        WQM_Station_Full_REST <- suppressWarnings(
          geojson_sf(
            paste0("http://apps.deq.virginia.gov/arcgis/rest/services/public/WQM_STATIONS_ALL/MapServer/0/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
          #"http://gis.deq.virginia.gov/arcgis/rest/services/staff/DEQInternalDataViewer/MapServer/104/query?&where=STATION_ID%3D%272-JKS023.61%27&outFields=*&f=geojson")))[1,] %>%
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
    
    output$stationInfoTable <- DT::renderDataTable({
      
      datatable(WQM_Station_Full_REST() %>% st_drop_geometry(), 
                rownames = F, escape= F, extensions = 'Buttons', selection = 'none',
                options = list(dom = 'Bt', scrollX= TRUE, scrollY = '100px',
                               pageLength = nrow(WQM_Station_Full_REST() %>% st_drop_geometry()),
                               buttons=list('copy','colvis')))  })
    
})
    






ui <- shinyUI(fluidPage(tags$head(
  tags$style(
    HTML(".shiny-notification {position:fixed; top: calc(60%); left: calc(10%); }"))),
  theme= "yeti.css",
  navbarPage("CEDS Benthic Data Query Tool", id = 'someID',  # key for passing URL to specific Tab
             
             # tabPanel('How To',
             #          includeMarkdown("BenthicQueryToolHowToNew.md")),
             # #includeHTML("BenthicQueryToolHowTo.html")),#htmlOutput("BenthicQueryToolHowTo") ), # this was a hot mess. when trying to update the BenthicQueryHowTo rmd and rendering to html, the app would not take any user inputs. so weird and wasted hours on this problem. ultimately had to go with rendering the .md in the app to have any semblance of a solution
             # 
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
                                 )))))))
                        

shinyApp(ui, server)