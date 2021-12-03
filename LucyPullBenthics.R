# pull benthic data from R server (pre analyzed SCI scores)

# library
library(tidyverse)
library(pins)
library(config)

# get configuration settings
conn <- config::get("connectionSettings")

board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


VSCIresults <- pin_get("ejones/VSCIresults", board = "rsconnect")
VCPMI63results <- pin_get("ejones/VCPMI63results", board = "rsconnect")
VCPMI65results <- pin_get("ejones/VCPMI65results", board = "rsconnect")

# for prob report

#1 bring in stations of interest- with bios call on SCI to use
#2 filter each SCI table by stations that apply- 2019 and 2020 sample dates
 # there could be multiple samples to combine
# 3 convert VCPMI results to VSCI/VCPMI scale
# 4 smash together with metrics we use for prob report
