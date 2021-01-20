# libraries needed (built in 3.6.2 but will work in most anything newer)
library(tidyverse)
library(pool)
library(config)

# get configuration settings
conn <- config::get("connectionSettings")

# establish connection to ODS production
pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "SQL Server Native Client 11.0", 
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)

# pull raw master taxa list
masterTaxaGenus <- pool %>% tbl("Edas_Benthic_Master_Taxa_View") %>%
  as_tibble()