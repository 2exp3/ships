here::i_am("APP.R")
library(fst)
library(magrittr)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(tidyr)
library(tibble)
library(lubridate)
library(shiny)
library(shiny.semantic)
library(leaflet)
library(htmlwidgets)
library(plotly)
library(waiter)

source(here::here("R/global.R")) # app data and grid
source(here::here("R/dropdowns.R"))# module
source(here::here("R/info.R"))# module
source(here::here("R/map.R"))# module
source(here::here("R/colorbar.R"))

ui = semanticPage(
  title = "Ship Dashboard",
  
  use_waiter(),
  preloader("Loading..."),
  
  grid(app_grid,
       dropdowns  = dropdowns_ui("dropdowns", data),
       map        = map_ui("map"),
       colorbar   = make_colorbar(data), 
       info_title = make_info_title(),
       info       = vessel_info_ui("info")
  )
)

server = function(input, output, session) {
  vessel_id = dropdowns_server("dropdowns", data)
  map_server("map", data, vessel_id)
  vessel_info_server("info", data, vessel_id)
}

shinyApp(ui, server)

