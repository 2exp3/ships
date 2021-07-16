# Map module ====

## UI ----
map_ui = function(id){
    leafletOutput(NS(id,"ship_map"),width = "95%", height = "100%")
}

## Server ----
map_server = function(id, data, vessel_id) {
  moduleServer(id, function(input, output, session) {

    output$ship_map = renderLeaflet({
      base_map(data)
      })

    observeEvent(vessel_id(), ignoreInit = TRUE,ignoreNULL = TRUE,{
      update_map("ship_map", data, vessel_id())
    })
  })
}

## Helper functions ----

base_map = function(data){
  stopifnot(is.data.frame(data))

  leaflet(data, options = leafletOptions(preferCanvas = TRUE, zoomControl = FALSE, attributionControl = TRUE) ) %>% 
    fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT)) %>%
    onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>% 
    addProviderTiles(
      providers$CartoDB.Positron,  
      options = 
        providerTileOptions(
          updateWhenZooming = FALSE,      
          updateWhenIdle = FALSE
        ) ) %>% 
    addLegend(
      layerId  = "legend",
      position = "bottomleft",
      colors   = rep("white", 2),
      labels   = rep("--", 2),
      title    = "Vessel movement"
    ) 
}

update_map = function(map_id, data, vessel_id){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(map_id))
  stopifnot(is.character(vessel_id))
  
  map_data = 
    data %>% 
    filter(ID == vessel_id) 
  
  tooltip = map(map_data$time, make_tooltip)
  circle  = make_legend_circle(border = "white", size = 16, fill = map_data$type_color)
  radius  = ifelse(map_data$move_type=="moved",c(10,12), 12 )
  label   = ifelse(map_data$move_type=="moved",c("Beginning", "End"), "Most current position" )


  leafletProxy(mapId = map_id, data = map_data) %>% 
    clearShapes() %>% 
    clearMarkers() %>% 
    removeControl(layerId = "legend") %>% 
    fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT)) %>% 
    addCircleMarkers(
                     lng = ~LON, lat = ~LAT, 
                     fillColor   = ~type_color, 
                     radius      = radius, 
                     fillOpacity = 1,
                     color       = "white",
                     weight      = 3, 
                     label       = tooltip
    ) %>% 
    addLegend(
      layerId  = "legend",
      position = "bottomleft",
      colors   = circle,
      labels   = label,
      title    = "Vessel movement"
    )
}

make_tooltip = function(time){
  Date    = as.Date(time)
  Time    = unlist(strsplit(as.character(time)," "))[2]
  HTML(
    paste0(
      icon("calendar alternate outline", style = "font-size: medium;"),format(Date, "%d %b %Y"),br(),
      icon("clock outline", style = "font-size: medium;"),Time
    )
  )
}

make_legend_circle = function(fill, size, border) {
  glue("{fill};\\
       width:{size}px;\\
       height:{size}px;\\
       border: 3px solid {border};\\
       border-radius: 50%;")
}