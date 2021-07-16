# usethis::use_test()
# Dropdown menus for vessel type, name and ID.====

## UI----
dropdowns_ui = function(id, data) {
  flow_layout(
    max_cell_width = "30%", column_gap = "2.5%",
    tagList(
      HTML("<p><b>Select vessel type:</b></p>"),
      dropdown_type_ui(NS(id, "dropdown_type"), data %>% count(ship_type) %>% pull(ship_type)),br()),
    tagList(
      HTML("<p><b>Select vessel name:</b></p>"),
      dropdown_name_ui(NS(id, "dropdown_name")), br()),
    tagList(
      HTML("<p><b>Select vessel ID:</b></p>"),
      dropdown_vid_ui(NS(id, "dropdown_vid")), br())
  )
}

dropdown_type_ui = function(id, ship_types) {
  stopifnot(is.character(ship_types))
  
  dropdown_input(input_id    = NS(id,"type"),
                 default_text="Select vessel type",
                 type = "fluid selection",
                 choices     = ship_types)
}

dropdown_name_ui = function(id) {
  dropdown_input(input_id    = NS(id,"name"),
                 default_text= "Select vessel name",
                 type = "fluid search selection",
                 choices     = NULL)
}

dropdown_vid_ui = function(id) {
  dropdown_input(input_id    = NS(id,"vid"),
                 default_text= "Select vessel ID",
                 type = "fluid search selection",
                 choices     = NULL)
}

## Server ----
dropdowns_server = function(id, data) {
  stopifnot(!is.reactive(data))
  moduleServer(id, function(input, output, session) {
    type   = dropdown_type_server("dropdown_type")
    name   = dropdown_name_server("dropdown_name", type, data, vid)
    vid    = dropdown_vid_server("dropdown_vid", type, data, name)
    vid
  })
}

dropdown_type_server = function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$type)
  })
}

dropdown_name_server = function(id, type, data, vid) {
  stopifnot(is.reactive(type))
  stopifnot(!is.reactive(data))
  moduleServer(id, function(input, output, session) {
    observeEvent(type(), {
      update_dropdown_input(session, "name",choices = get_names_ids_by_type(data, type())$fixed_name)
    })
    observeEvent(vid(), {
      update_dropdown_input(session, "name",value = map_name_id(data, "ID", vid(), "fixed_name"))
    })
    reactive(input$name)
  })
}

dropdown_vid_server = function(id, type, data, name) {
  stopifnot(is.reactive(type))
  stopifnot(!is.reactive(data))
  moduleServer(id, function(input, output, session) {
    observeEvent(type(), {
      update_dropdown_input(session, "vid", choices = get_names_ids_by_type(data, type())$ID)
    })
    observeEvent(name(), {
      update_dropdown_input(session, "vid", value = map_name_id(data, "fixed_name", name(), "ID"))
    })
    reactive(input$vid)
  })
}

# Helper functions ----
get_names_ids_by_type = function(data, type){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(type))
  
  data %>% 
    filter(ship_type == type) %>% 
    group_by(ID) %>% 
    slice_head() %>% 
    ungroup() %>% 
    select(ID, fixed_name)
}

map_name_id = function(data, filter_var, filter, count_var){
  stopifnot(is.data.frame(data))
  stopifnot(is.character(filter_var))
  stopifnot(is.character(filter))
  stopifnot(is.character(count_var))
  
  data %>% 
    filter(!!as.name(filter_var) == filter) %>% 
    count(!!as.name(count_var)) %>% 
    pull(!!as.name(count_var))
}
