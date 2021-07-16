# Vessel Info Module ====
## UI ----
vessel_info_ui = function(id) {
  vertical_layout(
    rows_heights = c("30%","30%","2%","30%"),
    card(class = "fluid",
         div(class = "content", htmlOutput(NS(id, "vessel_info")))),
    card(class = "fluid",
         div(class = "content", htmlOutput(NS(id, "vessel_route")))),
    segment(class = "basic"),
    card(class = "fluid",
         div(class = "content",  plotlyOutput(NS(id, "vessel_plot"), height = "250")))
  )
}


## Server ----
vessel_info_server = function(id, data, vessel_id) {
  moduleServer(id, function(input, output, session) {
    
    placeholder = make_placeholder(names(data), "--")
    
    output$vessel_info =
      renderUI({
        update_info_content(placeholder)
      })
    
    output$vessel_route =
      renderUI({
        update_route_content(placeholder)
      })
    
    output$vessel_plot =
      renderPlotly({
        make_vessel_plot(data[1,]) 
      })
    
    observeEvent(vessel_id(), ignoreInit = TRUE,ignoreNULL = TRUE,{
      vessel_max =  
        data %>% 
        filter(ID == vessel_id()) %>% 
        slice(which.max(time)) %>% 
        mutate(across(everything(), ~str_replace_na(.x, "-")))
      
      output$vessel_info =
        renderUI({
          update_info_content(vessel_max)
        })
      
      output$vessel_route =
        renderUI({
          update_route_content(vessel_max)
        })
      
      plotlyProxy("vessel_plot", session) %>%
        plotlyProxyInvoke("animate", update_vessel_plot(vessel_max))
    })
  })
}

## Helper functions ----
make_info_title = function(){
  segment(class = "basic fluid", 
          p("Vessel information ", icon("ship"),
            style = "text-align: center; font-size: 25px; font-size: 1.75vw; color: darkslategray;"))
}

make_placeholder = function(names_struct, holder){
  placeholder = rep(holder,length(names_struct))
  names(placeholder) = names_struct
  placeholder %>% t() %>% as_tibble()
}

### Info content ----
update_info_content = function(data){
  # card(class = "fluid",
  #   div(class = "content",
  list(
        div(class = "header",div(class="wrapper", style = "display: grid; grid-template-columns: 2fr 1fr;",
                                 div(class="block", style = "text-align: left;",data$fixed_name),
                                 make_flag(data$FLAG) ) ) ,
        div(class = "meta", paste0("ID: ",data$ID)),
        div(class = "description", make_description(data))
    )
  # )
  
}

make_flag = function(country){
  flag = ifelse(country == "--",
                country,
                glue("{country} <img src='https://www.countryflags.io/{country}/flat/32.png'>"))
  div(class="block",
      style = "text-align: right; margin-top: -5px;",
      HTML(flag)
  )
}

make_description = function(data){
  HTML(glue("Type: <b>{data$ship_type}</b> vessel<br>
            Length: <b>{data$LENGTH}</b> meters<br>
            Width: <b>{data$WIDTH}</b> meters<br>
            Deadweight: <b>{data$DWT}</b> tons<br>"))
}

### Route content ----
update_route_content = function(data){
  date = ifelse(data$time == "--",
                data$time,
                HTML(format(as.Date(data$time), "%d %b %Y")))
  if (data$move_type=="moved" | date=="--"){
    list(
      div(class = "meta", "Longest distance sailed"),
      div(class = "description",
          div(class="wrapper", style = "display: grid; grid-template-columns: 1fr 1fr;",
              div(class="block", 
                  title = "+/- 10 meters", 
                  style= "text-align: right; font-size: 45px; font-size: 3vw; margin-top: 0.5em;",
                  data$distance),
              div(class="block", 
                  style= "text-align: left; font-size: 15px; font-size: 0.9vw; margin-top: 2.6em;",
                  " meters")),
          br(),
          icon("calendar alternate outline", title = "Day of movement", style = "font-size: large;"), date, br(),
          icon("compass outline", title = "Course",style = "font-size: large;"), HTML(glue("<b>{data$COURSE}</b> °")), br(),
          icon("tachometer alternate", title = "Speed", style = "font-size: large;"),  HTML(glue("<b>{data$speed_kt}</b> knots"))
      )
    )
    
  }else if(data$move_type=="parked"){
    list(
      div(class = "meta", "This vessel is parked", icon("anchor",style = "font-size: large;")),
      div(class = "description",
          p(HTML(enc2utf8(glue("near <b>{data$port}</b> port"))),
            style= "text-align: center; font-size: 30px; font-size: 1.5vw; margin-top: 0.5em;"),
          br(),
          icon("calendar alternate outline", title = "Day of movement", style = "font-size: large;"), date, br(),
          icon("compass outline", title = "Course",style = "font-size: large;"), HTML(glue("<b>--</b> °")), br(),
          icon("tachometer alternate", title = "Speed", style = "font-size: large;"),  HTML(glue("<b>--</b> knots"))
      )
      
    )
  }else if(data$move_type=="undefined"){
      list(
        div(class = "meta", "There is only 1 record for this vessel", icon("exclamation", style = "font-size: large;")),
        div(class = "description",div(class="wrapper", style = "display: grid; grid-template-columns: 1fr 1fr;",
                                      div(class="block", 
                                          title = "+/- 10 meters", 
                                          style= "text-align: right; font-size: 45px; font-size: 3vw; margin-top: 0.5em;",
                                          "--"),
                                      div(class="block", 
                                          style= "text-align: left; font-size: 15px; font-size: 0.9vw; margin-top: 2.6em;",
                                          " meters")),
            br(),
            icon("calendar alternate outline", title = "Day of movement", style = "font-size: large;"), date, br(),
            icon("compass outline", title = "Course",style = "font-size: large;"), HTML(glue("<b>{data$COURSE}</b> °")), br(),
            icon("tachometer alternate", title = "Speed", style = "font-size: large;"),  HTML(glue("<b>{data$speed_kt}</b> knots"))
        )
      )
      
    }
}

### Plot ----
make_vessel_plot = function(data_struct){
  plot_data =
    data_struct %>% 
    mutate(across(starts_with(c("LENGTH","WIDTH","DWT","distance","speed_kt")), ~.x * 0)) %>% 
    pivot_longer(cols = ends_with("rank"),names_to = "var",values_to = "rank") %>% 
    mutate(y_var = c("Length ", "Width ", "Deadweight ", "Longest <br>distance ", "Speed "),
           y_varf = factor(y_var, levels = y_var))
  
  plot_data %>% 
    bind_cols(.,plot_data %>% 
                select(ends_with("max")) %>% 
                slice_head() %>% 
                pivot_longer(everything(),values_to = "max") %>% select(max)) %>% 
    mutate(all_name = glue("All {ship_type} vessels")) %>% 
    
    plot_ly() %>% 
    add_bars(x = ~max,
             y = ~y_varf, 
             name = ~all_name,
             orientation = 'h', 
             width = 0.2,
             marker = list(color = "lightgray")) %>%
    add_bars(x = ~rank, 
             y = ~y_varf, 
             orientation = 'h', 
             width = 0.2, 
             name = ~fixed_name,
             marker = list(color = ~type_color)) %>%
    layout(
      autosize   = TRUE,
      title      = list(text = "Ranking among vessels<br>", x = 0, font = list(size = 14)),
      barmode    = "overlay",
      margin     = list(l=0),
      showlegend = FALSE,
      bargap     = 0.2,
      font       = list(size = 12),
      xaxis      = list(title = "", range = c(0,100)),
      yaxis      = list(title = "", autorange = "reversed")
    )
}

update_vessel_plot = function(data){
  plot_data = 
    bind_cols(
      rank      = data %>% select(ends_with("rank")) %>% t() %>% .[,1],
      max       = data %>% select(ends_with("max")) %>% t() %>% .[,1],
      color     = data$type_color,
      ship_type = data$ship_type,
      name      = data$fixed_name
    ) %>% 
    mutate(all_name  = glue("All {ship_type} vessels"),
           hover     = glue('<b>{name}</b><br>#{rank} of {max}<extra></extra>'),
           hovermax  = glue('{max} <b>{ship_type}</b><br> vessels reported<extra></extra>'),
           title     = glue("Ranking among <b>{ship_type}</b> vessels<br>"))
  
  list(
    data = 
      list(
        list(
          x             = plot_data$max,
          name          = plot_data$all_name,
          hovertemplate = plot_data$hovermax
        ),
        list(
          x             = plot_data$rank,
          marker        = list(color = plot_data$color),
          name          = plot_data$name,
          hovertemplate = plot_data$hover
        )
      ),
    traces = list(0, 1),
    layout = list(xaxis = list(title = "", range = c(0, max(plot_data$max))),
                  title = list(text = plot_data$title[1], x = 0, font = list(size = 14)))
  )
}

