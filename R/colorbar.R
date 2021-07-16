make_colorbar = function(data){
  segment(class = "horizontal equal width segments", 
          style = "width: 95%; border: none;",
          pmap(.l = list(color = data %>% group_by(ID) %>% slice(which.max(time)) %>% ungroup() %>% count(type_color) %>% pull(type_color),
                         type  = data %>% count(ship_type) %>% pull(ship_type)),
               .f = color_segment)
  )
}


color_segment = function(color, type) {
  segment(
    class = "center aligned",
    style = "background-color: white; border: none;",
    div(class = "content",
        div(class = "circle", 
            style = glue("border: none;
                         height: 15px;
                         width: 15px;
                         background-color: {color};
                         border-radius: 50%;
                         display: inline-block;")),
        div(class = "description", style = "color: black;", type)
    )
  )
}