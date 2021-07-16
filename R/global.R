# usethis::use_test()

data = read_fst(here::here("data","ships_data.fst"), as.data.table = TRUE)

app_grid =
  grid_template(
    default =
      list(
        areas       = rbind(c("dropdowns", "info_title"),
                            c("map",       "info"),
                            c("colorbar",  "info")),
        rows_height = c("10%", "75%","5%"),
        cols_width  = c("75%", "22%")
      ))

preloader = function(text){
  stopifnot(is.character(text))
  content = 
    tagList(
      div(icon("ship", style = "font-size: 30px; font-size: 5vw; color:darkslategray;")),
      p(img(src = "https://media.tenor.com/images/23c2e8bc408bf228fadaaee35b8eddac/tenor.gif", 
            style = "height: 40px; height: 1.75vw; "),text,
        style = "text-align: center; font-size: 25px; font-size: 1.2vw; color: darkslategray; background-size: 100%;")
    )
  waiter_preloader(html = content,
                   color = "white")
}






