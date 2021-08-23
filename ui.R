source("ui/ui_pregled.R", local = TRUE) 
source("ui/ui_tablica.R", local = TRUE)
source("ui/ui_graf_prikaz.R", local = TRUE)

ui <- fluidPage(
  title = "Diplomski rad_Valentin Šamija",
  tags$head(
  ),
  tags$style(type = "text/css", ".container-fluid {padding-left: 0px; padding-right: 0px !important;}"),
  tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
  tags$style(type = "text/css", ".content {padding: 0px;}"),
  tags$style(type = "text/css", ".row {margin-left: 0px; margin-right: 0px;}"),
  tags$style(HTML(".col-sm-12 { padding: 5px; margin-bottom: -15px; }")),
  tags$style(HTML(".col-sm-6 { padding: 5px; margin-bottom: -15px; }")),
  navbarPage(
    title       = div("Diplomski rad - Valentin Šamija", style = "padding-left: 10px"),
    inverse = TRUE,
    collapsible = TRUE,
    fluid       = TRUE,
    tabPanel("Pregled", page_overview, value = "page-overview"),
    tabPanel("Tablica", page_fullTable, value = "page-fullTable"),
    tabPanel("Grafički prikaz", page_plots, value =  "page_plots"),
    tags$script(HTML("var header = $('.navbar > .container-fluid');
    console.log(header)")
    )
  )
)