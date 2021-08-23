body_fullTable <- dashboardBody( 
  tags$head(
    tags$style(type = "text/css", "@media (min-width: 768px) { .full-table { margin-top: -30px; } }")
  ),
  fluidPage(
    fluidRow(
      h3(paste0(" Tablica na dan (", strftime(current_date, format = "%d.%m.%Y"), ")"),
        class = "box-title", style = "margin-top: 10px; font-size: 18px;"),
      div(
        dataTableOutput("fullTable"),
        class = "full-table"
      ),
      width = 12
    )
  )
)

page_fullTable <- dashboardPage(
  title   = "Tablica",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = body_fullTable
)
