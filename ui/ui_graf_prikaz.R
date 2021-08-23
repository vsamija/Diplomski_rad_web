page_plots <- dashboardPage(
  title = "Grafički prikaz",
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(fluidRow(fluidRow(uiOutput("box_caseEvolution"))))
)