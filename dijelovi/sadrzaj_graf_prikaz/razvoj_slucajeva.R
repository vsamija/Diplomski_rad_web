output$case_evolution <- renderPlotly({
  data <- data_evolution %>%
    group_by(date, var) %>%
    summarise(
      "value" = sum(value, na.rm = T)
    ) %>%
    as.data.frame()

  p <- plot_ly(
    data,
    x     = ~date,
    y     = ~value,
    name  = sapply(data$var, capFirst),
    color = ~var,
    type  = 'scatter',
    mode  = 'lines') %>%
    layout(
      yaxis = list(title = "Slucajevi"),
      xaxis = list(title = "Datum")
    )


  if (input$checkbox_logCaseEvolution) {
    p <- layout(p, yaxis = list(type = "log"))
  }

  return(p)
})

getDataByCountry <- function(countries, normalizeByPopulation) {
  req(countries)
  data_confirmed <- data_evolution %>%
    select(`Country/Region`, date, var, value, population) %>%
    filter(`Country/Region` %in% countries &
      var == "confirmed" &
      value > 0) %>%
    group_by(`Country/Region`, date, population) %>%
    summarise("Confirmed" = sum(value, na.rm = T)) %>%
    arrange(date)
  if (nrow(data_confirmed) > 0) {
    data_confirmed <- data_confirmed %>%
      mutate(Confirmed = if_else(normalizeByPopulation, round(Confirmed / population * 100000, 2), Confirmed))
  }
  data_confirmed <- data_confirmed %>% as.data.frame()

  data_recovered <- data_evolution %>%
    select(`Country/Region`, date, var, value, population) %>%
    filter(`Country/Region` %in% countries &
      var == "recovered" &
      value > 0) %>%
    group_by(`Country/Region`, date, population) %>%
    summarise("Estimated Recoveries" = sum(value, na.rm = T)) %>%
    arrange(date)
  if (nrow(data_recovered) > 0) {
    data_recovered <- data_recovered %>%
      mutate(`Estimated Recoveries` = if_else(normalizeByPopulation, round(`Estimated Recoveries` / population * 100000, 2), `Estimated Recoveries`))
  }
  data_recovered <- data_recovered %>% as.data.frame()

  data_deceased <- data_evolution %>%
    select(`Country/Region`, date, var, value, population) %>%
    filter(`Country/Region` %in% countries &
      var == "deceased" &
      value > 0) %>%
    group_by(`Country/Region`, date, population) %>%
    summarise("Deceased" = sum(value, na.rm = T)) %>%
    arrange(date)
  if (nrow(data_deceased) > 0) {
    data_deceased <- data_deceased %>%
      mutate(Deceased = if_else(normalizeByPopulation, round(Deceased / population * 100000, 2), Deceased))
  }
  data_deceased <- data_deceased %>% as.data.frame()

  return(list(
    "confirmed" = data_confirmed,
    "recovered" = data_recovered,
    "deceased"  = data_deceased
  ))
}



output$selectize_casesByCountries_new <- renderUI({
  selectizeInput(
    "selectize_casesByCountries_new",
    label    = "Odaberite drzavu",
    choices  = c("All", unique(data_evolution$`Country/Region`)),
    selected = "All"
  )
})

output$case_evolution_new <- renderPlotly({
  req(input$selectize_casesByCountries_new)
  data <- data_evolution %>%
    mutate(var = sapply(var, capFirst)) %>%
    filter(if (input$selectize_casesByCountries_new == "All") TRUE else `Country/Region` %in% input$selectize_casesByCountries_new) %>%
    group_by(date, var, `Country/Region`) %>%
    summarise(new_cases = sum(value_new, na.rm = T))

  if (input$selectize_casesByCountries_new == "All") {
    data <- data %>%
      group_by(date, var) %>%
      summarise(new_cases = sum(new_cases, na.rm = T))
  }

  p <- plot_ly(data = data, x = ~date, y = ~new_cases, color = ~var, type = 'bar') %>%
    layout(
      yaxis = list(title = "Novi slucajevi"),
      xaxis = list(title = "Datum")
    )
})

output$box_caseEvolution <- renderUI({
  tagList(
    fluidRow(
      box(
        title = "Razvoj od pocetka pandemije",
        plotlyOutput("case_evolution"),
        column(
          checkboxInput("checkbox_logCaseEvolution", label = "Logaritamski prikaz", value = FALSE),
          width = 3,
          style = "float: right; padding: 10px; margin-right: 50px"
        ),
        width = 6
      ),
      box(
        title = "Novi slucajevi",
        plotlyOutput("case_evolution_new"),
        column(
          uiOutput("selectize_casesByCountries_new"),
          width = 3,
        ),
        column(
          HTML("Napomena: Aktivni se slucajevi racunaju na nacin da se od potvrdjenih oduzme zbroj oporavljenih i preminulih. 
                Broj aktivnih slucajeva moze biti nekoliko dana negativan ako je na dan pregledavanja broj umrlih i oporavljenih 
                bio veci od broja aktivnih slucajeva!"),
          width = 7
        ),
        width = 6
      )
    ),
      )
})