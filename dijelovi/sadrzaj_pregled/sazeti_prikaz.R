output$summaryTables <- renderUI({ 
  tabBox(
    tabPanel("Drzava/Regija",
      div(
        dataTableOutput("summaryDT_country"),
        style = "margin-top: -10px")
    ),
    width = 12
  )
})

output$summaryDT_country <- renderDataTable(getSummaryDT(data_atDate(current_date), "Country/Region", selectable = TRUE))
proxy_summaryDT_country  <- dataTableProxy("summaryDT_country")

observeEvent(input$timeSlider, {
  data <- data_atDate(input$timeSlider)
  replaceData(proxy_summaryDT_country, summariseData(data, "Country/Region"), rownames = FALSE)
}, ignoreInit = TRUE, ignoreNULL = TRUE)

observeEvent(input$summaryDT_country_row_last_clicked, {
  selectedRow     <- input$summaryDT_country_row_last_clicked
  selectedCountry <- summariseData(data_atDate(input$timeSlider), "Country/Region")[selectedRow, "Country/Region"]
  location        <- data_evolution %>%
    distinct(`Country/Region`, Lat, Long) %>%
    filter(`Country/Region` == selectedCountry) %>%
    summarise(
      Lat  = mean(Lat),
      Long = mean(Long)
    )
  leafletProxy("overview_map") %>%
    setView(lng = location$Long, lat = location$Lat, zoom = 4)
})

summariseData <- function(df, groupBy) {
  df %>%
    group_by(!!sym(groupBy)) %>%
    summarise(
      "Potvrdjeni"            = sum(confirmed, na.rm = T),
      "Oporavljeni" = sum(recovered, na.rm = T),
      "Preminuli"             = sum(deceased, na.rm = T),
      "Aktivni"               = sum(active, na.rm = T)
    ) %>%
    as.data.frame()
}

getSummaryDT <- function(data, groupBy, selectable = FALSE) {
  datatable(
    na.omit(summariseData(data, groupBy)),
    rownames  = FALSE,
    options   = list(
      order          = list(1, "desc"),
      scrollX        = TRUE,
      scrollY        = "37vh",
      scrollCollapse = T,
      dom            = 'ft',
      paging         = FALSE
    ),
    selection = ifelse(selectable, "single", "none")
  )
}