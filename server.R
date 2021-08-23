server <- function(input, output) { 
  sourceDirectory("dijelovi", recursive = TRUE)

  showNotification("Diplomski rad - Valentin Šamija",
    duration = NULL, type = "warning")

  # Učitavanje svakih sat vremena
  dataLoadingTrigger <- reactiveTimer(3600000)

  observeEvent(dataLoadingTrigger, {
    updateData()
  })

  observe({
    data <- data_atDate(input$timeSlider)
  })
}