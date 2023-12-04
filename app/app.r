library(shiny)

ui <- fluidPage(
  h1("CEAS"),
  fileInput("file", "Choose Seahorse wave file", accept = ".xlsx"),
  selectInput("dataset", label = "Upload data", choices = ls("package:datasets")),
  numericInput("pH", "pH", 7, min = 1, max = 14),
  numericInput("pka", "pka", 0),
  selectInput("plot", "Choose a plot:",
    list(`Bioenergetics` = list("Bioenergetic Scope"),
      `Rate plot` = list("OCR", "ECAR"),
      `ATP` = list(
        "Basal glycolysis",
        "Max glycolysis",
        "Basal respiration",
        "Max respiration"))
  ),
  verbatimTextOutput("summary"),
  dataTableOutput("table")
)
server <- function(input, output, session) {
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$table <- renderDataTable({
    dataset()
  })
}
shinyApp(ui, server)
