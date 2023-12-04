library(shiny)
library(ceas)
library(readxl)

rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)

ui <- fluidPage(
  titlePanel(
  h1("CEAS"),
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Seahorse wave file", accept = ".xlsx"),
      selectInput("dataset", label = "Upload data", choices = rep_list),
      numericInput("ph", "pH", 7.4, min = 1, max = 14),
      numericInput("pka", "pka", 6.093),
      numericInput("buffer", "buffer", 0.1),
      selectInput("plot", "Choose a plot:",
        list(
          `Bioenergetics` = list("Bioenergetic Scope"),
          `Rate plot` = list("OCR", "ECAR"),
          `ATP` = list(
            "Basal glycolysis",
            "Max glycolysis",
            "Basal respiration",
            "Max respiration")
        )
      ),
    ),
    mainPanel(
      h3("bioscope"),
      plotOutput("bioscope"),
      h3("ECAR"),
      plotOutput("ecar_plot"),
      h3("OCR"),
      plotOutput("ocr_plot"),
      verbatimTextOutput("summary"),
      dataTableOutput("table")
    ),
  ),
)
server <- function(input, output, session) {
  dataset <- reactive({
    read_data(input$dataset)
  })
  energetics <- reactive({
    get_energetics(partition_data(dataset()), input$ph, input$pka, input$buffer)
  })
  output$summary <- renderDataTable({
    get_energetics_summary(energetics())
  })
  output$table <- renderDataTable({
    dataset()
  })
  output$ecar_plot <- renderPlot({
    rate_plot(dataset(), measure = "ECAR")
  })
  output$ocr_plot <- renderPlot({
    rate_plot(dataset(), measure = "OCR")
  })
  output$bioscope <- renderPlot({
    bioscope_plot(energetics())
  })
}
shinyApp(ui, server)
