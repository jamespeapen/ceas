library(shiny)
library(ceas)
library(readxl)
library(ggplot2)

rep_list <- system.file("extdata", package = "ceas") |>
  list.files(pattern = "*.xlsx", full.names = TRUE)

ui <- fluidPage(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.ico")),
  titlePanel(
    "High CEAS",
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose Seahorse wave file", accept = ".xlsx"),
      selectInput("dataset", label = "Example data", choices = rep_list),
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
      plotOutput("plot", width = "60%"),
      downloadButton("plot_download"),
      dataTableOutput("table")
    ),
  ),
)

server <- function(input, output, session) {
  dataset <- reactive({
    if (is.null(input$file)) {
      read_data(input$dataset)
    } else {
      read_data(input$file$datapath)
    }
  })
  energetics <- reactive({
    get_energetics(partition_data(dataset()), input$ph, input$pka, input$buffer)
  })
  output$table <- renderDataTable({
    get_energetics_summary(energetics())
  })
  plotter <- reactive({
    if (input$plot == "Bioenergetic Scope") {
      bioscope_plot(energetics())
    } else if (input$plot %in% c("OCR", "ECAR")) {
      rate_plot(dataset(), measure = input$plot, assay = ifelse(input$plot == "OCR", "MITO", "GLYCO"))
    } else {
      a <- sapply(strsplit(input$plot, " "), tolower)
      atp_plot(energetics(), basal_vs_max = a[1], glyc_vs_resp = substr(a[2], 1, 4))
    }
  })
  output$plot <- renderPlot({
    plotter() + theme(text = element_text(size = 15))
  })
  output$plot_download <- downloadHandler(
    filename = function() {paste0("plot.png")},
    content = function(file) ggsave(file, plot = plotter(), device = "png", dpi = 300)
  )
}
shinyApp(ui, server)
