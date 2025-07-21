library(shiny)
library(shinyFiles)

ui <- fluidPage(
  titlePanel("Folder and File Selection"),
  
  # Folder selector for R: drive
  shinyDirButton("folder", "Choose folder on R drive", "Select a folder"),
  
  br(), br(),
  
  # File input for uploading files from client
  fileInput("file", "Upload a file"),
  
  br(),
  
  verbatimTextOutput("folder_path"),
  verbatimTextOutput("file_info")
)

server <- function(input, output, session) {
  roots <- c("R Drive" = "R:/")
  
  shinyDirChoose(input, "folder", roots = roots, session = session)
  
  folder_path <- reactive({
    req(input$folder)
    parseDirPath(roots, input$folder)
  })
  
  output$folder_path <- renderPrint({
    paste("Selected folder:", folder_path())
  })
  
  output$file_info <- renderPrint({
    req(input$file)
    input$file
  })
}

shinyApp(ui, server)
