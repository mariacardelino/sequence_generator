library(shiny)
library(shinyFiles)

# ui <- fluidPage(
#   shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE)
# )
ui <- bootstrapPage(
  shinyFilesButton('files', label='File select', title='Please select a file', multiple=FALSE)
)

# server <- function(input, output){
#   volumes = getVolumes()() # this makes the directory at the base of your computer.
#   observe({
#     shinyDirChoose(input, 'folder', roots=volumes, filetypes=c('', 'txt'))
#     print(input$folder)
#   })
# }

server <- (function(input, output) {
  shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))
})

shinyApp(ui=ui, server=server)