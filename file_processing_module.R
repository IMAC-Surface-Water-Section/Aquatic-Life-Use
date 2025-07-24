fileInputUI <- function(id){
  ns <- NS(id)
  
  fileInput(ns("upload"), "Choose xlsx File", accept = ".xlsx")
}

fileProcessServer <- function(id, start_row){
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      req(input$upload)

      file_path <- input$upload$datapath
      df <- read.xlsx(file_path, startRow = start_row) %>%
        clean_names()
      
      return(df)
    })
    
    return(data)
  })
}