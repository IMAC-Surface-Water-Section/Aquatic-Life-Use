
useSupportUI <- function(id){
  ns <- NS(id)
  
  tagList(
    reactableOutput(ns("debugging_table"))
  )
}

useSupportDebuggingUI <- function(id){
  ns <- NS(id)
  
  tagList(
    verbatimTextOutput(ns("debugging_text"))
  )
}

useSupportServer <- function(id, dataset1, dataset2, dataset3, dataset4){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      
      dataset1 <- dataset1()
      dataset2 <- dataset2()
      dataset3 <- dataset3()
      dataset4 <- dataset4()
      
      exceedances <- dataset1 %>% 
        full_join(dataset2) %>% 
        full_join(dataset3) %>% 
        full_join(dataset4) %>% 
        select(-c(limit_type_1, limit_value_1, limit_unit_1, limit_type_2, limit_value_2, limit_unit_2, data_usability, hardness)) %>% 
        filter(standard_violation == "EXCEEDANCE")
      
      exceedances
    })
    
    output$debugging_table <- renderReactable({
      req(data_reactive_internal())
      reactable(data_reactive_internal(),
                resizable = TRUE,
                searchable = TRUE,
                filterable = TRUE)
    })
    
    output$debugging_text <- renderPrint(nrow(dataset4() %>% filter(standard_violation == "NONE")))
    
    data_reactive_internal
    
  })
}