# anDebuggingUI <- function(id){
#   ns <- NS(id)
# 
#   tagList(
#     reactableOutput(ns("an_debugging_table"))
#   )
# }
# 
# anDebuggingTextUI <- function(id){
#   ns <- NS(id)
#   
#   tagList(
#     h4("Values from numericInputs:"),
#     verbatimTextOutput(ns("an_debugging_text"))
#   )
# }

ammoniaNitrogenServer <- function(id, data, numericInput_ids){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data())
      
      data <- data()
      
      values <- numericInput_ids()
      
      lakes_ammonia_nitrogen <- data %>% 
        mutate(hardness = as.numeric(hardness),
               unrounded_depth = as.numeric(sample_depth),
               sample_depth = round(sample_depth, 0)) %>% 
        filter(analyte %in% c("AMMONIA-NITROGEN", "PH")) %>% 
        group_by(mlid, start_date, sample_depth) %>% 
        mutate(pH = ifelse(analyte == "PH", as.numeric(result_value), NA)) %>% 
        fill(pH, .direction = "downup") %>% 
        ungroup() %>% 
        filter(!is.na(pH), 
               analyte == "AMMONIA-NITROGEN",
               !is.na(result_value)) %>% 
        mutate(acute_standard = (values[["an_acute_param1"]]/(1+(10^(values[["an_acute_param2"]]-pH))))+(values[["an_acute_param3"]]/(1+(10^(pH-values[["an_acute_param2"]]))))) %>% 
        mutate(acute_standard = ifelse(acute_standard > values[["an_max_limit"]] | is.na(acute_standard), values[["an_max_limit"]], acute_standard),
               result_value = as.numeric(result_value),
               quotient = result_value/acute_standard,
               standard_violation = ifelse(quotient > 1, "EXCEEDANCE", "NONE")) %>% 
        relocate(acute_standard, quotient, standard_violation, .before = 1)
      
      lakes_ammonia_nitrogen
      
    })
    
    # output$an_debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    # 
    # output$an_debugging_text <- renderPrint({
    #   values <- numericInput_ids()
    #   
    #   cat(values[["an_acute_param1"]], "\n")
    # })
    
  })
}