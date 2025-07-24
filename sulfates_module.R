# sulfatesDebuggingUI <- function(id){
#   ns <- NS(id)
# 
#   tagList(
#     reactableOutput(ns("sulfates_debugging_table"))
#   )
# }
# 
# sulfatesDebuggingTextUI <- function(id){
#   ns <- NS(id)
#   
#   tagList(
#     verbatimTextOutput(ns("sulfates_debugging_text"))
#   )
# }

sulfatesServer <- function(id, data, numericInput_ids){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data())
      
      data <- data()
      
      values <- numericInput_ids()
      
      lakes_sulfates_prep <- data %>% 
        group_by(mlid, start_date) %>% 
        filter(all(!is.na(hardness))) %>% 
        filter(!all(if_any(analyte, ~ . != "CHLORIDE")), # Keep sites only if chloride has been measured
               all(ifelse(analyte == "CHLORIDE", !is.na(result_value), T)),
               all(ifelse(analyte == "SULFATE", !is.na(result_value), T))) %>% 
        filter(analyte == "CHLORIDE") %>% 
        mutate(hardness = as.numeric(hardness),
               result_value = as.numeric(result_value),
               acute_standard = case_when(analyte == "CHLORIDE" & 
                                            hardness >= values[["sulfate1_hardness1"]] &
                                            hardness <= values[["sulfate1_hardness2"]] &
                                            result_value >= values[["sulfate1_chloride1"]] &
                                            result_value <= values[["sulfate1_chloride2"]] 
                                          ~ (values[["sulfate_acute_param1"]] + values[["sulfate_acute_param2"]]*hardness - 
                                               values[["sulfate_acute_param3"]]*result_value)*values[["sulfate_acute_param4"]],
                                          analyte == "CHLORIDE" &
                                            hardness >= values[["sulfate2_hardness1"]] &
                                            hardness <= values[["sulfate2_hardness2"]] &
                                            result_value >= values[["sulfate2_chloride1"]] &
                                            result_value <= values[["sulfate2_chloride2"]]
                                          ~ (values[["sulfate_acute_param5"]] + values[["sulfate_acute_param6"]]*hardness + 
                                               values[["sulfate_acute_param8"]]*result_value)*values[["sulfate_acute_param8"]],
                                          analyte == "CHLORIDE" &
                                            hardness < values[["sulfate3_hardness"]] | result_value < values[["sulfate3_chloride"]] ~ 
                                            values[["sulfate_constant1"]],
                                          analyte == "CHLORIDE" &
                                            hardness > values[["sulfate4_hardness"]] & result_value >= values[["sulfate4_chloride"]] ~ 
                                            values[["sulfate_constant2"]],
                                          TRUE ~ NA
                                            )) %>% 
        select(mlid, start_date, acute_standard)
      
      lakes_sulfates <- data %>% 
        left_join(lakes_sulfates_prep, by = c("mlid", "start_date")) %>% 
        filter(analyte == "SULFATE") %>% 
        mutate(hardness = as.numeric(hardness),
               acute_standard = as.numeric(acute_standard),
               result_value = as.numeric(result_value),
               quotient = result_value/acute_standard) %>% 
        mutate(standard_violation = ifelse(quotient > 1, "EXCEEDANCE", "NONE")) %>% 
        relocate(c(acute_standard, result_value, quotient, standard_violation), .before = 1)
      
      lakes_sulfates
        
    })
    
    # output$sulfates_debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    # 
    # output$sulfates_debugging_text <- renderPrint({
    #   values <- numericInput_ids()
    #   
    #   cat(values[["sulfate4_chloride"]], "\n")
    # })
    
    data_reactive_internal
    
  })

}