# svStandardsUI <- function(id){
#   ns <- NS(id)
#   
#   tagList(
#     reactableOutput(ns("sv_debugging_table"))
#   )
# }

lakesAcuteServer <- function(id, data_input, las_input){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data_input())
      req(las_input())
      
      acute_standards_input <- data_input() %>% 
        rename(analyte = Analyte)
      
      data <- las_input()
      
      # Some analyte standards don't specify a sample fraction (i.e. total; dissolved) that they apply to
      # Data in AWQMS sometimes has a sample fraction listed for these analytes
      # I therefore need to separately join standards with and without fractions so every analyte is properly assessed
      
      acute_standards_for_join1 <- acute_standards_input %>% 
        filter(!is.na(sample_fraction)) %>% 
        select(analyte, sample_fraction, acute_standard, acute_units)
      
      acute_standards_for_join2 <- acute_standards_input %>% 
        filter(is.na(sample_fraction)) %>% 
        select(analyte, acute_standard, acute_units)
      
      acute_standards_for_join <- list(acute_standards_for_join1, acute_standards_for_join2)
      
      for(i in 1:length(acute_standards_for_join)){
        temp_df <- data %>% 
          left_join(acute_standards_for_join[[i]]) %>% 
          filter(!is.na(acute_standard)) %>% 
          mutate(hardness = as.numeric(hardness),
                 result_value = as.numeric(result_value),
                 acute_standard = as.numeric(acute_standard),
                 quotient = result_value/acute_standard,
                 standard_violation = ifelse(quotient > 1, "EXCEEDANCE", "NONE"))
        
        if(i == 1){
          lakes_acute <- temp_df
        } else {
          lakes_acute <- rbind(lakes_acute, temp_df)
        }
      }
      
      lakes_acute
        
    })
    
    # output$sv_debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    
    data_reactive_internal
    
  })
}