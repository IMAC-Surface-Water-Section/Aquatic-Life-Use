# thermoclineDebuggingUI <- function(id){
#   ns <- NS(id)
#   
#   tagList(
#     reactableOutput(ns("debugging_table"))
#   )
# }

lakesThermoclineServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data())
      
      data <- data()
      
      lakes_thermocline_filter_info <- data %>% 
        filter(analyte == "TEMPERATURE, WATER") %>% 
        select(start_date, start_time, mlid, result_value, result_unit, sample_depth, depth_unit) %>% 
        group_by(mlid, start_date, sample_depth, start_time) %>% 
        arrange(mlid, start_date, sample_depth, desc(start_time)) %>% 
        slice_head() %>% 
        ungroup() %>% 
        select(-start_time) %>% 
        mutate(result_value = as.numeric(result_value)) %>% 
        mutate(temp_change = ifelse(mlid == lag(mlid) 
                                    & start_date == lag(start_date),
                                    (result_value - lag(result_value))/(sample_depth - lag(sample_depth)), NA)) %>%
        mutate(greaterthan1degree_change = ifelse(abs(temp_change) > 0.5, "yes", "no")) %>% 
        filter(greaterthan1degree_change == "yes") %>%
        group_by(mlid, start_date) %>%
        arrange(mlid, start_date, sample_depth) %>% # Arrange by station code, collection date, and depth (shallowest to deepest)
        slice_head() %>% # Retain only first record (shallowest depth)
        ungroup() %>%
        select(mlid, start_date, sample_depth) %>%
        rename(thermocline_depth = sample_depth) 
      
      lakes_thermocline_filter_info
    })
    # 
    # output$debugging_table <- renderReactable({
    #     req(data_reactive_internal())
    #     reactable(data_reactive_internal(),
    #               resizable = TRUE,
    #               searchable = TRUE)
    #   })
    
    data_reactive_internal
      
  })
}