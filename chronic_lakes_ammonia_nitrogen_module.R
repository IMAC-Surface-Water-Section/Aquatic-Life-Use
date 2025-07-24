# canDebuggingUI <- function(id){
#   ns <- NS(id)
# 
#   tagList(
#     reactableOutput(ns("can_debugging_table"))
#   )
# }
# 
# canDebuggingText <- function(id){
#   ns <- NS(id)
#   
#   tagList(
#     verbatimTextOutput(ns("can_debugging_text"))
#   )
# }

chronicAmmoniaNitrogenServer <- function(id, data_input, thermocline_data, numericInput_ids){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data_input)
      req(thermocline_data)
      
      values <- numericInput_ids()
      
      data <- data_input() %>% 
        mutate(result_value = as.numeric(result_value),
               hardness = as.numeric(hardness))
      thermocline_data <- thermocline_data()
      
      lakes_ammonia_filtered <- data %>%
        filter(analyte == "AMMONIA-NITROGEN") %>% 
        select(AUID, mlid, start_date, analyte, sample_fraction, sample_depth) %>%
        mutate(depth_narrative = ifelse(sample_depth <= 2, "Shallow", "Deep")) %>%
        group_by(mlid, analyte, sample_fraction, start_date, depth_narrative) %>%
        distinct() %>%
        group_by(mlid, analyte, sample_fraction, depth_narrative) %>%
        add_count(name = "times_sampled") %>%
        filter(times_sampled < 3) %>% 
        ungroup()
      
      lakes_ammonia_chronic <- data %>%
        left_join(thermocline_data, by = c("mlid", "start_date")) %>%
        filter(sample_depth < thermocline_depth) %>%
        mutate(rounded_depth = round(sample_depth, 0)) %>% 
        mutate(depth_narrative = ifelse(sample_depth <= 2, "Shallow", "Deep")) %>% 
        filter(analyte %in% c("AMMONIA-NITROGEN", "PH", "TEMPERATURE, WATER")) %>% 
        anti_join(lakes_ammonia_filtered, by = c("mlid", "depth_narrative")) %>% 
        group_by(mlid, start_date) %>% 
        mutate(pH = ifelse(analyte == "PH", result_value, NA),
               temperature = ifelse(analyte == "TEMPERATURE, WATER", result_value, NA)) %>% 
        fill(pH, temperature, .direction = "downup") %>% 
        mutate(pH = as.numeric(pH),
               temperature = as.numeric(temperature)) %>% 
        filter(!if_any(c(pH, temperature, result_value), is.na),
               analyte == "AMMONIA-NITROGEN") %>% 
        ungroup() %>% 
        mutate(numeric_date = as.numeric(start_date),
               numeric_month = month(start_date)) %>% 
        mutate(early_life_present = ifelse(numeric_month %in% c(3, 4, 5, 6, 7, 8, 9, 10), "Present", "Absent")) %>% 
        mutate(standard = case_when(early_life_present == "Present" & temperature <= values[["total_an_temp1"]] ~
                                      ((values[["an_param1"]]/(1+10^(values[["an_param2"]] - pH))) + (values[["an_param3"]]/(1+10^(pH - values[["an_param2"]]))))*values[["an_param4"]],
                                    early_life_present == "Present" & temperature > values[["total_an_temp1"]] ~
                                      ((values[["an_param5"]]/(1+10^(values[["an_param2"]] - pH))) + (values[["an_param6"]]/(1+10^(pH - values[["an_param2"]]))))*(values[["an_param7"]]*10^(0.028*(25 - temperature))),
                                    early_life_present == "Absent" & temperature <= values[["total_an_temp3"]] ~ 
                                      ((values[["an_param8"]]/(1+10^(values[["an_param2"]] - pH))) + (values[["an_param9"]]/(1+10^(pH - values[["an_param2"]]))))*(values[["an_param10"]]*10^0.504),
                                    early_life_present == "Absent" & temperature > values[["total_an_temp3"]] ~
                                      ((values[["an_param11"]]/(1+10^(values[["an_param2"]] - pH))) + (values[["an_param12"]]/(1+10^(pH - values[["an_param2"]]))))*(values[["an_param13"]]*10^(0.028*(25 - temperature)))
                                    )) %>% 
        mutate(quotient_chronic = result_value/standard,
               standard_subchronic = standard*2.5,
               quotient_subchronic = result_value/standard_subchronic) %>% 
        arrange(mlid, analyte, start_date) %>% 
        group_by(mlid, depth_narrative) %>% 
        arrange(depth_narrative, .by_group = T) %>% 
        mutate(chronic_start_date = ifelse(quotient_chronic > 1 & lag(quotient_chronic) < 1,
                                           ((lag(numeric_date)*(1-quotient_chronic)) + numeric_date*(lag(quotient_chronic)-1)) / 
                                             (lag(quotient_chronic)-quotient_chronic), NA)) %>% 
        mutate(chronic_end_date = ifelse(quotient_chronic < 1 & lag(quotient_chronic) > 1,
                                         ((lag(numeric_date)*(1-quotient_chronic))+numeric_date*(lag(quotient_chronic)-1)) / 
                                           (lag(quotient_chronic)-quotient_chronic), NA)) %>% 
        mutate(chronic_start_date = ifelse(chronic_start_date > numeric_date, numeric_date, chronic_start_date),
               chronic_event_duration = ifelse(lead(chronic_end_date) > chronic_start_date, lead(chronic_end_date) - chronic_start_date, NA)) %>% 
        mutate(chronic_event_duration = round(chronic_event_duration, 1),
               standard_violation = ifelse(quotient_chronic > 1 & chronic_event_duration >= 30, "EXCEEDANCE", "NONE")) %>% 
        mutate(standard_violation = ifelse(is.na(standard_violation) & all(quotient_chronic > 1) & (max(numeric_date)-min(numeric_date)) >= 30,
                                           "EXCEEDANCE", standard_violation),
               subchronic_start_date = ifelse(quotient_subchronic > 1 & lag(quotient_subchronic) < 1,
                                              ((lag(numeric_date)*(1-quotient_subchronic))+numeric_date*(lag(quotient_subchronic)-1))/
                                                (lag(quotient_subchronic)-quotient_subchronic), NA)) %>% 
        mutate(subchronic_end_date = ifelse(quotient_subchronic < 1 & lag(quotient_subchronic) > 1,
                                            ((lag(numeric_date)*(1-quotient_subchronic))+numeric_date*(lag(quotient_subchronic)-1))/
                                              (lag(quotient_subchronic)-quotient_subchronic), NA)) %>% 
        mutate(subchronic_start_date = ifelse(subchronic_start_date > numeric_date, numeric_date, subchronic_start_date),
               subchronic_event_duration = ifelse(lead(subchronic_end_date) > subchronic_start_date, 
                                                  lead(subchronic_end_date)-subchronic_start_date, NA)) %>% 
        mutate(subchronic_event_duration = round(subchronic_event_duration, 1)) %>% 
        mutate(standard_violation_subchronic = ifelse(quotient_subchronic > 1 & subchronic_event_duration >= 4, "EXCEEDANCE", "NONE")) %>% 
        mutate(standard_violation_subchronic = ifelse(is.na(standard_violation_subchronic) & all(quotient_subchronic > 1) & 
                                                        (max(numeric_date)-min(numeric_date)) >= 4, "EXCEEDANCE", standard_violation_subchronic)) %>% 
        ungroup()
      
      df1 <- lakes_ammonia_chronic %>% 
        arrange(mlid, analyte, start_date) %>% 
        group_by(mlid, depth_narrative) %>% 
        arrange(depth_narrative, .by_group = T) %>% 
        slice_tail() %>% 
        filter(quotient_chronic > 1,
               quotient_subchronic > 1) %>% 
        mutate(last_exceedance = "No following data")
      
      df2 <- lakes_ammonia_chronic %>% 
        arrange(mlid, analyte, start_date) %>% 
        group_by(mlid, depth_narrative) %>% 
        arrange(depth_narrative, .by_group = T) %>% 
        slice_head() %>% 
        filter(quotient_chronic > 1,
               quotient_subchronic > 1) %>% 
        mutate(first_exceedance = "No prior data")
      
      df3 <- rbind(df1, df2)
      
      if(nrow(df3) == 0){
        df4 <- lakes_ammonia_chronic %>% 
          mutate(first_exceedance = "NA",
                 last_exceedance = "NA")
      } else if(nrow(df1) != 0 & nrow(df2) == 0) {
        df4 <- df3 %>% 
          mutate(last_exceedance = "NA")
      } else {
        df4 <- lakes_ammonia_chronic %>% 
          left_join(df3)
      }
        
      df4
      
    })
    
    # output$can_debugging_text <- renderPrint({
    #     values <- numericInput_ids()
    # 
    #     cat(values[["an_param1"]], "\n")
    #   })
    # 
    # output$can_debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    
    data_reactive_internal
    
  })
}