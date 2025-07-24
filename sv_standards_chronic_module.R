# chronicDebuggingUI <- function(id){
#   ns <- NS(id)
# 
#   tagList(
#     reactableOutput(ns("debugging_table"))
#   )
# }

chronicDebuggingTextUI <- function(id){
  ns <- NS(id)

  tagList(
    verbatimTextOutput(ns("chronic_debugging_text"))
  )
}

lakesChronicServer <- function(id, data_input, hds_input, standards_input, thermocline_data){
  moduleServer(id, function(input, output, session){
    data_reactive_internal <- reactive({
      req(data_input)
      req(hds_input)
      req(standards_input)
      req(thermocline_data)
      
      data <- data_input() %>% 
        mutate(result_value = as.numeric(result_value),
               hardness = as.numeric(hardness))
      hds_data <- hds_input()
      standards_data <- standards_input()
      thermocline_data <- thermocline_data()
      
      combined_data <- data %>% full_join(hds_data) %>% 
        rename(hd_chronic_standard = chronic_standard) %>% 
        select(-c(acute_standard, quotient, standard_violation))
      
      chronic_standards_for_join <- standards_data %>%
        select(Analyte, sample_fraction, chronic_standard, chronic_units) %>%
        rename(analyte = Analyte)

      test_csfj <- chronic_standards_for_join %>%
        select(analyte, sample_fraction, chronic_standard, chronic_units) %>%
        rename(standards_fraction = sample_fraction)

      lakes_chronic <- combined_data %>%
        left_join(test_csfj)  %>%
        mutate(standards_fraction = ifelse(is.na(standards_fraction), "NA_", standards_fraction)) %>%
        mutate(fraction_match = ifelse(str_detect(sample_fraction, standards_fraction), "YES", "NO")) %>%
        filter(case_when(!is.na(hd_chronic_standard) ~ T,
                         is.na(fraction_match) & !is.na(chronic_standard) ~ T,
                         fraction_match == "NO" & standards_fraction == "NA_" & !is.na(chronic_standard) ~ T,
                         fraction_match == "NO" & standards_fraction != "NA_" ~ F,
                         fraction_match == "YES" & !is.na(chronic_standard) ~ T)) %>%
        mutate(chronic_standard = ifelse(!is.na(hd_chronic_standard) & is.na(chronic_standard), hd_chronic_standard, chronic_standard)) %>%
        mutate(result_value = as.numeric(result_value),
               chronic_standard = as.numeric(chronic_standard),
               quotient = result_value/chronic_standard)

      lakes_chronic_filtered <- lakes_chronic %>%
        select(AUID, mlid, start_date, analyte, sample_fraction, sample_depth) %>%
        mutate(depth_narrative = ifelse(sample_depth <= 2, "Shallow", "Deep")) %>%
        group_by(mlid, analyte, sample_fraction, start_date, depth_narrative) %>%
        distinct() %>%
        group_by(mlid, analyte, sample_fraction, depth_narrative) %>%
        add_count(name = "times_sampled") %>%
        filter(times_sampled < 3)

      lakes_data_below_thermocline <- combined_data %>%
        left_join(thermocline_data, by = c("mlid", "start_date")) %>%
        filter(sample_depth >= thermocline_depth) %>%
        mutate(below_thermocline = "yes",
               result_value = as.numeric(result_value),
               hardness = as.numeric(hardness))

      lakes_chronic_consecutive <- anti_join(lakes_chronic, lakes_chronic_filtered) %>%
        mutate(hardness = as.numeric(hardness)) %>%
        left_join(lakes_data_below_thermocline) %>%
        filter(!str_detect(result_unit, "kg"),
               is.na(below_thermocline)) %>%
        select(-below_thermocline) %>%
        mutate(numeric_date = as.numeric(as.Date(start_date)),
               sample_depth = round(sample_depth, 0),
               depth_narrative = ifelse(sample_depth < 2, "Shallow", "Deep"),
               quotient = as.numeric(quotient)) %>%
        arrange(mlid, analyte, start_date) %>%
        group_by(mlid, analyte, depth_narrative, sample_fraction) %>%
        arrange(depth_narrative, .by_group = T) %>%
        mutate(chronic_start_date = ifelse(quotient > 1 & lag(quotient) < 1,
                                         ((lag(numeric_date)*(1-quotient))+numeric_date*(lag(quotient)-1))/
                                           (lag(quotient)-quotient), NA)
        ) %>%
        mutate(chronic_end_date = ifelse(quotient < 1 & lag(quotient) > 1,
                                       ((lag(numeric_date)*(1-quotient))+numeric_date*(lag(quotient)-1))/
                                         (lag(quotient)-quotient), NA)
        ) %>%
        mutate(chronic_end_date = as.numeric(chronic_end_date),
               chronic_start_date = as.numeric(chronic_start_date)) %>%
        # With the linear model (new method), if the quotient of the previous data point is greater than the quotient of the data point in question, the chronic start date will be calculated as being AFTER the date of the data point in question. We logically know, however, that the start date must be at the latest, the same as the collection date of the data point.
        mutate(chronic_start_date = ifelse(chronic_start_date > numeric_date, numeric_date, chronic_start_date),
               chronic_event_duration = ifelse(lead(chronic_end_date) > chronic_start_date, lead(chronic_end_date)-chronic_start_date, NA),
               chronic_event_duration = round(chronic_event_duration, 1),
               standard_violation = ifelse(quotient >1 & chronic_event_duration >= 4, "EXCEEDANCE", "NONE"),
               standard_violation = ifelse(is.na(standard_violation) & all(quotient > 1) &
                                             (max(numeric_date)-min(numeric_date)) >= 4,
                                           "EXCEEDANCE", standard_violation)) %>%
        ungroup()
      
      lakes_chronic_consecutive
            
    })
    
    # output$debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    
    output$chronic_debugging_text <- renderPrint({
      head(standards_input())
    })
    
    data_reactive_internal
    
  })
}