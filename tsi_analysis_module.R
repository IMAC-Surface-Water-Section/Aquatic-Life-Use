# tsiAnalysisDebuggingUI <- function(id){
#   ns <- NS(id)
# 
#   tagList(
#     reactableOutput(ns("tsi_debugging_table"))
#   )
# }

tsiAnalysisServer <- function(id, data, switch_state){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data())
      
      switch_state <- switch_state()
      
      req(switch_state == "Yes")
      
      data <- data()
      
      analytes_for_tsi <- data %>%
        group_by(AUID, analyte, collection_year) %>%
        mutate(median_analyte = median(as.numeric(result_value)),
               mean_coverage = mean(as.numeric(coverage))) %>%
        filter(min_site_reqs == "met") %>%
        select(AUID, analyte, collection_year, median_analyte, mean_coverage) %>%
        distinct() %>% 
        mutate(tsi_weight = case_when(analyte == "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN" ~ 9.81*log(median_analyte)+30.6,
                                      analyte == "DEPTH, SECCHI DISK DEPTH" ~ 60-(14.41*log(median_analyte)),
                                      analyte == "PHOSPHORUS" ~ 14.42*log(median_analyte*1000)+4.15),
               ) %>%
        mutate(NVSS_points = case_when(analyte == "NVSS" & median_analyte < 12 ~ 0,
                                       analyte == "NVSS" & median_analyte >= 12 & median_analyte < 15 ~ 5,
                                       analyte == "NVSS" & median_analyte >= 15 & median_analyte < 20 ~ 10,
                                       analyte == "NVSS" & median_analyte >= 20 ~ 15)) %>%
        mutate(macrophyte_points = case_when(mean_coverage >= 15 & mean_coverage < 40 ~ 0,
                                             mean_coverage >= 10 & mean_coverage < 15 ~ 5,
                                             mean_coverage >= 40 & mean_coverage < 50 ~ 5,
                                             mean_coverage >= 5 & mean_coverage < 10 ~ 10,
                                             mean_coverage >= 50 & mean_coverage < 70 ~ 10,
                                             mean_coverage < 5 ~ 15,
                                             mean_coverage >= 70 ~ 15)) %>%
        group_by(AUID, collection_year) %>%
        fill(NVSS_points, .direction = "downup") %>%
        mutate(NVSS_usable = ifelse(is.na(NVSS_points), "no", "yes"),
               mac_usable = ifelse(is.na(macrophyte_points), "no", "yes"),
               NVSS_points = ifelse(is.na(NVSS_points), 0, NVSS_points),
               macrophyte_points = ifelse(is.na(macrophyte_points), 0, macrophyte_points)) %>%
        group_by(AUID, analyte, collection_year) %>%
        mutate(secchi_points = case_when(analyte == "DEPTH, SECCHI DISK DEPTH" ~
                                           case_when(tsi_weight < 60 ~ 40 + NVSS_points + macrophyte_points,
                                                     tsi_weight >= 60 & tsi_weight < 85 ~ 50 + NVSS_points + macrophyte_points,
                                                     tsi_weight >= 85 & tsi_weight < 90 ~ 60 + NVSS_points + macrophyte_points,
                                                     tsi_weight >= 90 ~ 70 + NVSS_points + macrophyte_points))) %>%
        group_by(AUID, collection_year) %>%
        fill(secchi_points, .direction = "downup") %>%
        group_by(AUID, analyte, collection_year) %>%
        mutate(chlorophyll_points = case_when(analyte == "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN" ~
                                                case_when(tsi_weight < 60 ~ 40 + NVSS_points + macrophyte_points,
                                                          tsi_weight >= 60 & tsi_weight < 85 ~ 50 + NVSS_points + macrophyte_points,
                                                          tsi_weight >= 85 & tsi_weight < 90 ~ 60 + NVSS_points + macrophyte_points,
                                                          tsi_weight >= 90 ~ 70 + NVSS_points + macrophyte_points))) %>%
        group_by(AUID, collection_year) %>%
        fill(chlorophyll_points, .direction = "downup") %>%
        group_by(AUID, analyte, collection_year) %>%
        mutate(phosphorus_points = case_when(analyte == "PHOSPHORUS" ~
                                               case_when(tsi_weight < 60 ~ 40 + NVSS_points + macrophyte_points,
                                                         tsi_weight >= 60 & tsi_weight < 85 ~ 50 + NVSS_points + macrophyte_points,
                                                         tsi_weight >= 85 & tsi_weight < 90 ~ 60 + NVSS_points + macrophyte_points,
                                                         tsi_weight >= 90 ~ 70 + NVSS_points + macrophyte_points))) %>%
        group_by(AUID, collection_year) %>%
        fill(phosphorus_points, .direction = "downup") %>%
        mutate(secchi_use = case_when(!is.na(secchi_points) & mac_usable == "yes" ~ ifelse(secchi_points < 75, "FULLY SUPPORTING", "NOT SUPPORTING"),
                                      !is.na(secchi_points) & NVSS_usable == "yes" ~ ifelse(secchi_points < 75, "FULLY SUPPORTING", "NOT SUPPORTING"),
                                      mac_usable == "no" & NVSS_usable == "no" ~ "NOT ASSESSED",
                                      is.na(secchi_points) ~ "NOT ASSESSED")) %>%
        mutate(chlorophyll_use = case_when(!is.na(chlorophyll_points) & mac_usable == "yes" ~
                                             ifelse(chlorophyll_points < 75, "FULLY SUPPORTING", "NOT SUPPORTING"),
                                           !is.na(chlorophyll_points) & NVSS_usable == "yes" ~
                                             ifelse(chlorophyll_points < 75, "FULLY SUPPORTING", "NOT SUPPORTING"),
                                           mac_usable == "no" & NVSS_usable == "no" ~ "NOT ASSESSED",
                                           is.na(chlorophyll_points) ~ "NOT ASSESSED")) %>%
        mutate(phosphorus_use = case_when(!is.na(phosphorus_points) & mac_usable == "yes" ~
                                            ifelse(phosphorus_points < 75, "FULLY SUPPORTING", "NOT SUPPORTING"),
                                          !is.na(phosphorus_points) & NVSS_usable == "yes" ~
                                            ifelse(phosphorus_points < 75, "FULLY SUPPORTING", "NOT SUPPORTING"),
                                          mac_usable == "no" & NVSS_usable == "no" ~ "NOT ASSESSED",
                                          is.na(phosphorus_points) ~ "NOT ASSESSED")) %>%
        mutate(ALU_use = case_when(secchi_use == chlorophyll_use ~ secchi_use,
                                   secchi_use == phosphorus_use ~ secchi_use,
                                   chlorophyll_use == phosphorus_use ~ chlorophyll_use),
               ALU_use = ifelse(is.na(ALU_use), "NEEDS REVIEW", ALU_use))

      analytes_for_tsi
    })

    # output$tsi_debugging_table <- renderReactable({
    #     req(data_reactive_internal())
    #     reactable(data_reactive_internal(),
    #               resizable = TRUE,
    #               searchable = TRUE,
    #               filterable = TRUE)
    #   })
    
    data_reactive_internal
    
  })
}