# tsiSiteReqDebuggingUI <- function(id){
#   ns <- NS(id)
#   
#   tagList(
#     reactableOutput(ns("debugging_table"))
#   )
# }

tsiSiteReqServer <- function(id, data, thermocline_data, macrophyte_data, macrophyte_columns, switch_state){
  moduleServer(id, function(input, output, session) {
    
    data_reactive_internal <- reactive({
      req(data(), macrophyte_data(), macrophyte_columns())
      
      switch_state <- switch_state()
      
      req(switch_state == "Yes")
      
      data <- data()
      thermocline_data <- thermocline_data()
      macrophyte_data <- macrophyte_data()
      selected <- macrophyte_columns()
      
      macrophyte_data_for_join <- macrophyte_data %>% 
        select(all_of(selected)) %>% 
        mutate(AUID = ifelse(str_detect(AUID, "IL_"), AUID, str_c("IL_", AUID))) %>% 
        rename(start_date = collection_date_id) %>% 
        mutate(start_date = convert_date(start_date))
      
      months <- c("april", "may", "june", "july", "august", "september", "october")
      
      # Helper function to determine if data are available from at least half of the sites sampled within any given lake each visit
      check_use <- function(sites_value, total_sites) {
        ifelse(sites_value == 0, 0,
               ifelse(sites_value > 0 & sites_value/total_sites >= 0.5, 1, 0))
      }
      
      # Following "Figure C-1. Flow Chart to Assess Aquatic Life Use in Lakes" from 2024 Assessment Methodology
      tsi_analytes <- data %>% 
        # Water Quality Parameters: 
          # total phosphorus, chlorophyll a, and Secchi disk transparency
        left_join(macrophyte_data_for_join) %>% # plus we need macrophyte data
        filter(case_when(analyte == "PHOSPHORUS" & sample_fraction == "Total" ~ T,
                         analyte %in% c("TOTAL SUSPENDED SOLIDS", "VOLATILE SUSPENDED SOLIDS") ~ T, # and TSS/VSS for NVSS
                         analyte == "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN" ~ T,
                         analyte == "DEPTH, SECCHI DISK DEPTH" ~ T)) %>% 
        filter(!is.na(result_value)) %>% 
        left_join(thermocline_data) %>% 
        # Data must be above thermocline for phosphorus and TSS/VSS
        filter(ifelse(!is.na(thermocline_depth) & analyte != "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN", 
                      sample_depth <= thermocline_depth, T)) %>% 
        group_by(mlid, start_date, sample_depth) %>% 
        # If either TSS or VSS isn't present in the group, keep neither, otherwise keep both
        filter(!(analyte %in% c("TOTAL SUSPENDED SOLIDS", "VOLATILE SUSPENDED SOLIDS")) | 
                 (sum(analyte == "TOTAL SUSPENDED SOLIDS") > 0 &
                    sum(analyte == "VOLATILE SUSPENDED SOLIDS") > 0)) %>% 
        mutate(result_value = as.numeric(result_value),
               TSS = ifelse(analyte == "TOTAL SUSPENDED SOLIDS", result_value, NA),
               VSS = ifelse(analyte == "VOLATILE SUSPENDED SOLIDS", result_value, NA)) %>% 
        fill(TSS, VSS, .direction = "downup") %>% 
        mutate(NVSS = TSS-VSS,
               # When both samples are ND, a negative NVSS value is possible due to MDL values; also, logically VSS can't be > TSS
               NVSS = ifelse(NVSS < 0, 0, NVSS), 
               analyte = ifelse(analyte %in% c("TOTAL SUSPENDED SOLIDS", "VOLATILE SUSPENDED SOLIDS"), "NVSS", analyte),
               result_value = case_when(analyte == "NVSS" ~ NVSS,
                                        analyte == "DEPTH, SECCHI DISK DEPTH" ~ result_value/39.3700787, # Convert Secchi from inches to meters
                                        T ~ result_value)) %>%
        select(-c(qualifiers, limit_type_1, limit_value_1, limit_unit_1, limit_type_2, limit_value_2, limit_unit_2, data_usability)) %>% 
        distinct()
      
      # Check if data meet minimum site requirements
      tsi_site_min_reqs <- tsi_analytes %>%
        mutate(collection_month = as.numeric(month(start_date)),
               collection_year = year(start_date)) %>%
        group_by(AUID, analyte, collection_year) %>%
        mutate(n_collection_months = n_distinct(collection_month), # number of months in which each analyte was collected
               # Determine number of peak growing season months (June-August)
               n_peak_season_months = str_count(paste(unique(collection_month), collapse = ", "), regex("6|7|8")), 
               n_sites = length(unique(mlid)),
               use_peak_season = ifelse(n_peak_season_months >= 2, "yes", "no")) %>%
        # Determine number of months data was collected from April through October
        mutate(
          list_cbind(map2(
            4:10, c("n_sites_april", "n_sites_may", "n_sites_june", "n_sites_july", "n_sites_august", "n_sites_september", "n_sites_october"),
            ~setNames(tibble(length(unique(mlid[collection_month == .x]))), .y)
          ))) %>%
        # Check if at least half of sites per lake per visit have usable data per month, and create columns indicating yes (1) or no (0)
        mutate(across(paste0("n_sites_", months),
                      ~ check_use(.x, n_sites),
                      .names = "use_{.col}")) %>%
        rename_with(~ gsub("n_sites_", "", .), starts_with("use_n_sites_")) %>%
        rowwise() %>%
        # Get total number of usable months of data from April through October
        mutate(n_use_months = sum(use_april, use_may, use_june, use_july, use_august, use_september, use_october)) %>%
        ungroup() %>% 
        mutate(use_n_months = ifelse(n_use_months >= 4, "yes", "no"),
               min_site_reqs = case_when(use_peak_season == "yes" & use_n_months == "yes" ~ "met",
                                         use_peak_season == "yes" & use_n_months == "no" ~ "unmet; < 4 months available",
                                         use_peak_season == "no" & use_n_months == "yes" ~ "unmet; < 2 months peak season",
                                         use_peak_season == "no" & use_n_months == "no" ~ "unmet; < 4 months available; < 2 months peak season")) %>% 
        select(-matches("(^use_[[:alpha:]]+$)|(n_sites.*$)"))
      
      tsi_param_min_reqs <- tsi_site_min_reqs %>% 
        filter(min_site_reqs == "met",
               analyte %in% c("PHOSPHORUS", "NVSS", "CHLOROPHYLL A, CORRECTED FOR PHEOPHYTIN")) %>% 
        group_by(AUID, collection_year) %>% 
        mutate(use_param = ifelse(length(unique(analyte)) >= 2, "yes", "no")) %>% 
        right_join(tsi_site_min_reqs)
      
      tsi_param_min_reqs
    })
    
    # output$debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    
    data_reactive_internal
  })
}