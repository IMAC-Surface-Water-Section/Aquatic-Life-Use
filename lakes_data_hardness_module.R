# Read in file with MLIDs and their associated AUID
suppressWarnings(
  MLIDs_AUIDs <- read_xls("N:/Spiusers1/Yaal.Dryer/R Stuff/PFPWS/Latest_stationCodes_for2024 3.xls",
                                         sheet = "EPA_GISVECTOR.EPA_GIS.bowswsSta") %>%
                   select(StationCode, AUID) %>%
                   rename(MLID = StationCode)
  )

lakesHardnessServer <- function(id, selected_columns, dataset, trigger){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- eventReactive(trigger(), {
      
      data <- dataset()
      selected <- selected_columns()
      
      validate(
        need(length(selected) > 0, "No columns selected. Please select columns in Step 2 of the Assessment Tool")
      )
      
      # Due to the way select() handles named vectors, the colnames for `lakes_data` will be the *names* of `selected_columns` instead of the *values*
      # Alternatively, using select(all_of(unname(selected))) would result in the values of `selected` being used for the colnames of lakes_data.
      # However, this would subsequently require a different approach to referring to the columns (i.e. !!sym(selected[['colname']])), which is more verbose
      # and less clear/straight forward. The colnames for lakes_data are therefore ultimately the `inputId`s in selectize_lists
      lakes_data <- data %>%
        select(all_of(selected)) %>% 
        left_join(MLIDs_AUIDs, join_by(mlid == "MLID")) %>% 
        mutate(AUID = case_when(mlid == "RGF-2" ~ "IL_RGF",
                                mlid == "WTP-1" ~ "IL_WTP",
                                mlid == "RHL" ~ "IL_RHL",
                                mlid == "RHZH" ~ "IL_RHZH",
                                mlid == "RHZG" ~ "IL_RHZG",
                                mlid == "RGK" ~ "IL_RGK",
                                mlid == "RTZJ" ~ "IL_RTZJ",
                                mlid == "RGZO" ~ "IL_RGZO",
                                mlid == "RDZP" ~ "IL_RDZP",
                                mlid == "RDZF" ~ "IL_RDZF",
                                mlid == "WDW" ~ "IL_WDW",
                                !is.na(AUID) ~ AUID)) %>% 
        filter(!str_detect(result_unit, ".*/kg$")) %>% # Filter out results that are x per kilogram
        # Trip ID and qualifiers are sometimes stored together in this column. The Trip ID is separated from the qualifiers with a pipe (|). 
        # This removes everything except the qualifiers.
        # CHANGE THIS FOR 2028; Trip ID and qualifiers are no longer stored in the same column
        mutate(qualifiers = ifelse(str_detect(qualifiers, regex("Trip ID", TRUE)), 
                                   str_remove(qualifiers, regex("Trip ID:.+\\|[:space:]*", TRUE)),
                                   qualifiers),
               start_date = convert_date(start_date),
               start_time = convert_hms(start_time)) %>% 
        # Remove rows with data that has a qualifier that makes it unusable
        mutate(data_usability = case_when(is.na(qualifiers) | grepl("[SC]", qualifiers) ~ "Usable",
                                         str_detect(qualifiers, "A|B|B1|B2|J(?![:digit:])|E|I|J5|J6|J7|K|L|L1|L2|M|N|TNTC|ND") &
                                           !str_detect(qualifiers, "J1|J2|J3|J4|J9|Q|V|X|W|Y") ~ "Conditional Use",
                                         grepl("J1|J2|J3|J4|J9|Q|V|X|W|Y", qualifiers) ~ "Not Usable",
                                         TRUE ~ NA_character_)) %>%
        filter(data_usability != "Not Usable") %>% 
        # If qualifier is ND, use 1/2 the Lower Reporting Limit of the analytical method
        mutate(result_value = ifelse(is.na(result_value) & str_detect(qualifiers, "ND") & limit_type_1 == "Method Detection Level",
                                     limit_value_1 * 0.5, result_value))
      
      req(lakes_data) # Require `lakes_data` before moving on with analysis

      lakes_data_hardness <- lakes_data %>%
        mutate(analyte = toupper(analyte)) %>% 
        group_by(mlid, start_date) %>% 
        mutate(hardness = case_when(analyte == "ALKALINITY, TOTAL" ~ as.character(result_value),
                                    analyte == "ALKALINITY, TOTAL" & is.na(result_value) ~ "not measured")) %>% 
        fill(hardness, .direction = "downup") %>% 
        ungroup()
      
      lakes_data_hardness
    })
    
    data_reactive_internal
    
  })
}