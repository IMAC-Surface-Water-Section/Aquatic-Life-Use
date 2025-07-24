# depDebuggingUI <- function(id) {
#   ns <- NS(id)
# 
#   tagList(reactableOutput(ns("dep_debugging_table")))
# }
# 
# debuggingTextUI <- function(id){
#   ns <- NS(id)
# 
#   tagList(
#     h4("Value from numericInput:"),
#     verbatimTextOutput(ns("debugging_text"))
#   )
# }

dependentServer <- function(id, data, numericInput_ids){
  moduleServer(id, function(input, output, session){
    
    data_reactive_internal <- reactive({
      req(data())
      
      data <- data()
      
      values <- numericInput_ids()
      
      filter_vec <- c("CADMIUM" = "Dissolved", "COPPER" = "Dissolved", "CHROMIUM" = "Dissolved", "FLUORIDE" = "Total", "LEAD" = "Dissolved", "MANGANESE" = "Dissolved", "NICKEL" = "Dissolved", "ZINC" = "Dissolved")
      
      lakes_hardness_dependent <- data %>% 
        filterByNamedVector("analyte", "sample_fraction", filter_vec) %>% 
        mutate(hardness = as.numeric(hardness),
               result_value = as.numeric(result_value),
          acute_standard = case_when(
            analyte == "CADMIUM" & sample_fraction == "Dissolved" ~ exp(values[["cad_as_a"]]+(values[["cad_as_b"]]*log(hardness)))*(values[["cad_as_cfb"]]-(log(hardness)*values[["cad_as_cfm"]])),
            analyte == "CHROMIUM" & sample_fraction == "Dissolved" ~ exp(values[["chrom_as_a"]]+(values[["chrom_as_b"]]*log(hardness)))*values[["chrom_as_cf"]],
            analyte == "COPPER" & sample_fraction == "Dissolved" ~ exp(values[["copper_as_a"]]+(values[["copper_as_b"]]*log(hardness)))*values[["copper_as_cf"]],
            analyte == "FLUORIDE" & sample_fraction == "Total" ~ (exp(values[["fluoride_as_a"]]+(values[["fluoride_as_b"]]*log(hardness))))/1000,
            analyte == "LEAD" & sample_fraction == "Dissolved" ~ exp(values[["lead_as_a"]]+(values[["lead_as_b"]]*log(hardness)))*(values[["lead_as_cfb"]]-(log(hardness)*values[["lead_as_cfm"]])),
            analyte == "MANGANESE" & sample_fraction == "Dissolved" ~ exp(values[["mang_as_a"]]+(values[["mang_as_b"]]*log(hardness)))*values[["mang_as_cf"]],
            analyte == "NICKEL" & sample_fraction == "Dissolved" ~ exp(values[["nickel_as_a"]]+(values[["nickel_as_b"]]*log(hardness)))*values[["nickel_as_cf"]],
            analyte == "ZINC" & sample_fraction == "Dissolved" ~ exp(values[["zinc_as_a"]]+(values[["zinc_as_b"]]*log(hardness)))*values[["zinc_as_cf"]],
            TRUE ~ NA
        )) %>% 
        # Calculating the numeric chronic standards here, but standards violations are determined in sv_standards_chronic_module.R
        mutate(chronic_standard = case_when(
          analyte == "CADMIUM" & sample_fraction == "Dissolved" ~ exp(values[["cad_cs_a"]]+(values[["cad_cs_b"]]*log(hardness)))*(values[["cad_cs_cfb"]]-(log(hardness)*values[["cad_cs_cfm"]])),
          analyte == "CHROMIUM" & sample_fraction == "Dissolved" ~ exp(values[["chrom_cs_a"]]+(values[["chrom_cs_b"]]*log(hardness)))*values[["chrom_cs_cf"]],
          analyte == "COPPER" & sample_fraction == "Dissolved" ~ exp(values[["copper_cs_a"]]+(values[["copper_cs_b"]]*log(hardness)))*values[["copper_cs_cf"]],
          analyte == "FLUORIDE" & sample_fraction == "Total" ~ (exp(values[["fluoride_cs_a"]]+(values[["fluoride_cs_b"]]*log(hardness))))/1000,
          analyte == "LEAD" & sample_fraction == "Dissolved" ~ exp(values[["lead_cs_a"]]+(values[["lead_cs_b"]]*log(hardness)))*(values[["lead_cs_cfb"]]-(log(hardness)*values[["lead_cs_cfm"]])),
          analyte == "MANGANESE" & sample_fraction == "Dissolved" ~ exp(values[["mang_cs_a"]]+(values[["mang_cs_b"]]*log(hardness)))*values[["mang_cs_cf"]],
          analyte == "NICKEL" & sample_fraction == "Dissolved" ~ exp(values[["nickel_cs_a"]]+(values[["nickel_cs_b"]]*log(hardness)))*values[["nickel_cs_cf"]],
          analyte == "ZINC" & sample_fraction == "Dissolved" ~ exp(values[["zinc_cs_a"]]+(values[["zinc_cs_b"]]*log(hardness)))*values[["zinc_cs_cf"]],
          TRUE ~ NA
        )) %>% 
        mutate(chronic_standard = ifelse(analyte == "FLUORIDE" & sample_fraction == "Total" & chronic_standard > 4, 4, chronic_standard)) %>% 
        mutate(quotient = result_value/acute_standard,
               standard_violation = ifelse(quotient > 1, "EXCEEDANCE", "NONE")) %>% 
        relocate(acute_standard, result_value, quotient, standard_violation, .before = 1)
      
      lakes_hardness_dependent
      
    })
    
    # output$dep_debugging_table <- renderReactable({
    #   req(data_reactive_internal())
    #   reactable(data_reactive_internal(),
    #             resizable = TRUE,
    #             searchable = TRUE,
    #             filterable = TRUE)
    # })
    
    data_reactive_internal
    
    # output$debugging_text <- renderPrint({
    #   values <- numericInput_ids()
    # 
    #   # Direct access by input name example
    #   cat("Direct access example:\n")
    #   cat(    "cad_as_a value", values[["cad_as_a"]], "\n")
    # })
    
  })
}