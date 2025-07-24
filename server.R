source("file_processing_module.R")
source("lakes_data_hardness_module.R")
source("single_value_standards_module.R")
source("dependent_numeric_standards_module.R")
source("lakes_ammonia_nitrogen_module.R")
source("sulfates_module.R")
source("chronic_standards_module.R")
source("thermocline_module.R")
source("chronic_lakes_ammonia_nitrogen_module.R")
source("tsi_min_reqs_module.R")
source("tsi_analysis_module.R")
source("user_colnames_module.R")
# source("use_support_module.R")

server <- function(input, output, session) {
  
# ---------- Options ----------
  
  options(shiny.maxRequestSize=30*1024^2) # Increase file size upload limit to 30MB
  
# ---------- General Functions ----------
  
  # A generalized function that creates a MathJax UI output using user supplied input values
  eq_standards <- function(input, ui_output_id, formula_template, input_mappings) {
    output[[ui_output_id]] <- renderUI({
      # Extract input values and use them with the formula template
      input_values <- sapply(input_mappings, function(mapping) input[[mapping$input_id]])
      text_formula <- do.call(sprintf, c(list(formula_template), input_values))
      
      withMathJax(tags$p(text_formula))
    })
  }
  
# ---------- Server Functions ----------  
  
  output$as_gen_out <- renderUI({
    text_as_gen <- "$$
    e^{V \\ln(H) + \\ln(A) - V \\ln(Z)} \\cdot \\text{CF}
    $$"
    withMathJax(
      tags$p(text_as_gen)
    )
  })
  
  output$cs_gen_out <- renderUI({
    text_cs_gen <- "$$
    e^{L \\ln(H) + \\ln(S) - L \\ln(Z)} \\cdot \\text{CF}
    $$"
    withMathJax(
      tags$p(text_cs_gen)
    )
  })
  
  map(eq_params, ~ do.call(eq_standards, list(
    input = input,
    ui_output_id = .x$ui_output_id,
    formula_template = .x$formula_template,
    input_mappings = map(.x$input_ids, function(id) list(input_id = id))
  )))
   
        ## ---------- Datamods Table; Single Value Standards ----------
    
  data_store <- reactiveVal(sv_standards)
  
  # Hidden editable table -- completely off-screen so it initializes but isn't visible
  # Only used for initialization
  hidden_res_edit <- edit_data_server(
    id = "hidden_editable_table",
    data_r = data_store,
    add = FALSE,
    update = TRUE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = c("acute_standard", "chronic_standard")
  )
  
  # Regular editable table for the visible tab
  # This one will be used for actual editing
  res_edit <- edit_data_server(
    id = "original_table",
    data_r = data_store,
    add = FALSE,
    update = TRUE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = c("acute_standard", "chronic_standard"),
    reactable_options = list(resizable = TRUE,
                             columns = purrr::map(standards_colnames_list, one_col))
  )
  
  # When the visible tab changes, update the shared data
  observe({
    updated_data <- res_edit()
    if(!is.null(updated_data)){
      data_store(updated_data)
    }
  })
  
  # Once the hidden table is initialized, remove the hidden div
  observe({
    req(hidden_res_edit())
    # Delay removal slightly to ensure everything is initialized
    delay(1000, {
      runjs("document.getElementById('hidden-ui-container').remove();")
    })
  })
  
        ## ---------- Functions from Modules ----------
  
  # Call file_processing_module.R server function
  uploaded_data <- fileProcessServer("file_upload", 2)
  
  output$uploaded_preview <- renderTable({
    req(uploaded_data())
    head(uploaded_data())
    })
  
  userColnamesServer_fpm_returns <- userColnamesServer("fpm_id", uploaded_data,
                                                       possible_colnames = possible_colnames)
  
  selected_cols <- reactive(userColnamesServer_fpm_returns())
  
  lakesHardnessServer_returns <- lakesHardnessServer("analysis",
                                                     selected_columns = selected_cols,
                                                     dataset = uploaded_data,
                                                     trigger = reactive(input$analysis))
  
  lakesAcuteServer_returns <- lakesAcuteServer("LAS_id", data_store, lakesHardnessServer_returns)
  
  lakesThermoclineServer_returns <- lakesThermoclineServer("thermocline_id", lakesHardnessServer_returns)
  
  # Define list of inputs used for hardness dependent numeric standards
  hardness_input_id_list <- c("cad_as_a", "cad_as_b", "cad_as_cfm", "cad_as_cfb",
                              "cad_cs_a", "cad_cs_b", "cad_cs_cfm", "cad_cs_cfb",
                              "chrom_as_a", "chrom_as_b", "chrom_as_cf",
                              "chrom_cs_a", "chrom_cs_b", "chrom_cs_cf",
                              "copper_as_a", "copper_as_b", "copper_as_cf",
                              "copper_cs_a", "copper_cs_b", "copper_cs_cf",
                              "fluoride_as_a", "fluoride_as_b",
                              "fluoride_cs_a", "fluoride_cs_b",
                              "lead_as_a", "lead_as_b", "lead_as_cfm", "lead_as_cfb",
                              "lead_cs_a", "lead_cs_b", "lead_cs_cfm", "lead_cs_cfb",
                              "mang_as_a", "mang_as_b", "mang_as_cf",
                              "mang_cs_a", "mang_cs_b", "mang_cs_cf",
                              "nickel_as_a", "nickel_as_b", "nickel_as_cf",
                              "nickel_cs_a", "nickel_cs_b", "nickel_cs_cf",
                              "zinc_as_a", "zinc_as_b", "zinc_as_cf",
                              "zinc_cs_a", "zinc_cs_b", "zinc_cs_cf")
  
  # Create reactive expression with all input values as a list
  all_hardness_input_values <- reactive({
    h_values <- lapply(hardness_input_id_list, function(id){
      input[[id]]
    })
    names(h_values) <- hardness_input_id_list
    h_values
  })
  
  # Reactive list 'all_hardness_input_values' is passed to dependentServer to make 
  # the values of those inputs available to the module
  dependentServer_returns <- dependentServer("DS_id", lakesHardnessServer_returns, all_hardness_input_values)
  
  # Define list of inputs used for ammonia nitrogen standards
  ammonia_nitrogen_input_id_list <- c("an_max_limit", "total_an_temp1", "total_an_temp2", "total_an_temp3", "an_acute_param1", "an_acute_param2", "an_acute_param3", "an_param1", "an_param2", "an_param3", "an_param4", "an_param5", "an_param6", "an_param7", "an_param8", "an_param9", "an_param10", "an_param11", "an_param12", "an_param13")
  
  all_ammonia_nitrogen_input_values <- reactive({
    an_values <- lapply(ammonia_nitrogen_input_id_list, function(id){
      input[[id]]
    })
    names(an_values) <- ammonia_nitrogen_input_id_list
    an_values
  })
  
  ammoniaNitrogenServer_returns <- ammoniaNitrogenServer("AN_id", lakesHardnessServer_returns,
                                                         all_ammonia_nitrogen_input_values)
  
  # Define list of inputs used for sulfate standards
  sulfates_input_id_list <- c("sulfate1_hardness1", "sulfate1_hardness2", "sulfate1_chloride1", "sulfate1_chloride2", "sulfate_acute_param1", "sulfate2_hardness1", "sulfate2_hardness2", "sulfate2_chloride1", "sulfate2_chloride2", "sulfate_acute_param2", "sulfate_acute_param3", "sulfate_acute_param4", "sulfate_acute_param5", "sulfate_acute_param6", "sulfate_acute_param7", "sulfate_acute_param8", "sulfate3_hardness", "sulfate3_chloride", "sulfate4_hardness", "sulfate4_chloride", "sulfate_constant1", "sulfate_constant2")
  
  all_sulfates_input_values <- reactive({
    sulfates_values <- lapply(sulfates_input_id_list, function(id){
      input[[id]]
    })
    names(sulfates_values) <- sulfates_input_id_list
    sulfates_values
  })
  
  sulfatesServer_returns <- sulfatesServer("sulfates_id", lakesHardnessServer_returns, all_sulfates_input_values)
  
  lakesChronicServer_returns <- lakesChronicServer("chronic_id", lakesHardnessServer_returns, 
                                                   hds_input = dependentServer_returns,
                                                   standards_input = data_store, 
                                                   thermocline_data = lakesThermoclineServer_returns)
  
  chronicAmmoniaNitrogenServer_returns <- chronicAmmoniaNitrogenServer(
                                            "CAN_id", lakesHardnessServer_returns,
                                            thermocline_data = lakesThermoclineServer_returns,
                                            numericInput_ids = all_ammonia_nitrogen_input_values)
  
  macrophyte_data <- fileProcessServer("macrophyte_upload", 1)
  
  userColnamesServer_ucm_returns <- userColnamesServer("ucm_id", macrophyte_data, 
                                                   possible_colnames = possible_macrophyte_colnames)
  
  macrophyte_selected_cols <- reactive(userColnamesServer_ucm_returns())
  
  output$mac_col_validation <- renderText({
    validate(
      need(!any(sapply(macrophyte_selected_cols(), is.null)), "Please select the required columns above!")
    )
  }) %>% bindEvent(macrophyte_data()) %>% 
    bindEvent(macrophyte_selected_cols())
  
  output$uploaded_mac_preview <- renderTable({
    req(macrophyte_data())
    head(macrophyte_data())
  })
  
  TSI_radio_state <- reactive(input$TSI_radio_id)
  
  tsiSiteReqServer_returns <- tsiSiteReqServer("tsiSR_id", lakesHardnessServer_returns,
                                               thermocline_data = lakesThermoclineServer_returns,
                                               macrophyte_data = macrophyte_data,
                                               macrophyte_columns = macrophyte_selected_cols,
                                               switch_state = TSI_radio_state)
    
  tsiAnalysisServer_returns <- tsiAnalysisServer("tsiA_id", tsiSiteReqServer_returns, switch_state = TSI_radio_state)
  
  # useSupportServer_returns <- useSupportServer("assess_use", lakesAcuteServer_returns, dependentServer_returns, ammoniaNitrogenServer_returns, sulfatesServer_returns)
  
  wb <- wb_workbook()
  wb$add_worksheet("acute_results")
  wb$add_worksheet("chronic_results")
  wb$add_worksheet("dependent_results")
  wb$add_worksheet("ammonia_nitrogen_results")
  wb$add_worksheet("chronic_ammonia_nitrogen_res")
  wb$add_worksheet("sulfates_results")
  wb$add_worksheet("tsi_results")
  
  observe({
    wb$add_data("acute_results", lakesAcuteServer_returns())
    wb$add_data("chronic_results", lakesChronicServer_returns())
    wb$add_data("dependent_results", dependentServer_returns())
    wb$add_data("ammonia_nitrogen_results", ammoniaNitrogenServer_returns())
    wb$add_data("chronic_ammonia_nitrogen_res", chronicAmmoniaNitrogenServer_returns())
    wb$add_data("sulfates_results", sulfatesServer_returns())
    wb$add_data("tsi_results", tsiAnalysisServer_returns())
  }) %>%
    bindEvent(input$analysis)
  
  observeEvent(input$analysis, {
    req(lakesAcuteServer_returns())
    shinyjs::show("download_results")
  })

  output$download_results <- downloadHandler(
    filename = "ALU_assessment_results.xlsx",
    content = function(file){
      wb_save(wb, file)
    }
  )
  
  # For debugging; restart app on shiny server
  # observeEvent(input$restart, {
  #   file.create("restart.txt")
  # })
  
}