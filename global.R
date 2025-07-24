# ---------- Packages ----------

library(bslib)
library(janitor)
library(openxlsx)
library(openxlsx2)
library(readxl)
library(shiny)
library(shinyjs)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(shinymath)
library(shinycssloaders)
library(datamods)
library(reactable)
library(lubridate)

# ---------- Functions ----------

# Function to filter a dataframe by a named vector
filterByNamedVector <- function(df, col_name, match_col_name, vec){
  # Check if columns exist
  if(!col_name %in% names(df)){
    stop(paste0("Column '", col_name, "' not found in '", deparse(substitute(df)), "'"))
  } 
  if(!match_col_name %in% names(df)){
    stop(paste("Column '", match_col_name, "' not found in '", deparse(substitute(df)), "'"))
  }
  
  # Create mask and ensure it's the right length
  # Also handle case when col_values doesn't exist in names(vec)
  mask <- !is.na(vec[df[[col_name]]]) & (vec[df[[col_name]]] == df[[match_col_name]])
  
  df[mask, , drop = FALSE]
}

# Function for hardness dependent standard calculations
eq1 <- function(A, B, CFB, CFM, H){
  exp(A+(B*log(H)))*(CFB-(log(H)*CFM))
}

# Function for creating UI element for sulfate standards
sulfate_ranges <- function(inputId, hardness_range1, hardness_range2, chloride_range1, chloride_range2){
  div(p("When:"),
      div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 1rem;",
          create_inline_input(inputId[1], hardness_range1),
          "mg/L ≤ hardness ≤",
          create_inline_input(inputId[2], hardness_range2),
          "mg/L, and"),
      div(style = "display: flex; align-items: center; gap: 5px;",
          create_inline_input(inputId[3], chloride_range1),
          "mg/L ≤ chloride ≤",
          create_inline_input(inputId[4], chloride_range2),
          "mg/L, then"))
}

create_standard_card <- function(card_title_name = NULL, card_content_before = NULL, inputs_list, ui_output_name, card_content_after = NULL) {
  # inputs_list should be a list of lists, where each inner list contains:
  #   - id: The input ID 
  #   - label: The display label
  #   - value: The default value
  #   - step: The step increment (optional, defaults to 0.1)
  #   - other_args: A named list of any other arguments to pass to numericInput (optional)
  
  # Inputs are organized into rows based on the input_rows parameter which specifies how many inputs to place in each row
  
  # Create all numeric inputs
  all_inputs <- lapply(inputs_list, function(input_info) {
    # Extract parameters with defaults
    id <- input_info$id
    label <- input_info$label
    value <- input_info$value
    step <- input_info$step %||% 0.1 # Replaces null with default value. %||% is inspired by the way Ruby's or operation (||) works
    other_args <- input_info$other_args %||% list()
    
    # Create the numericInput with any additional arguments
    do.call(numericInput, 
            c(list(inputId = id, label = label, value = value, step = step), 
              other_args))
  })
  
  # Group inputs by rows (default 2 per row)
  rows_of_inputs <- list()
  input_groups <- split(all_inputs, ceiling(seq_along(all_inputs) / 2))
  
  for (group in input_groups) {
    rows_of_inputs <- c(rows_of_inputs, list(do.call(tags$div, group)))
  }
  
  # Process card_content based on its type
  # If it's already a UI element, use it directly; otherwise, wrap it in a div
  card_content_before_ui <- if (inherits(card_content_before, "shiny.tag") || inherits(card_content_before, "shiny.tag.list")) {
    card_content_before  # Already a UI element, use as is
  } else {
    div(card_content_before)  # Wrap text or other content in a div
  }
  
  card_content_after_ui <- if(!is.null(card_content_after)){
    if(inherits(card_content_after, "shiny.tag") || inherits(card_content_after, "siny.tag.list")){
      card_content_after
    } else{
      div(card_content_after)
    }
  } 
  
  # Create and return the complete card
  if(is.null(card_title_name) & is.null(card_content_before) & is.null(card_content_after)){
    card(
      layout_columns(!!!rows_of_inputs),
      uiOutput(ui_output_name)
    )
  }
  if(is.null(card_title_name) & is.null(card_content_after)){
    card(
      card_content_before_ui,
      layout_columns(!!!rows_of_inputs),
      uiOutput(ui_output_name)
    )
  } else if(!is.null(card_title_name) & is.null(card_content_after)){
    card(
      card_title(card_title_name),
      card_content_before_ui,
      layout_columns(!!!rows_of_inputs),
      uiOutput(ui_output_name)
    )
  } else{
    card(
      card_title(card_title_name),
      card_content_before_ui,
      layout_columns(!!!rows_of_inputs),
      uiOutput(ui_output_name),
      card_content_after_ui
    )
  }
}

create_card_elements_for_group <- function(card_group) {
  map(
    card_group,
    function(card_data) {
      create_standard_card(
        card_title_name = card_data$card_title_name,
        card_content_before = card_data$card_content_before,
        inputs_list = card_data$inputs_list,
        ui_output_name = card_data$ui_output_name,
        card_content_after = card_data$card_content_after
      )
    }
  )
}

# For creating the minimum column width in a ReacTable
one_col <- function(label){
  colDef(minWidth = nchar(label)*10)
}

# Create a numericInput that can be used in the same line as text
create_inline_input <- function(inputId, input_value){
  div(
    class = "inline-edit-container",
    numericInput(inputId = inputId,
                 value = input_value,
                 label = NULL,
                 width = "80px")
  )
}

# To lay out the user selected column names with 4 inputs for each row
userColnamesUI <- function(selectize_lists_input){
  layout_column_wrap(width = 1 / 4, !!!map(
    selectize_lists_input, ~ do.call(selectizeInput, .x)
  ))
}

# ---------- Variables ---------- 

  # These are all the variables that need to be defined before running the app

Monitoring_locations_PFPWS <- read.xlsx("Monitoring_locations_PFPWS.xlsx")

sv_standards <- read.xlsx("test_gen_use_standards.xlsx")

standards_colnames <- as.list(colnames(sv_standards))

standards_colnames_list <- set_names(standards_colnames, standards_colnames)

# Explanation of equation variables for sulfate standards
sulfate_explanation <- HTML('
                          Where:<br>
                          C = Sulfate Concentration<br>
                          FAI = Final Acute Intercept<br>
                            <ul>
                              <li>Derived using the species acute intercepts, obtained from ILSC 302.618(g)</li>
                            </ul>
                          m<sub>H</sub> = Hardness slope<br>
                          m<sub>C</sub> = Chloride slope<br>
                            <ul>
                              <li>Values from "Effects of Water Quality on Acute and Chronic Toxicity of Sulfate to Freshwater Bivalves <em>Ceriodaphnia dubia</em>, and <em>Hyalella azteca</em>."</li>
                              <li>Obtained with multiple linear regression analysis with covariance using sulfate LC50 as the dependent variable, and hardness, chloride, and species as independent variables</li>
                             ')

hardness_dependent_cards <- list(
  list(list(card_title_name = "Cadmium Acute Standard",
         inputs_list = list(list(id = "cad_as_a", label = "A", value = -2.918), 
                            list(id = "cad_as_b", label = "B", value = 1.128), 
                            list(id = "cad_as_cfm", label = HTML("CF<sub>m</sub>"), value = 0.041838), 
                            list(id = "cad_as_cfb", label = HTML("CF<sub>b</sub>"), value = 1.138672)),
         ui_output_name = "cad_as_out"),
    list(card_title_name = "Cadmium Chronic Standard",
         inputs_list = list(list(id = "cad_cs_a", label = "A", value = -3.490), 
                            list(id = "cad_cs_b", label = "B", value = 0.7852), 
                            list(id = "cad_cs_cfm", label = HTML("CF<sub>m</sub>"), value = 0.041838), 
                            list(id = "cad_cs_cfb", label = HTML("CF<sub>b</sub>"), value = 1.101672)),
         ui_output_name = "cad_cs_out")),
  list(list(card_title_name = "Chromium Acute Standard", 
            inputs_list = list(list(id = "chrom_as_a", label = "A", value = 3.688), 
                               list(id = "chrom_as_b", label = "B", value = 0.8190), 
                               list(id = "chrom_as_cf", label = "CF", value = 0.316)),
         ui_output_name = "chrom_as_out"),
    list(card_title_name = "Chromium Chronic Standard",
         inputs_list = list(list(id = "chrom_cs_a", label = "A", value = 1.561), 
                            list(id = "chrom_cs_b", label = "B", value = 0.8190), 
                            list(id = "chrom_cs_cf", label = "CF", value = 8.60)),
         ui_output_name = "chrom_cs_out")),
  list(list(card_title_name = "Copper Acute Standard", 
            inputs_list = list(list(id = "copper_as_a", label = "A", value = -1.464), 
                               list(id = "copper_as_b", label = "B", value = 0.9422), 
                               list(id = "copper_as_cf", label = "CF", value = 0.960)),
         ui_output_name = "copper_as_out"),
    list(card_title_name = "Copper Chronic Standard", 
         inputs_list = list(list(id = "copper_cs_a", label = "A", value = -1.465), 
                            list(id = "copper_cs_b", label = "B", value = 0.8545), 
                            list(id = "copper_cs_cf", label = "CF", value = 6.7319)),
         ui_output_name = "copper_cs_out")),
  list(list(card_title_name = "Fluoride Acute Standard", 
            inputs_list = list(list(id = "fluoride_as_a", label = "A", value = 6.7319), 
                               list(id = "fluoride_as_b", label = "B", value = 0.5394)),
         ui_output_name = "fluoride_as_out"),
    list(card_title_name = "Fluoride Chronic Standard", 
         inputs_list = list(list(id = "fluoride_cs_a", label = "A", value = 6.0445), 
                            list(id = "fluoride_cs_b", label = "B", value = 0.5394)),
         ui_output_name = "fluoride_cs_out")),
  list(list(card_title_name = "Lead Acute_standard",
            inputs_list = list(list(id = "lead_as_a", label = "A", value = -1.301), 
                               list(id = "lead_as_b", label = "B", value = 1.273), 
                               list(id = "lead_as_cfm", label = HTML("CF<sub>m</sub>"), value = 0.145712), 
                               list(id = "lead_as_cfb", label = HTML("CF<sub>b</sub>"), value = 1.46203)),
         ui_output_name = "lead_as_out"),
       list(card_title_name = "Lead Chronic Standard",
         inputs_list = list(list(id = "lead_cs_a", label = "A", value = -2.863), 
                            list(id = "lead_cs_b", label = "B", value = 1.273), 
                            list(id = "lead_cs_cfm", label = HTML("CF<sub>m</sub>"), value = 0.145712), 
                            list(id = "lead_cs_cfb", label = HTML("CF<sub>b</sub>"), value = 1.46203)),
         ui_output_name = "lead_cs_out")),
  list(list(card_title_name = "Manganese Acute Standard",
            inputs_list = list(list(id = "mang_as_a", label = "A", value = 4.9187), 
                               list(id = "mang_as_b", label = "B", value = 0.7467), 
                               list(id = "mang_as_cf", label = "CF", value = 0.9812)),
            ui_output_name = "mang_as_out"),
       list(card_title_name = "Manganese Chronic Standard",
            inputs_list = list(list(id = "mang_cs_a", label = "A", value = 4.0635),
                               list(id = "mang_cs_b", label = "B", value = 0.7467),
                               list(id = "mang_cs_cf", label = "CF", value = 0.9812)),
            ui_output_name = "mang_cs_out")),
  list(list(card_title_name = "Nickel Acute Standard",
            inputs_list = list(list(id = "nickel_as_a", label = "A", value = 0.5173),
                               list(id = "nickel_as_b", label = "B", value = 0.8460),
                               list(id = "nickel_as_cf", label = "CF", value = 0.998)),
            ui_output_name = "nickel_as_out"),
       list(card_title_name = "Nickel Chronic Standard",
            inputs_list = list(list(id = "nickel_cs_a", label = "A", value = -2.286),
                               list(id = "nickel_cs_b", label = "B", value = 0.8460),
                               list(id = "nickel_cs_cf", label = "CF", value = 0.997)),
            ui_output_name = "nickel_cs_out")),
  list(list(card_title_name = "Zinc Acute Standard",
            inputs_list = list(list(id = "zinc_as_a", label = "A", value = 0.9035),
                               list(id = "zinc_as_b", label = "B", value = 0.8473),
                               list(id = "zinc_as_cf", label = "CF", value = 0.978)),
            ui_output_name = "zinc_as_out"),
       list(card_title_name = "Zinc Chronic Standard",
            inputs_list = list(list(id = "zinc_cs_a", label = "A", value = -0.4456),
                               list(id = "zinc_cs_b", label = "B", value = 0.8473),
                               list(id = "zinc_cs_cf", label = "CF", value = 0.986)),
            ui_output_name = "zinc_cs_out"))
)

tab_titles <- c("Cadmium", "Chromium", "Copper", "Fluoride", "Lead", "Manganese", "Nickel", "Zinc")

card_element_groups <- map(hardness_dependent_cards, create_card_elements_for_group)

ammonia_nitrogen_cards <- list(
  list(list(card_title_name = "Acute Standard",
       card_content_before = div(p("The acute standard (AS) is calculated using the following equation:")),
       inputs_list = list(list(id = "an_acute_param1", label = "A", value = 0.411),
                          list(id = "an_acute_param2", label = "B", value = 7.204),
                          list(id = "an_acute_param3", label = "C", value = 58.4)),
       ui_output_name = "an_acute_ui_output",
       card_content_after =
         HTML('<h5>General Equation</h5><br>
              $$
              (\\text{AV}_{\\text{t,}8})\\cdot(\\frac{\\text{LC}50_{\\text{t,}8}}{\\frac{\\text{R}}{1+10^{\\text{pH}_\\text{T}-8}}+\\frac{1}{1+10^{\\text{pH}-\\text{pH}_\\text{T}}}})\\cdot(\\frac{\\text{R}}{1+10^{\\text{pH}_{\\text{T}}-\\text{pH}}}+\\frac{1}{1+10^{\\text{pH}-\\text{pH}_\\text{T}}})
              $$
              <br>
              Where:<br>
              AV<sub>t,8</sub> = The Criterion Maximum Concentration (CMC) of the Final Acute Value (FAV) at pH = 8 (i.e. one-half of the FAV)<br>
              R = LIM<sub>H</sub>/LIM<sub>L</sub><br>
              LIM<sub>H</sub> = Asymptotic (limiting) LC50 at high pH<br>
              LIM<sub>L</sub> = Asymptotic (limiting) LC50 at low pH<br>
              pH<sub>T</sub> = The transition pH at which the LC50 is the arithmetic average of LIM<sub>H</sub> and LIM<sub>L</sub>')
       )),
  list(list(card_title_name = "Chronic Standard; Early Life Stage Present",
       card_content_before = div(p("When:"),
                                 div(style = "display: flex; align-items: center; gap: 5px;",
                                     "water temperature ≤", create_inline_input("total_an_temp1", 14.51), "°C")),
       inputs_list = list(list(id = "an_param1", label = "A", value = 0.0577),
                          list(id = "an_param2", label = "B", value = 7.688),
                          list(id = "an_param3", label = "C", value = 2.487),
                          list(id = "an_param4", label = "D", value = 2.85)),
       ui_output_name = "an_ui_output1",
       card_content_after =
         HTML('<h5>General Equation</h5><br>
              $$
              (\\text{CV}_{\\text{t,}8})\\cdot(\\frac{\\text{LC}50_{\\text{t,}8}}{\\frac{\\text{R}}{1+10^{\\text{pH}_\\text{T}-8}}+\\frac{1}{1+10^{\\text{pH}-\\text{pH}_\\text{T}}}})\\cdot(\\frac{\\text{R}}{1+10^{\\text{pH}_{\\text{T}}-\\text{pH}}}+\\frac{1}{1+10^{\\text{pH}-\\text{pH}_\\text{T}}})
              $$
              <br>
              Where:<br>
              CV<sub>t,8</sub> = The Criterion Continuous Concentration. In this case, it is 85.4% of the lowest seasonal Genus Mean Chronic Value (GMCV) for fish. The lowest GMCV for fish is 2.85 mg N/L for <em>Lepomis sp.</em> juveniles.<br>
              R = LIM<sub>H</sub>/LIM<sub>L</sub><br>
              LIM<sub>H</sub> = Asymptotic (limiting) LC50 at high pH<br>
              LIM<sub>L</sub> = Asymptotic (limiting) LC50 at low pH<br>
              pH<sub>T</sub> = The transition pH at which the LC50 is the arithmetic average of LIM<sub>H</sub> and LIM<sub>L</sub>'))),
  list(list(card_title_name = "Chronic Standard; Early Life Stage Present",
       card_content_before = div(p("When:"),
                                 div(style = "display: flex; align-items: center; gap: 5px;",
                                     "water temperature >", create_inline_input("total_an_temp2", 14.51), "°C")),
       inputs_list = list(list(id = "an_param5", label = "A", value = 0.0577),
                          list(id = "an_param6", label = "B", value = 2.487),
                          list(id = "an_param7", label = "C", value = 1.45)),
       ui_output_name = "an_ui_output2",
       card_content_after =
         HTML('The function \\(1.45\\cdot10^{0.028\\cdot(25-\\text{T})}\\) increases steadily with decreasing temperature, T, until it reaches its maximum \\((0.854\\cdot2.85)\\) at 14.5°C, below which it remains constant.'))),
  list(list(card_title_name = "Chronic Standard; Early Life Stage Absent",
       card_content_before = div(p("When:"),
                                 div(style = "display: flex; align-items: center; gap: 5px;",
                                     "water temperature ≤", create_inline_input("total_an_temp3", 7), "°C")),
       inputs_list = list(list(id = "an_param8", label = "A", value = 0.0577),
                          list(id = "an_param9", label = "B", value = 2.487),
                          list(id = "an_param10", label = "C", value = 1.45)),
       ui_output_name = "an_ui_output3",
       card_content_after =
         HTML('
              Notice that \\(1.45\\cdot10^{0.504}\\) is simply \\(1.45\\cdot10^{0.028(25-T)}\\) with \\(\\text{T} = 7\\text{°C}\\)
              '))),
  list(list(card_title_name = "Chronic Standard; Early Life Stage Absent",
       card_content_before = div(p("When:"),
                                 div(style = "display: flex; align-items: center; gap: 5px;",
                                     "water temperature >", create_inline_input("total_an_temp4", 7), "°C")),
       inputs_list = list(list(id = "an_param11", label = "A", value = 0.0577),
                          list(id = "an_param12", label = "B", value = 2.487),
                          list(id = "an_param13", label = "C", value = 1.45)),
       ui_output_name = "an_ui_output4"))
)

an_tab_titles <- c("Acute Standard", "Chronic Standard 1; Early Life Stage Present", "Chronic Standard 2; Early Life Stage Present", "Chronic Standard 3; Elary Life Stage Absent", "Chronic Standard 4; Early Life Stage Absent")

an_card_element_groups <- map(ammonia_nitrogen_cards, create_card_elements_for_group)

possible_colnames <- data.frame(names = c("monitoring_location_id", "activity_start_date", "activity_start_time", "activity_depth_height", "activity_depth_height_unit", "characteristic_name", "sample_fraction", "result_value", "result_unit", "result_comment", "detection_limit_type_1", "detection_limit_value_1", "detection_limit_unit_1", "detection_limit_type_2", "detection_limit_value_2", "detection_limit_unit_2"),
                                ids = c("mlid", "start_date", "start_time", "sample_depth", "depth_unit", "analyte", "sample_fraction", "result_value", "result_unit", "qualifiers", "limit_type_1", "limit_value_1", "limit_unit_1", "limit_type_2", "limit_value_2", "limit_unit_2"))

possible_macrophyte_colnames <- data.frame(names = c("basin_code", "collection_date", "percent_macrophyte_cover"),
                                           ids = c("AUID", "collection_date_id", "coverage"))
  
selectize_lists <- list(
  list(inputId = 'mlid', label = 'Monitoring Location ID', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'start_date', label = 'Activity Start Date', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'start_time', label = 'Activity Start Time', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'sample_depth', label = 'Activity Depth/Height', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'depth_unit', label = 'Activity Depth/Height Unit', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'analyte', label = 'Characteristic Name', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'sample_fraction', label = 'Sample Fraction', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'result_value', label = 'Result Value', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'result_unit', label = 'Result Unit', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'qualifiers', label = 'Result Comment', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'limit_type_1', label = 'Detection Limit Type 1', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'limit_value_1', label = 'Detection Limit Value 1', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'limit_unit_1', label = 'Detection Limit Unit 1', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'limit_type_2', label = 'Detection Limit Type 2', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'limit_value_2', label = 'Detection Limit Value 2', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'limit_unit_2', label = 'Detection Limit Unit 2', choices = NULL, multiple = TRUE, selected = NULL, options = list(placeholder = "", maxItems = 1, dropdownParent = 'body'))
)

macrophyte_selectize_lists <- list(
  list(inputId = 'AUID', label = 'AUID', choices = NULL, multiple = TRUE, selected = NULL, 
       options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'coverage', label = "Percent Macrophyte Coverage", choices = NULL, multiple = TRUE, selected = NULL,
       options = list(placeholder = "", maxItems = 1, dropdownParent = 'body')),
  list(inputId = 'collection_date_id', label = "Collection Date", choices = NULL, multiple = TRUE, selected = NULL,
       options = list(placeholder = "", maxItems = 1, dropdownParent = 'body'))
)

eq_params <- list(
  list(ui_output_id = "cad_as_out_test", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot (%s - %s \\cdot \\ln(H))$$", input_ids = c("cad_as_a_test", "cad_as_b_test", "cad_as_cfm_test", "cad_as_cfb_test")),
  list(ui_output_id = "cad_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot (%s - %s \\cdot \\ln(H))$$", input_ids = c("cad_as_a", "cad_as_b", "cad_as_cfm", "cad_as_cfb")),
  list(ui_output_id = "cad_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot (%s - %s \\cdot \\ln(H))$$", input_ids = c("cad_cs_a", "cad_cs_b", "cad_cs_cfm", "cad_cs_cfb")),
  list(ui_output_id = "chrom_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("chrom_as_a", "chrom_as_b", "chrom_as_cf")),
  list(ui_output_id = "chrom_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("chrom_cs_a", "chrom_cs_b", "chrom_cs_cf")),
  list(ui_output_id = "copper_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("copper_as_a", "copper_as_b", "copper_as_cf")),
  list(ui_output_id = "copper_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("copper_cs_a", "copper_cs_b", "copper_cs_cf")),
  list(ui_output_id = "fluoride_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)}$$", input_ids = c("fluoride_as_a", "fluoride_as_b")),
  list(ui_output_id = "fluoride_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)}$$", input_ids = c("fluoride_cs_a", "fluoride_cs_b")),
  list(ui_output_id = "lead_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot (%s - %s \\cdot \\ln(H))$$", input_ids = c("lead_as_a", "lead_as_b", "lead_as_cfm", "lead_as_cfb")),
  list(ui_output_id = "lead_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot (%s - %s \\cdot \\ln(H))$$", input_ids = c("lead_cs_a", "lead_cs_b", "lead_cs_cfm", "lead_cs_cfb")),
  list(ui_output_id = "mang_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("mang_as_a", "mang_as_b", "mang_as_cf")),
  list(ui_output_id = "mang_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("mang_cs_a", "mang_cs_b", "mang_cs_cf")),
  list(ui_output_id = "nickel_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("nickel_as_a", "nickel_as_b", "nickel_as_cf")),
  list(ui_output_id = "nickel_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("nickel_cs_a", "nickel_cs_b", "nickel_cs_cf")),
  list(ui_output_id = "zinc_as_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("zinc_as_a", "zinc_as_b", "zinc_as_cf")),
  list(ui_output_id = "zinc_cs_out", formula_template = "$$e^{%s + %s \\cdot \\ln(H)} \\cdot %s$$", input_ids = c("zinc_cs_a", "zinc_cs_b", "zinc_cs_cf")),
  list(ui_output_id = "an_acute_ui_output", formula_template = "$$\\text{AS}=\\frac{%s}{1+10^{%s-\\text{pH}}}+\\frac{%s}{1+10^{\\text{pH}-%s}}$$", input_ids = c("an_acute_param1", "an_acute_param2", "an_acute_param3", "an_acute_param2")),
  list(ui_output_id = "an_ui_output1", formula_template = "$$\\text{CS}=(\\frac{%s}{1+10^{%s-\\text{pH}}}+\\frac{%s}{1+10^{\\text{pH}-%s}})\\cdot%s$$", input_ids = c("an_param1", "an_param2", "an_param3", "an_param2", "an_param4")),
  list(ui_output_id = "an_ui_output2", formula_template = "$$\\text{CS}=(\\frac{%s}{1+10^{7.688-\\text{pH}}}+\\frac{%s}{1+10^{\\text{pH}-7.688}})\\cdot(%s\\cdot10^{0.028\\cdot(25-T)})$$", input_ids = c("an_param5", "an_param6", "an_param7")),
  list(ui_output_id = "an_ui_output3", formula_template = "$$\\text{CS}=(\\frac{%s}{1+10^{7.688-\\text{pH}}}+\\frac{%s}{1+10^{\\text{pH}-7.688}})\\cdot(%s\\cdot10^{0.504})$$", input_ids = c("an_param8", "an_param9", "an_param10")),
  list(ui_output_id = "an_ui_output4", formula_template = "$$\\text{CS}=(\\frac{%s}{1+10^{7.688-\\text{pH}}}+\\frac{%s}{1+10^{\\text{pH}-7.688}})\\cdot(%s\\cdot10^{0.028\\cdot(25-\\text{T})})$$", input_ids = c("an_param11", "an_param12", "an_param13")),
  list(ui_output_id = "sulfate_acute_ui_output", formula_template = "$$\\text{C}=[%s+%s(\\text{hardness})-%s(\\text{chloride})]\\cdot%s$$", input_ids = c("sulfate_acute_param1", "sulfate_acute_param2", "sulfate_acute_param3", "sulfate_acute_param4")),
  list(ui_output_id = "sulfate_acute_ui_output1", formula_template = "$$\\text{C}=[%s+%s(\\text{hardness})-%s(\\text{chloride})]\\cdot%s$$", input_ids = c("sulfate_acute_param5", "sulfate_acute_param6", "sulfate_acute_param7", "sulfate_acute_param8"))
)