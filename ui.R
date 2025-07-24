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

# ---------- Begin UI ----------

ui <- fluidPage(
  tags$head(tags$style(
    HTML("
      .inline-edit-container * {
        margin-bottom: 0 !important;
      }
      
      /* Make validation errors red and bold */
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    ")
  )),
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  useShinyjs(),
  
  # Add editable table UI to a hidden div
  div(
    id = "hidden-ui-container",
    style = "position: absolute; left: -9999px; top: -9999px; width: 1px, height: 1px; overflow: hidden;",
    edit_data_ui("hidden_editable_table")
  ),
  
  navbarPage(collapsible = TRUE,
    "General Aquatic Life Use Calculator",
    tabPanel("Instructions", 
      card(
      card_header("Instructions"), card_body(
        HTML('
         <p>This is a semi-automated tool for assessing Aquatic Life Use (ALU) for general use waters as defined in 35 ILCS 303/201. These are waters of the State for which there is no specified designation.</p>
         <p>To use this tool, navigate to the tab titled "Assessment Tool" and follow the instructions. There are multiple steps, each of which can be found by naviagting to the correspondingly labeled card tab. When you are finished with the task under each step, move on to the next step on the following card tab.<p/>
         ')
      )
    )
    ),
    
    # ---------- Numeric Standards ----------
    
    tabPanel("Numeric Standards", card(
      card_header("Numeric Standards"),
      card_body(
        HTML('
          <p>This is a table of all the numeric standards for general use waters as defined in 35 ILCS 303/201. If any of the standards have changed since 2025, you can edit them here using the button to the right of the row containing the desired standard. Once you have finished, continue to the next tab.<p/>
             '),
        edit_data_ui("original_table")
      )
    )),
    
    # ---------- Hardness Dependent Standards ----------
    
    tabPanel(
      "Hardness Dependent Standards",
      
      # For each card element group, create a tab with the corresponding title, and lay out the cards in columns half the width of the container
      navset_card_tab(!!!map2(card_element_groups, tab_titles, function(elements, title) {
        nav_panel(title = title, layout_columns(!!!elements, col_widths = 6))
      })),
      layout_columns(
        card(
          card_title("Generic Acute Standard Equation"),
          uiOutput("as_gen_out"),
          HTML("
            <p>
            Where:<br>
            V = pooled acute slope,<br>
            A = Final Acute Value at Z,<br>
            Z = a selected value of the water quality characteristic, and<br>
            CF = the conversion factor for converting from total to dissolved concentrations
            </p>
               ")
        ),
        card(
          card_title("Generic Chronic Standard Equation"),
          uiOutput("cs_gen_out"),
          HTML("
            <p>
            Where: <br>
            L = pooled chronic slope,<br>
            S = Final Chronic Value at Z,<br>
            Z = a selected value of the water quality characteristic, and<br>
            CF = the conversion factor for converting from total to dissolved concentrations
            </p>
               ")
        )
      )
    ),
    
    # ---------- Sulfate Standards ----------
    
    tabPanel("Sulfate Standards", card(
      card_header("Sulfate Standards"),
      card_body(
        HTML('
          <p>Sulfate standards are dependent on both hardness and chloride concentration. Different combinations of hardness and chloride concentrations have different standards. The sulfate standards for these combinations are displayed below.</p>
             '),
        layout_columns(
          create_standard_card(
            card_content_before =
              sulfate_ranges(
                c("sulfate1_hardness1",
                  "sulfate1_hardness2",
                  "sulfate1_chloride1",
                  "sulfate1_chloride2"),
                hardness_range1 = 100,
                hardness_range2 = 500,
                chloride_range1 = 25,
                chloride_range2 = 500
              ),
            inputs_list = list(
              list(id = "sulfate_acute_param1", label = "FAI", value = 1276.7),
              list(id = "sulfate_acute_param2", label = HTML("m<sub>H</sub>"), value = 5.508),
              list(id = "sulfate_acute_param3", label = HTML("m<sub>C</sub>"), value = 1.457),
              list(id = "sulfate_acute_param4", label = "Sulfate Specific Factor", value = 0.65)
              ),
            ui_output_name = "sulfate_acute_ui_output",
            card_content_after = sulfate_explanation
          ),
          create_standard_card(
            card_content_before =
              sulfate_ranges(
                c("sulfate2_hardness1",
                  "sulfate2_hardness2",
                  "sulfate2_chloride1",
                  "sulfate2_chloride2"),
                hardness_range1 = 100,
                hardness_range2 = 500,
                chloride_range1 = 5,
                chloride_range2 = 25
              ),
            inputs_list = list(
              list(id = "sulfate_acute_param5", label = "FAI", value = -57.478),
              list(id = "sulfate_acute_param6", label = HTML("m<sub>H</sub>"), value = 5.79),
              list(id = "sulfate_acute_param7", label = HTML("m<sub>C</sub>"), value = 54.163),
              list(id = "sulfate_acute_param8", label = "Sulfate Specific Factor", value = 0.65)
              ),
            ui_output_name = "sulfate_acute_ui_output1",
            card_content_after = sulfate_explanation
          )
        ),
        layout_columns(card(
          p("When:"),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            "hardness <",
            create_inline_input("sulfate3_hardness", 100),
            "mg/L, or"
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            "chloride <",
            create_inline_input("sulfate3_chloride", 5),
            "mg/L, then"
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            "C = ",
            create_inline_input("sulfate_constant1", 500),
            "mg/L"
          ),
        ), card(
          p("When:"),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            "hardness >",
            create_inline_input("sulfate4_hardness", 500),
            "mg/L, and"
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            "chloride ≥",
            create_inline_input("sulfate4_chloride", 5),
            "mg/L, then"
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            "C = ",
            create_inline_input("sulfate_constant2", 2000),
            "mg/L")
        ))
      )
    )),
    
    # ---------- Total Ammonia Nitrogen ----------
    
    tabPanel(
      "Total Ammonia Nitrogen",
      HTML('
        <p>Total ammonia nitrogen standards are pH and temperature dependent. Different combinations of pH and temperature have different standards. The ammonia nitrogen standards for these combinations are displayed below. There is additionally a maximum allowable limit.</p>
        '),
      div(style = "display: flex; align-items: center; gap: 5px;",
          "The maximum allowable limit for ammonia nitrogen is: ",
          create_inline_input("an_max_limit", 15), "mg/L"),
      br(),
      navset_card_tab(!!!map2(an_card_element_groups, an_tab_titles, function(elements, title) {
        nav_panel(title = title, !!!elements)
      })),
      card(card_header("Further Explanations"), card_body(
        HTML('
          <p>More information about the equations and derivations thereof for ammonia nitrogen standards can be found in the USEPA\'s 1999 "Update of Ambient Water Quality Criteria for Ammonia," which is available for download here. The equations used in this app are from 35 ILCS 302/212, which only minimally differ from those in USEPA, 1999</p>
          <p>Importantly, the following notices are included in USEPA\'s 1999 update:</p>
          <p style = "margin-left:10%; margin-right:10%;">This update provides guidance to States and Tribes authorized to establish water quality standards under the Clean Water Act (CWA), to protect aquatic life from acute and chronic effects of ammonia. Under the CWA, States and Tribes are to establish water quality criteria to protect designated uses. State and tribal decision makers retain the discretion to adopt approaches on a case-by-case basis that differ from this guidance when appropriate. While this update constitutes EPA\'s scientific recommendations regarding ambient concentrations of ammonia that protect freshwater aquatic life, this update does not substitute for the CWA or EPA\'s regulations; nor is it a regulation itself. Thus, it cannot impose legally binding requirements on EPA, States, Tribes, or the regulated community, and might not apply to a particular situation based upon the circumstances. EPA may change this guidance in the future.</p>
             ')
      ))
    ),
    
    # ---------- Assessment Tool ----------
    
    tabPanel("Assessment Tool", navset_card_tab(
      nav_panel(
        "Step 1",
        HTML('<p>Please upload the lakes water data that you would like analyzed for ALU use here:</p>'),
        fileInputUI("file_upload"),
        textOutput("fileError"), # Display error message if file is not xlsx
        HTML('<p>A preview of the file you uploaded will appear below</p>'),
        withSpinner(tableOutput("uploaded_preview"), type = 3, color.background = "#ffffff")
      ),
      nav_panel(
        "Step 2",
        HTML('
          <p>There are 16 types of data for water chemistry that are required from your file to be able to assess waterbodies for ALU. Because the column names for data stored in AWQMS are subject to change, the columns must be manually selected to ensure proper results. The data required and their possible column name from AWQMS are as follows:</p>
          <ul>
            <li>Monitoring Location ID</li>
            <li>The date the water was collected (Activity Start Date)</li>
            <li>The time the water was collected (Activity Start Time)</li>
            <li>The depth when the water was collected (Activity Depth/Height)</li>
            <li>The units for the depth when the water was collected (Activity Depth/Height Unit)</li>
            <li>The analyte (Characteristic Name)</li>
            <li>The fraction (Sample Fraction)</li>
            <li>The numerical value of the lab reported result (Result Value)</li>
            <li>The units of the numerical result value (Result Unit)</li>
            <li>Result qualifiers (Result Comment)</li>
            <li>Detection limit type, value, and unit</li>
              <ul>
                <li>Detection Limit Type 1</li>
                <li>Detection Limit Value 1</li>
                <li>Detection Limit Unit 1</li>
                <li>Detection Limit Type 2</li>
                <li>Detection Limit Value 2</li>
                <li>Detection Limit Unit 2</li>
              </ul>
            </ul>
             '),
        userColnamesUI("fpm_id", selectize_lists)
      ),
      nav_panel("Step 3",
        radioButtons("TSI_radio_id", "Would you like to compute the Trophic State Index?",
                     choices = list("Yes", "No"), selected = "No"),
        conditionalPanel(
          condition = "input.TSI_radio_id == 'Yes'",
          p("Please upload lake macrophyte coverage data and select the required columns:"),
          br(),
          fileInputUI("macrophyte_upload"),
          p("Select columns:"),
          userColnamesUI("ucm_id", macrophyte_selectize_lists, col_width = 3),
          br(),
          textOutput("mac_col_validation"),
          p("A preview of the file you upload will appear below"),
          br(),
          tableOutput("uploaded_mac_preview")
        ),
      ),
      nav_panel("Step 4",
        #debuggingTextUI("DS_id"),
        #anDebuggingTextUI("AN_id"),
        #sulfatesDebuggingTextUI("sulfates_id"),
        # chronicDebuggingTextUI("chronic_id"),
        input_task_button("analysis", "Run Analysis"),
        hidden(
          downloadButton("download_results", "Download Results")
        )
        # useSupportDebuggingUI("assess_use"),
        # useSupportUI("assess_use")
        # canDebuggingText("CAN_id"),
        #debuggingUI("DS_id")
        #anDebuggingUI("AN_id")
        #thermoclineDebuggingUI("thermocline_id"),
        # tsiSiteReqDebuggingUI("tsiSR_id")
      )
      # nav_panel("All Data",
      #   navset_card_tab(
      #     nav_panel("SV",
      #       svStandardsUI("LAS_id")
      #     ),
      #     nav_panel("Dependent",
      #       depDebuggingUI("DS_id")
      #     ),
      #     nav_panel("AN",
      #       anDebuggingUI("AN_id")
      #     ),
      #     nav_panel("Sulfates",
      #       sulfatesDebuggingUI("sulfates_id")
      #     ),
      #     nav_panel("Chronic",
      #       chronicDebuggingUI("chronic_id")
      #     ),
      #     nav_panel("Chronic AN",
      #       canDebuggingUI("CAN_id")
      #     ),
      #     nav_panel("TSI",
      #       tsiAnalysisDebuggingUI("tsiA_id")
      #     )
      #   )        
      # )
    )
    )
  )
)