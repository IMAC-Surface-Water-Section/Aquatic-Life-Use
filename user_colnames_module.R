userColnamesUI <- function(id, selectize_lists_input, col_width = 4){
  
  ns <- NS(id)
  
  # apply namespace to each inputId for selectizeInput
  selectize_lists_namespaced <- map(selectize_lists_input, function(x){
    x$inputId <- ns(x$inputId)
    return(x)
  })
  
  layout_column_wrap(width = 1 / col_width, !!!map(
    selectize_lists_namespaced, ~ do.call(selectizeInput, .x)
  ))
}

userColnamesServer <- function(id, data, possible_colnames = NULL){
  moduleServer(id, function(input, output, session){
    
    observe({
      req(data())
      
      user_supplied_colnames <- colnames(data())
      
      if(!is.null(possible_colnames)){
        walk(1:nrow(possible_colnames), function(i){
          updateSelectizeInput(
            session = session,
            inputId = possible_colnames$id[i],
            choices = user_supplied_colnames,
            selected = possible_colnames$names[i]
          )
        })
      }
    })
    
    # return selected columns as a reactive
    selected_columns <- reactive({
      req(possible_colnames)
      
      col_mappings <- sapply(possible_colnames$ids, function(id){
        input[[id]]
      })
      
      col_mappings <- col_mappings[!is.null(col_mappings) & col_mappings != ""]
      
      return(col_mappings)
    })
    
    return(selected_columns)
  })
}