#' @title Convert summaries
#' @description Converts a list containing output from \code{\link{summary}} into a html format usable by shiny.
#'
#' @param summaries a list of summary objects
#'
#' @return html: a paragraph for every summary containing the computed values
#'
#' @importFrom checkmate assert_list
#' @importFrom magrittr %>%
#' @importFrom dplyr rowwise mutate
#' @export
es_summaries_to_html <- function( summaries ) {
  # check arguments
  assert_list(summaries, types = c('summaryDefault'), any.missing = F, min.len = 1)

  # build the hmtl output
  summaries <- summaries %>%
    rowwise() %>%
    mutate( text = tags$p(
      "Min:", summary['Min.'] %>% round(digits=2),
      ", 1st Qu.:", summary['1st Qu.'] %>% round(digits=2),
      ", Median:", summary['Median'] %>% round(digits=2),
      ", Mean:", summary['Mean'] %>% round(digits=2),
      ", 3rd Qu.:", summary['3rd Qu.'] %>% round(digits=2),
      ", Max:", summary['Max.'] %>% round(digits=2)
      ) %>% as.character()
    )

  summaries$text %>%
    paste0( collapse = ' ' )
}

#' @title Add Shiny Object to App
#'
#' Adds the given params to the internal visuals list.
#'
#' @param render_call a call to a shiny render... function (e.g. \code{\link{renderPlot}})
#'   or NULL for input and static elements
#' @param ui_function a shiny Output function (should have an `inputId` parameter)
#' @param tab the tabname (default: 'Output')
#' @param box the boxname (default: 'Result')
#' @param resize logical, shall the object be resizable in a modal Window (shinyBS, Default: FALSE)
#'
#' @return NULL
es_add <- function(render_call, ui_function, tab, box, resize = FALSE) {

  # get an unique number for the plot
  vis_counter <- get('vis_counter', envir = appData)
  vis_counter <- vis_counter + 1
  assign('vis_counter', vis_counter, envir = appData)

  # add the plot to the visuals
  vis_matrix <- get('visuals', envir = appData)
  vis_matrix <- rbind(
    vis_matrix,
    list(
      id=paste0('object_', vis_counter),
      render_func=render_call,
      ui_func=ui_function,
      tab=tab,
      box=box,
      resize=resize
      )
    )
  assign('visuals', vis_matrix, envir = appData)

}
#' @title Add Output Object
#' @description Adds a ouput object from shiny to the Easy Shiny app and places it into a specific tab and box.
#'
#' @param renderFunction a shiny render function, that generates an output (like \code{\link{renderPlot}}),
#'   the corresponding output function will be automatically chosen
#' @param expr the expression to pass to the outputFunction (should be generating the correct object type)
#' @param ... the parameters, that should be given to the output function
#' @param tab tab to show the plot in (default: 'Output', new name creates tab)
#' @param box box in the view area to show plot in (default: 'Result', new name creates box)
#'
#' @importFrom checkmate assert_string assert_function
#' @importFrom shiny plotOutput dataTableOutput imageOutput verbatimTextOutput tableOutput textOutput htmlOutput
#' @export
es_add_output <- function(renderFunction, expr, tab = 'Output', box = 'Result', ...) {
  # argument check
  assert_function(renderFunction)
  assert_string(tab)
  assert_string(box)

  # internal data structure
  # is used to determine the right UI output element for each es_add_output call
  translate_output_UI <- list(
    renderPlot = plotOutput,
    renderDataTable = dataTableOutput,
    renderImage = imageOutput,
    renderPrint = verbatimTextOutput,
    renderTable = tableOutput,
    renderText = textOutput,
    renderUI = htmlOutput
  )

  # determine UI element
  render_function_name <- as.character(substitute(renderFunction))
  ui_function <- translate_output_UI[[render_function_name]]
  assert_function(ui_function)

  es_add(
    render_call = substitute(renderFunction(expr)),
    ui_function = ui_function,
    tab,
    box,
    resize = TRUE,
    ...
    )

  return(function() {expr})
}

#' @title  Adds a shiny input object to the UI.
#'
#' @description Useful for inputs (e.g. \code{\link{textInput}}).
#' The input variables can be used in shiny style (\code{input$variable}) in the output functions.
#' Do not use this function to create output objects, as they are registered in a different way.
#' The specialized functions (e.g. \code{\link{es_add_output}}) can be used for that.
#'
#' @param shinyfunction a shiny function to call to create the object
#' @param tab tab to show the object in (new name creates tab)
#' @param box box in the view area to show object in (new name creates box)
#' @param ... the arguments to deliver to the shiny function (\code{inputId} and \code{value} create
#' an entry in the \code{input$inputId} list useful in console mode)
#'
#' @importFrom checkmate assert_string assert_function
#' @export
es_add_input <- function(shinyfunction, tab = 'output', box = 'objects', ...) {
  # argument check
  assert_function(shinyfunction, args = c('inputId', 'label'))
  assert_string(tab)
  assert_string(box)

  es_add(
    render_call = NULL,
    ui_function = substitute(shinyfunction(...)),
    tab = tab,
    box = box
    )

  # add the default value to the input$ list in globalenv
  arguments <- list(...)
  if(!is.null(arguments$inputId) && !is.null(arguments$value)) {
    input <- get('input', envir = globalenv())
    input[[arguments$inputId]] <- arguments$value
    assign('input', input, envir = globalenv())
  }
}
