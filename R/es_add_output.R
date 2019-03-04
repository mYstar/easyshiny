#' @title Add Output Object
#' @description Adds an output object from shiny to the Easy Shiny app and places it into a specific tab and box. All the
#'   wrapper functions pass all their arguments to \code{es_add_output} (useful for specifying tabs and boxes).
#'
#' @param render_call a call to a shiny render function, that generates an output (like \code{\link{renderPlot}}),
#'   the corresponding output function will be automatically chosen
#' @param ui_function a shiny UI function, that has to be corresponding to the function used in \code{render_call}
#' @param ... the parameters, that should be given to the output function
#' @param tab tab to show the plot in (default: 'Output', new name creates tab)
#' @param box box in the view area to show plot in (default: 'Result', new name creates box)
#'
#' @importFrom checkmate assert_string assert_function
#' @importFrom shiny plotOutput dataTableOutput imageOutput verbatimTextOutput tableOutput textOutput htmlOutput
es_add_output <- function(render_call, ui_function, tab = 'Output', box = 'Result', ...) {
  # argument check
  assert_function(ui_function)
  assert_string(tab)
  assert_string(box)

  es_add(
    render_call = render_call,
    ui_call = substitute(ui_function(outputId = 'output_')),
    tab,
    box,
    resize = TRUE,
    ...
    )
}

#' @describeIn es_add_output
#'
#' adds a shiny plot to the app
#'
#' @param expr the expression to pass to the outputFunction (should be generating the correct object type)
#' @export
es_renderPlot <- function(expr, ...) {
  es_add_output(substitute(renderPlot(expr)), plotOutput, ...)
  return( function() { expr } )
  }

#' @describeIn es_add_output
#'
#' adds a shiny datatable to the app
#'
#' @param expr the expression to pass to the outputFunction (should be generating the correct object type)
#' @export
es_renderDataTable <- function(expr, ...) { es_add_output(substitute(renderDataTable(expr)), dataTableOutput, ...) }

#' @describeIn es_add_output
#'
#' adds a shiny image to the app
#' @export
es_renderImage <- function(expr, ...) { es_add_output(substitute(renderImage(expr)), imageOutput, ...) }

#' @describeIn es_add_output
#'
#' prints the results in the app
#' @export
es_renderPrint <- function(expr, ...) { es_add_output(substitute(renderPrint(expr)), verbatimTextOutput, ...) }

#' @describeIn es_add_output
#'
#' adds a shiny table to the app
#' @export
es_renderTable <- function(expr, ...) { es_add_output(substitute(renderTable(expr)), tableOutput, ...) }

#' @describeIn es_add_output
#'
#' adds a shiny text to the app
#' @export
es_renderText <- function(expr, ...) { es_add_output(substitute(renderText(expr)), textOutput, ...) }

#' @describeIn es_add_output
#'
#' adds shiny UI elements to the app
#' @export
es_renderUI <- function(expr, ...) { es_add_output(substitute(renderUI(expr)), htmlOutput, ...) }
