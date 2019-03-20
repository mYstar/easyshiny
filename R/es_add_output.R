#' @title Add Output Object
#' @description Adds an output object from shiny to the Easy Shiny app and places it into a specific tab and box
#'   after the previously inserted elements. All the wrapper functions pass all their arguments to
#'   \code{es_add_output} (useful for specifying tabs and boxes).
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
es_add_output <- function(render_call, ui_function, tab = NULL, box = NULL, ...) {
  # argument check
  assert_function(ui_function)
  assert_string(tab, null.ok = TRUE)
  assert_string(box, null.ok = TRUE)

  if(is.null(tab))
    tab <- 'Output'
  if(is.null(box))
    box <- 'Result'

  es_add(
    render_call = render_call,
    ui_call = substitute(ui_function(outputId = 'output_')),
    tab,
    box,
    resize = TRUE,
    ...
    )
}

#' @title Add Output Object
#'
#' @description Adds an output object from shiny to the Easy Shiny app and places it into a specific tab and box. All the
#'   wrapper functions pass all their arguments to \code{es_add_output} (useful for specifying tabs and boxes).
#'
#' @param expr the expression to pass to the outputFunction (should be generating the correct object type)
#' @param tab tab to show the plot in (default: 'Output', new name creates tab)
#' @param box box in the view area to show plot in (default: 'Result', new name creates box)
#' @param ... the parameters, that should be given to the output function
#' @export
es_renderPlot <- function(expr, tab=NULL, box=NULL, ...) {
  es_add_output(substitute(renderPlot(expr)), plotOutput, tab, box, ...)
  return( function() { expr } )
  }

#' @describeIn es_renderPlot
#'
#' adds a shiny datatable to the app
#' @export
es_renderDataTable <- function(expr, tab=NULL, box=NULL, ...) { es_add_output(substitute(renderDataTable(expr)), dataTableOutput, tab, box, ...)
  return( function() { expr } )
  }

#' @describeIn es_renderPlot
#'
#' adds a shiny image to the app
#' @export
es_renderImage <- function(expr, tab=NULL, box=NULL, ...) { es_add_output(substitute(renderImage(expr)), imageOutput, tab, box, ...)
  return( function() { expr } )
  }

#' @describeIn es_renderPlot
#'
#' prints the results in the app
#' @export
es_renderPrint <- function(expr, tab=NULL, box=NULL, ...) { es_add_output(substitute(renderPrint(expr)), verbatimTextOutput, tab, box, ...)
  return( function() { expr } )
  }

#' @describeIn es_renderPlot
#'
#' adds a shiny table to the app
#' @export
es_renderTable <- function(expr, tab=NULL, box=NULL, ...) { es_add_output(substitute(renderTable(expr)), tableOutput, tab, box, ...)
  return( function() { expr } )
  }

#' @describeIn es_renderPlot
#'
#' adds a shiny text to the app
#' @export
es_renderText <- function(expr, tab=NULL, box=NULL, ...) { es_add_output(substitute(renderText(expr)), textOutput, tab, box, ...)
  return( function() { expr } )
  }

#' @describeIn es_renderPlot
#'
#' adds shiny UI elements to the app
#' @export
es_renderUI <- function(expr, tab=NULL, box=NULL, ...) { es_add_output(substitute(renderUI(expr)), htmlOutput, tab, box, ...)
  return( function() { expr } )
  }
