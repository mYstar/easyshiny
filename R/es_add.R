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

#' @title Add Plot
#' @description Adds a plot to the Easy Shiny app and places it into a specific tab and box.
#'
#' @param plot expression, that generates a plot (as in \code{\link{renderPlot}})
#' @param tab tab to show the plot in (new name creates tab)
#' @param box box in the view area to show plot in (new name creates box)
#'
#' @importFrom checkmate assert_string
#' @export
es_add_plot <- function(plot, tab = 'Output', box = 'Result') {
  # argument check
  assert_string(tab)
  assert_string(box)

  # get an unique number for the plot
  vis_counter <- get('vis_counter', envir = appData)
  vis_counter <- vis_counter + 1
  assign('vis_counter', vis_counter, envir = appData)

  # add the plot to the visuals
  vis_matrix <- get('visuals', envir = appData)
  vis_matrix <- rbind(
    vis_matrix,
    list(
      id=paste0('plot', vis_counter),
      expr=substitute( renderPlot(plot) ),
      tab=tab,
      box=box,
      type='plot'
      )
    )
  assign('visuals', vis_matrix, envir = appData)

  return(function() {plot})
}

#' @title  Adds a shiny object to the UI.
#'
#' @description Useful for inputs (e.g. \code{\link{textInput}}).
#' The input variables can be used in shiny style (\code{input$variable}) in the output functions.
#' Do not use it for output objects, as they are registered in a different way.
#' The specialized functions (e.g. \code{\link{es_add_plot}}) can be used for that.
#'
#' @param shinyfunction a shiny function to call to create the object
#' @param tab tab to show the object in (new name creates tab)
#' @param box box in the view area to show object in (new name creates box)
#' @param ... the arguments to deliver to the shiny function (\code{inputId} and \code{value} create
#' an entry in the \code{input$list} useful for console mode)
#'
#' @importFrom checkmate assert_string assert_function
#' @export
es_add_object <- function(shinyfunction, tab = 'output', box = 'objects', ...) {
  # argument check
  assert_function(shinyfunction, args = c('inputId', 'label'))
  assert_string(tab)
  assert_string(box)

  # get an unique number for the plot
  vis_counter <- get('vis_counter', envir = appData)
  vis_counter <- vis_counter + 1
  assign('vis_counter', vis_counter, envir = appData)

  # update the matrix of visuals
  vis_matrix <- get('visuals', envir = appData)
  vis_matrix <- rbind(
    vis_matrix,
    list(
      id=paste0('object_', vis_counter),
      expr=substitute( shinyfunction(...) ),
      tab=tab,
      box=box,
      type='object'
      )
    )
  assign('visuals', vis_matrix, envir = appData)

  # add the default value to the input$ list in globalenv
  arguments <- list(...)
  if(!is.null(arguments$inputId) && !is.null(arguments$value)) {
    input <- get('input', envir = globalenv())
    input[[arguments$inputId]] <- arguments$value
    assign('input', input, envir = globalenv())
  }
}
