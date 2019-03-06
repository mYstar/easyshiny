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
#' @param ui_call a shiny Output function call (should have a valid `inputId`)
#' @param tab the tabname (default: 'Output')
#' @param box the boxname (default: 'Result')
#' @param resize logical, shall the object be resizable in a modal Window (shinyBS, Default: FALSE)
#' @param ... arguments to pass to the \code{ui_function}
#' @return NULL
#'
#' @importFrom checkmate assert_string assert_logical
es_add <- function(render_call, ui_call, tab, box, resize = FALSE, ...) {
  # parameter check
  assert_string(tab)
  assert_string(box)
  assert_logical(resize)

  # get an unique number for the plot
  vis_counter <- get('vis_counter', envir = appData)
  vis_counter <- vis_counter + 1
  assign('vis_counter', vis_counter, envir = appData)
  if(!is.null(ui_call$outputId)) {
    object_id <- paste0(ui_call$outputId, vis_counter)
    ui_call$outputId <- object_id
  }
  else
    object_id <- paste0('object_', vis_counter)

  # add the plot to the visuals
  vis_matrix <- get('visuals', envir = appData)
  vis_matrix <- rbind(
    vis_matrix,
    list(
      id=object_id,
      render_func=render_call,
      ui_func=ui_call,
      tab=tab,
      box=box,
      resize=resize
      )
    )
  assign('visuals', vis_matrix, envir = appData)
}

#' @title  Adds a shiny input object to the UI.
#'
#' @description Useful for inputs (e.g. \code{\link{textInput}}).
#' The input variables can be used in shiny style (\code{input$variable}) in the output functions.
#' Do not use this function to create output objects, as they are registered in a different way.
#' The specialized functions (e.g. \code{\link{es_renderPlot}}) can be used for that.
#'
#' @param shinyfunction a shiny function to call to create the object
#' @param tab tab to show the object in (new name creates tab)
#' @param box box in the view area to show object in (new name creates box)
#' @param ... the arguments to deliver to the shiny function (\code{inputId} and \code{value} create
#' an entry in the \code{input$inputId} list useful in console mode)
#'
#' @importFrom checkmate assert_string assert_function
#' @export
es_add_input <- function(shinyfunction, tab = 'Output', box = 'Result', ...) {
  # argument check
  assert_function(shinyfunction, args = c('inputId', 'label'))
  assert_string(tab)
  assert_string(box)

  es_add(
    render_call = NULL,
    ui_call = substitute(shinyfunction(...)),
    tab = tab,
    box = box,
    ...
    )

  # add the default value to the input$ list in globalenv
  arguments <- list(...)
  if(!is.null(arguments$inputId) && !is.null(arguments$value)) {
    input <- get('input', envir = globalenv())
    input[[arguments$inputId]] <- arguments$value
    assign('input', input, envir = globalenv())
  }
}

#' @title Add Static Component
#' @description Adds a static shiny component to the easyshiny app.
#'   The object can be composed of different nested shiny calls and will be placed in the given tab and box.
#'
#' @param static_call a call to a \code{\link[shiny]{tags}} element or a corresponding wrapper
#' @param tab the tab to place the static element in
#' @param box the box to place the static element in
#'
#' @importFrom checkmate assert_string
#' @export
es_add_static <- function(static_call, tab = 'Output', box = 'Result') {
  # param check
  assert_string(tab)
  assert_string(box)

  # add to visuals
  es_add(render_call = NULL, ui_call = substitute(static_call), tab = tab, box = box, resize = FALSE)
}
