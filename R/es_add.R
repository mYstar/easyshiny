library(dplyr)
library(shiny)

#' Converts a list containing output from \code{\link{summary}} into a html format usable by shiny.
#'
#' @param summaries a list of summary objects
#'
#' @return html: a paragraph for every summary containing the computed values
#' @export
es_summaries_to_html <- function( summaries ) {
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

#' Adds a plot to the Easy Shiny app.
#'
#' @param plot expression, that generates a plot (as in \code{\link{shiny::renderPlot}})
#' @param tab tab to show the plot in (new name creates tab)
#' @param box box in the view area to show plot in (new name creates box)
#'
#' @export
es_add_plot <- function(plot, tab = 'Output', box = 'Result') {

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
}

#' Adds a shiny object to the UI. Useful for inputs (e.g. \link{\code{shiny::textInput}}).
#' The input variables can be used in shiny style (\code{input$variable}) in the output functions.
#' Do not use it for output objects, as they are registered in a different way.
#' The specialized functions (e.g. \link{\code{easyshiny::add_plot}}) can be used for that.
#'
#' @param object a shiny object to insert
#' @param tab tab to show the object in (new name creates tab)
#' @param box box in the view area to show object in (new name creates box)
#'
#' @export
es_add_object <- function(object, tab = 'output', box = 'objects') {

  # get an unique number for the plot
  vis_counter <- get('vis_counter', envir = appData)
  vis_counter <- vis_counter + 1
  assign('vis_counter', vis_counter, envir = appData)

  vis_matrix <- get('visuals', envir = appData)
  vis_matrix <- rbind(
    vis_matrix,
    list(
      id=paste0('object_', vis_counter),
      expr=substitute( object ),
      tab=tab,
      box=box,
      type='object'
      )
    )
  assign('visuals', vis_matrix, envir = appData)
}
