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
#' @return
#' @export
#'
#' @examples
es_add_plot <- function(plot, tab = 'Output', box = 'Data') {

  # get an unique number for the plot
  vis_counter <- get('vis_counter', envir = appData)
  vis_counter <- vis_counter + 1
  assign('vis_counter', vis_counter, envir = appData)

  # add the plot to the visuals
  vis_table <- get('visuals', envir = appData)
  vis_table <- rbind(
    vis_table,
    list(
      id=paste0('plot', vis_counter),
      expr=substitute( renderPlot(plot) )
      )
    )
  assign('visuals', vis_table, envir = appData)
}
