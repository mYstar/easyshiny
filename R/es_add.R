library(dplyr)
library(shiny)

# --- util ---
summaries_to_html <- function( summaries ) {
  # build the hmtl output
  summaries <- summaries %>%
    ungroup() %>%
    mutate( setname = setname %>% as.character ) %>%
    group_by( setname) %>%
    mutate( header = case_when( (setname != lag(setname , default='')) ~ setname,
                               TRUE ~ '') ) %>%
    ungroup

  summaries <- summaries %>%
    rowwise() %>%
    mutate( text = tags$p(
      tags$h5(header),
      tags$b(prodnum),
      tags$b(lifternumber),
      tags$b(machine),
      "Min:", summary['Min.'] %>% round(digits=2),
      ", 1st Qu.:", summary['1st Qu.'] %>% round(digits=2),
      ", Median:", summary['Median'] %>% round(digits=2),
      ", Mean:", summary['Mean'] %>% round(digits=2),
      ", 3rd Qu.:", summary['3rd Qu.'] %>% round(digits=2),
      ", Max:", summary['Max.'] %>% round(digits=2),
      '(',
      max_prod,
      max_time,
      ')'
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
