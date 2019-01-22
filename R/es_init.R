library(shinycssloaders)

appData <- new.env()

#' Initializes and resets all datastructures used by easyshiny.
#'
#' @import shinycssloaders
#' @export
es_init <- function() {
  visuals <- matrix(ncol = 5)
  colnames(visuals) <- c('id', 'expr', 'tab', 'box', 'type')
  visuals <- visuals[!is.na(visuals[,'id']),]
  assign('visuals', visuals, envir = appData)
  assign('vis_counter', 0, envir = appData)

  assign('files', list(), envir = appData)
}
