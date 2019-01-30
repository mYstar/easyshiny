library(shinycssloaders)

appData <- new.env()

#' Initializes and resets all datastructures used by easyshiny.
#'
#' @param local_folder a folder on the computer running the server, containing
#' input files to test the easyshiny program locally (console mode)
#'
#' @import shinycssloaders
#' @export
es_init <- function(local_folder = NULL) {
  visuals <- matrix(ncol = 5)
  colnames(visuals) <- c('id', 'expr', 'tab', 'box', 'type')
  visuals <- visuals[!is.na(visuals[,'id']),]
  assign('visuals', visuals, envir = appData)
  assign('vis_counter', 0, envir = appData)

  assign('console_fileset', es_read_filesets(local_folder), envir = appData)
  assign('files', list(), envir = appData)

  assign('input', list(), envir = globalenv())
}
