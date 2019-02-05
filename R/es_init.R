library(shinycssloaders)

appData <- new.env()

#' @title Easyshiny Initialization
#' @description  Initializes and resets all datastructures used by easyshiny.
#'
#' @param local_folder a folder on the computer running the server, containing
#' input files to test the easyshiny program locally (console mode)
#'
#' @import shinycssloaders
#' @importFrom checkmate assert check_directory_exists check_null
#' @export
es_init <- function(local_folder = NULL) {
  # argument checks
  assert(
    check_directory_exists(local_folder, access = 'r'),
    check_null(local_folder)
  )

  visuals <- matrix(ncol = 6)
  colnames(visuals) <- c('id', 'expr', 'tab', 'box', 'ui_func', 'type')
  visuals <- visuals[!is.na(visuals[,'id']),]
  assign('visuals', visuals, envir = appData)
  assign('vis_counter', 0, envir = appData)

  assign('console_fileset', es_read_filesets(local_folder), envir = appData)
  assign('files', list(), envir = appData)

  assign('input', list(), envir = globalenv())
}
