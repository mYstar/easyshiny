appData <- new.env()

#' Initializes and resets all datastructures used by easyshiny.
#'
#' @export
es_init <- function() {
  assign('visuals', list(), envir = appData)
  assign('files', list(), envir = appData)
  assign('vis_counter', 0, envir = appData)
}
