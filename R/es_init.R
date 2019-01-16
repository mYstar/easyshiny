appData <- new.env()

#' Initializes and resets all datastructures used by easyshiny.
#'
#' @export
es_init <- function() {
  visuals <- matrix(ncol = 4)
  colnames(visuals) <- c('id', 'expr', 'tab', 'box')
  visuals <- visuals[!is.na(visuals[,'id']),]

  assign('visuals', visuals, envir = appData)
  assign('files', list(), envir = appData)
  assign('vis_counter', 0, envir = appData)
}
