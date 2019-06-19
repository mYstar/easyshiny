#' @title Create Reactive Value
#' @description Makes a calue usable for calculations.
#'
#' @param id the name of the reactive value
#' @param expression the expression to evaluate for the value
#'
#' @importFrom checkmate assert_string
#' @export
es_reactive <- function(id, expression) {
  # parameter checking
  assert_string(id)

  reactive_table <- get('reactives', envir = appData)

  reactive_table <- rbind(
    reactive_table,
    list(
      id=id,
      rvalue=substitute( reactive({expression}))
      )
    )
  assign('reactives', reactive_table, envir = appData)
}
