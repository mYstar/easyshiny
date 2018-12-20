options(shiny.maxRequestSize=500*1024^2) # max filesize 500MB
options(spinner.type=8) # shinycssspinner type

### filenames of the statisics from Visual Components
file.machines_states='statistic'
file.machines_boxes='statistic_machines'
file.transferlids='statistic_transferlids'
file.qc_cycles='statistic_qc_cycles'
file.qc_buffer='statistic_qc_buffer'
file.box_stacker='statistic_stacker'
file.lifter='statistic_lifter_states'
file.warehouse_pallets='statistic_warehouse_pallets'
file.loop='statistic_conveyor_boxes'
file.montage_manual='statistic_montage_manual'
file.montage_auto='statistic_montage_auto'
file.caps='statistic_caps'

#' @title Start Easyshiny App.
#' @description Create the Shiny server and UI code and start the server.
#' The Shiny project will contain all previously added data and visualization.
#'
#' @param title The title to insert in the main menu.
#'
#' @return The output from \code{\link{shiny::runApp}}.
#' @export
es_start <- function(title='Easy Shiny Project') {
  print('not implemented jet.')
}
