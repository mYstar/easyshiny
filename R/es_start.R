#' @title Start Easyshiny App.
#' @description Create the Shiny server and UI code and start the server.
#' The Shiny project will contain all previously added data and visualization.
#'
#' @param title The title to insert in the main menu.
#'
#' @return The output from \code{\link{shiny::shinyApp}}.
#' @importFrom shiny shinyApp
#' @importFrom dplyr tbl_df mutate
#' @importFrom magrittr %>%
#' @export
es_start <- function(title='Easy Shiny Project') {

  files <- get('files', envir = appData)
  visuals <- get('visuals', envir = appData)

  ui <- es_build_ui(title, visuals)
  server <- function(input, output) {

      # create the filereader functions
      if(nrow(files) > 0) {
        for(idx in 1:nrow(files)) {
          assign(
            files[idx,]$name,
            eval(files[idx,]$reader)
          )
        }
      }

      # render all the visuals
      if(length(visuals) > 0) {
        apply(visuals, 1, function(line) {
            output[[line$id]] <- eval(line$expr)
            output[[paste0('win_', line$id)]] <- eval(line$expr)
          }
        )
      }
    }
  shiny::shinyApp(ui = ui, server = server)
}
