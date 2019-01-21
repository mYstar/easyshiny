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

      ## read all the user input
      # default values
      filesets <- reactiveValues(
          set1_name = 'fileset1_name',
          data = NULL
        )

      # observer on the OK buttons
      observeEvent( input$fileset1_button,
        {
          # read the names of the filesets
          filesets$set1_name <- input$fileset1_name
          filesets$data <- input$files1 %>% tbl_df %>% mutate(n='1')
        }
      )

      # create the filereader functions
      if(nrow(files) > 0) {
        for(idx in 1:nrow(files)) {
          assign(
            files[idx,]$name,
            reactive( { es_read_files( filesets$data, files[idx,]$name, function(data) {data} ) } )
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
