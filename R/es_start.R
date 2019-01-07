options(shiny.maxRequestSize=500*1024^2) # max filesize 500MB
options(spinner.type=8) # shinycssspinner type

appData <- new.env()
assign('visuals', list(), envir = appData)
assign('vis_counter', 0, envir = appData)

#' @title Start Easyshiny App.
#' @description Create the Shiny server and UI code and start the server.
#' The Shiny project will contain all previously added data and visualization.
#'
#' @param title The title to insert in the main menu.
#'
#' @return The output from \code{\link{shiny::shinyApp}}.
#' @importFrom shiny shinyApp
#' @export
es_start <- function(title='Easy Shiny Project') {

  visuals <- get('visuals', envir = appData)
  ui <- es_build_ui(title, visuals)
  server <- function(input, output) {
      apply(visuals, 1, function(line) {
          output[[line$id]] <- renderPlot(line$expr)
        }
      )
    }
  shiny::shinyApp(ui = ui, server = server)
}

#' Builds the UI skeleton for the app.
#'
#' @param title The title to insert in the main menu.
#' @param visuals The visuals to use for the UI
#'
#' @import shinydashboard
#' @import shiny
#' @return html + js code with the needed shiny directives
es_build_ui <-  function(title, visuals) {
  dashboardPage(
    dashboardHeader(title = title),
    dashboardSidebar(
      sidebarMenu(
        menuItem('input', tabName = 'input', icon = icon('folder')),
        menuItem('output', tabName = 'output', icon = icon('gears'))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(HTML('
                        .content-wrapper { height: 90vh; overflow-y: auto; }
                        .modal-lg { width: 95%; }
                        ' )
        )
      ),
      tabItems(
        tabItem(tabName='input',
                box(
                  status = 'danger',
                  fileInput(inputId = 'simfiles1',
                            label = '1: choose simulation files',
                            multiple = TRUE),
                  textInput(inputId = 'fileset1_name',
                            label='choose a name for the dataset:',
                            value='fileset 1'),
                  actionButton( inputId = 'fileset1_button',
                                label = 'OK',
                                icon = icon('check') )
                )
        ),
        tabItem(tabName='output',
                apply(visuals, 1, function(vis) {
                  box(
                    width = 12,
                    status = "info",
                    title=vis$id,
                    plotOutput(outputId=vis$id)
                  )
                })
        )
      )
    )
  )
}
