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
                  fileInput(inputId = 'files1',
                            label = '1: choose files',
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
          if(length(visuals) > 0) {
            apply(visuals, 1, function(vis) {
              box(
                width = 12,
                status = "info",
                title=vis$id,
                plotOutput(outputId=vis$id)
              )
            })
          }
        )
      )
    )
  )
}
