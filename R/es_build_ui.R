#' Builds the UI skeleton for the app.
#'
#' @param title The title to insert in the main menu.
#' @param visuals The visuals to use for the UI
#'
#' @import shinydashboard
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rlist list.prepend
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsModal
#' @return html + js code with the needed shiny directives
es_build_ui <-  function(title, visuals) {
  dashboardPage(
    dashboardHeader(title = title),
    dashboardSidebar(
      sidebarMenu(
        menuItem('input', tabName = 'input', icon = icon('folder')),
        if(length(visuals) > 0) {
          lapply(visuals[, 'tab'] %>% unique() , function(tname) {
            menuItem(tname, tabName = tname, icon = icon('gears'))
          })
        }
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
      do.call(tabItems,
          lapply( visuals[,'tab'] %>% unique(), function(tab) {
            tabItem(tabName=tab,
              lapply( visuals[,'box'] %>% unique(), function(boxname) {
                if(boxname %in% visuals[visuals[,"tab"] == tab,, drop = F][,'box']) {
                  box(
                    width = 12,
                    status = "info",
                    title=boxname,
                    apply( visuals, 1, function(vis) {
                      if(vis$tab==tab & vis$box == boxname) {
                        list(
                          bsModal(paste0('win_', vis$id), '', paste0('link_', vis$id), size='large', plotOutput(outputId=paste0('win_',vis$id))),
                          a( href='#', id=paste0('link_', vis$id), plotOutput(outputId=vis$id) %>% withSpinner() )
                        )
                      }
                  } # 3rd apply (visuals)
                 ) %>%
                unlist(recursive = FALSE)
             ) } } ) # 2nd lapply (boxes)
          ) } )  %>% # 1st lapply (tabs)
          list.prepend(
            tabItem(tabName='input',
                    box(
                      status = 'danger',
                      fileInput(inputId = 'files1',
                                label = '1: choose files',
                                multiple = TRUE)
                    )
              )
          )
      ) # do.call
    )
  )
}
