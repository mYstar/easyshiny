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
          lapply( visuals[,'tab'] %>% unique(), function(tabname) {
            tabItem(tabName=tabname,
              lapply( visuals[visuals %>% mat_col('tab') == tabname,'box'] %>% unique(), function(boxname) {
                  box(
                    width = 12,
                    status = "info",
                    title=boxname,
                    es_build_objects(visuals[visuals %>% mat_col('tab') == tabname & visuals %>% mat_col('box') == boxname, ,drop=FALSE])
             ) } ) # 2nd lapply (boxes)
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

#' Builds the objects for the UI. To use in a box.
#'
#' @param objects a matrix of object descriptions (from the visuals)
#'
#' @return a list of shiny boxes
es_build_objects <- function(objects) {
  apply( objects, 1, function(obj) {
    switch(obj$type,
           plot={
             list(
               bsModal(paste0('win_', obj$id), '', paste0('link_', obj$id), size='large', plotOutput(outputId=paste0('win_',obj$id))),
               a( href='#', id=paste0('link_', obj$id), plotOutput(outputId=obj$id) %>% withSpinner() )
             )
           },
           object={
             list(
               eval(obj$expr)
             )
           }
         )
  } ) %>%
  unlist(recursive = FALSE)
}

#' helper to extract a column from a matrix
#'
#' @param mat a matrix
#' @param colname the name of the column to extract
#'
#' @return a vector with the data from the column
mat_col <- function(mat, colname) {
  unlist(mat[, colname])
}
