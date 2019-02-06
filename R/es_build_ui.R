#' @title Build UI
#' @description Builds the UI skeleton for the app.
#'
#' @param title The title to insert in the main menu.
#' @param visuals The visuals to use for the UI
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu
#'   menuItem dashboardBody tabItems tabItem box
#' @importFrom shiny tags fileInput icon HTML
#' @importFrom magrittr %>%
#' @importFrom rlist list.prepend
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

#' @title Build Objects
#' @description Builds the objects for the UI. To use in a \code{\link[shinydashboard]{box}}.
#'
#' @param objects a matrix of object descriptions (from the visuals)
#'
#' @return a list of shiny \code{\link[shinydashboard]{box}}es
#'
#' @importFrom magrittr %>%
#' @importFrom shiny a
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsModal
es_build_objects <- function(objects) {
  apply( objects, 1, function(obj) {
    switch(obj$type,
           output={
             ui_function <- obj$ui_func
             list(
               bsModal(paste0('win_', obj$id), '', paste0('link_', obj$id), size='large', ui_function(outputId=paste0('win_',obj$id))),
               a( href='#', id=paste0('link_', obj$id), ui_function(outputId=obj$id) %>% withSpinner() )
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

#' @title Extract column
#' @description helper to extract a column from a \code{\link{matrix}}
#'
#' @param mat a \code{\link{matrix}}
#' @param colname the name of the column to extract
#'
#' @return a \code{\link{vector}} with the data from the column
mat_col <- function(mat, colname) {
  unlist(mat[, colname])
}
