library(shiny)

### ---------------------------------------------- ###
### reading input data
### ---------------------------------------------- ###

## read all the user input
# default values
filesets <- reactiveValues(
    set1_name = 'fileset1_name',
    set2_name = 'fileset2_name',
    data = NULL
  )
# observer on the OK buttons
observeEvent( c(
    input$fileset1_button,
    input$fileset2_button
  ),
  {
    # read the names of the filesets
    filesets$set1_name <- input$fileset1_name
    filesets$set2_name <- input$fileset2_name
    filesets$data <- input$simfiles1 %>% tbl_df %>% mutate(n='1') %>%
        bind_rows( input$simfiles2 %>% tbl_df %>% mutate(n='2') )
  }
)
