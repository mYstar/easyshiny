library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)

ui.build <- function() {
dashboardPage(
  dashboardHeader(title="Tubing 4.0"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("input", tabName = "input", icon = icon("folder")),
        menuItem("machines", tabName = "machines", icon = icon("gears")),
        menuItem("transferlids", tabName = "transferlids", icon = icon("exchange")),
        menuItem("quality control", tabName = "qc_cycles", icon = icon("search")),
        menuItem("warehouse", tabName = "warehouse", icon = icon("truck")),
        menuItem("box stacker", tabName = "box_stacker", icon = icon("th")),
        menuItem("lifter", tabName = "lifter", icon = icon("arrows-v")),
        menuItem("loop", tabName = "loop", icon = icon("circle-thin"))
      )
    ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .content-wrapper { height: 90vh; overflow-y: auto; }
                      .modal-lg { width: 95%; }
                      " )
      )
    ),
    tabItems(
      tabItem(tabName='input',
        box(
          status = 'danger',
          fileInput(inputId = "simfiles1",
                    label = "1: choose simulation files",
                    multiple = TRUE),
          textInput(inputId = "fileset1_name",
                    label='choose a name for the dataset:',
                    value='fileset 1'),
          actionButton( inputId = 'fileset1_button',
                        label = 'OK',
                        icon = icon('check') )
        ),
        conditionalPanel(
          condition = "output.files1_present == true",
          box(
            status = 'info',
            fileInput(inputId = "simfiles2",
                      label = "2: choose simulation files for compare",
                      multiple = TRUE),
            textInput(inputId = "fileset2_name",
                      label='choose a name for the dataset:',
                      value='fileset 2'),
            actionButton( inputId = 'fileset2_button',
                          label = 'OK',
                          icon = icon('check') )
          )
        ),
        conditionalPanel(
          condition = "output.machine1_present == true",
          box(
            title='total number of boxes at the machines',
            width=12,
            bsModal('max_boxes_total_timeseries', '', 'link_boxes_total_timeseries', size='large', plotOutput(outputId="boxes_total_timeseries2")),
            a( href='#', id='link_boxes_total_timeseries', plotOutput(outputId="boxes_total_timeseries") %>% withSpinner() ),
            uiOutput("timerange_slider")
          )
        )
      ),
      tabItem(tabName='machines',
        conditionalPanel(
          condition = "output.machine1_present == true",
          box(
            width = 12,
            status = "info",
            title='Status of the Machines',
            bsModal('max_machines_states_barplot', '', 'link_machines_states_barplot', size='large', plotOutput(outputId="machines_states_barplot2")),
            a( href='#', id='link_machines_states_barplot', plotOutput(outputId="machines_states_barplot") %>% withSpinner() )
          )
        ),
        conditionalPanel(
          condition = "output.machine1_present == true",
          box(
            status = "info",
            title='number of boxes at the machines',
            bsModal('max_machines_boxes_boxplot', '', 'link_machines_boxes_boxplot', size='large', plotOutput(outputId="machines_boxes_boxplot2")),
            a( href='#', id='link_machines_boxes_boxplot', plotOutput(outputId="machines_boxes_boxplot") %>% withSpinner() )
          ),
          box(
            status = "info",
            title='summary: boxes at the machines',
            tags$p(htmlOutput(outputId="machines_boxes_summary"))
          )
        ),
        conditionalPanel(
          condition = "output.machine1_present == true",
          box(
            width = 12,
            status = "info",
            title='daily produced tubes (mean in blue)',
            bsModal('max_machines_tubes_points', '', 'link_machines_tubes_points', size='large', plotOutput(outputId="machines_tubes_points2")),
            a( href='#', id='link_machines_tubes_points', plotOutput(outputId="machines_tubes_points") %>% withSpinner() )
          )
        )
      ),
      tabItem(tabName='transferlids',
        conditionalPanel(
          condition = "output.transferlids1_present == true",
          box(
            title='transferlids over time',
            bsModal('max_transferlids_timeseries', '', 'link_transferlids_timeseries', size='large', plotOutput(outputId="transferlids_timeseries2")),
            a( href='#', id='link_transferlids_timeseries', plotOutput(outputId="transferlids_timeseries") %>% withSpinner() )
          ),
          box(
            title='analysis transferlids',
            tags$p(htmlOutput(outputId="transferlids_summary")),
            bsModal('max_transferlids_boxplot', '', 'link_transferlids_boxplot', size='large', plotOutput(outputId="transferlids_boxplot2")),
            a( href='#', id='link_transferlids_boxplot', plotOutput(outputId="transferlids_boxplot") %>% withSpinner() )
          )
        )
      ),
      tabItem(tabName='qc_cycles',
        conditionalPanel(
          condition = "output.qc_cycles1_present == true",
          box(
            title='QC cycle time',
            width=12,
            bsModal('max_qc_cycles_timeseries', '', 'link_qc_cycles_timeseries', size='large', plotOutput(outputId="qc_cycles_timeseries2")),
            a( href='#', id='link_qc_cycles_timeseries', plotOutput(outputId="qc_cycles_timeseries") %>% withSpinner() )
          ),
          box(
            title='summary: QC cycles',
            htmlOutput(outputId="qc_cycles_summary"),
            bsModal('max_qc_cycles_boxplot', '', 'link_qc_cycles_boxplot', size='large', plotOutput(outputId="qc_cycles_boxplot2")),
            a( href='#', id='link_qc_cycles_boxplot', plotOutput(outputId="qc_cycles_boxplot") %>% withSpinner() )
          )
        ),
        conditionalPanel(
          condition = "output.qc_buffer1_present == true",
          box(
            title='QC buffer fill',
            width=12,
            bsModal('max_qc_buffer_timeseries', '', 'link_qc_buffer_timeseries', size='large', plotOutput(outputId="qc_buffer_timeseries2")),
            a( href='#', id='link_', plotOutput(outputId='qc_buffer_timeseries') %>% withSpinner() )
          ),
          box(
            title='summary: QC buffer',
            tags$p(htmlOutput(outputId="qc_buffer_summary")),
            bsModal('max_qc_buffer_boxplot', '', 'link_qc_buffer_boxplot', size='large', plotOutput(outputId="qc_buffer_boxplot2")),
            a( href='#', id='link_qc_buffer_boxplot', plotOutput(outputId='qc_buffer_boxplot') %>% withSpinner() )
          )
        )
      ),
      tabItem(tabName='box_stacker',
        conditionalPanel(
          condition = "output.stacker1_present == true",
          box(
            title='Stacker Boxes total',
            bsModal('max_stacker_timeseries', '', 'link_stacker_timeseries', size='large', plotOutput(outputId="stacker_timeseries2")),
            a( href='#', id='link_stacker_timeseries', plotOutput(outputId='stacker_timeseries') %>% withSpinner() )
          ),
          box(
            title='Box Stacker summary',
            tags$p(htmlOutput(outputId='stacker_summary')),
            bsModal('max_stacker_boxplot', '', 'link_stacker_boxplot', size='large', plotOutput(outputId="stacker_boxplot2")),
            a( href='#', id='link_stacker_boxplot', plotOutput(outputId='stacker_boxplot') %>% withSpinner() )
          ),
          box(
            title='detailed filling ordered per arrivaltime',
            bsModal('max_stacker_products_boxplot', '', 'link_stacker_products_boxplot', size='large', plotOutput(outputId="stacker_products_boxplot2")),
            a( href='#', id='link_stacker_products_boxplot', plotOutput(outputId='stacker_products_boxplot') %>% withSpinner() )
          ),
          box(
            title='summary: detailed filling',
            tags$p(htmlOutput(outputId='stacker_products_summary'))
          )
        )
      ),
      tabItem(tabName='warehouse',
        conditionalPanel(
          condition = "output.warehouse1_present == true",
          box(
            title='Warehouse Status',
            width = 12,
            bsModal('max_warehouse_fill_timeseries', '', 'link_warehouse_fill_timeseries', size='large', plotOutput(outputId="warehouse_fill_timeseries2")),
            a( href='#', id='link_warehouse_fill_timeseries', plotOutput(outputId='warehouse_fill_timeseries') %>% withSpinner() )
          ),
          box(
            title='Warehouse Input/Output',
            width = 12,
            tags$h4('Input of the warehouse in timeframe'),
            htmlOutput(outputId = 'warehouse_input_summary'),
            tags$h4('Output of the warehouse in timeframe'),
            htmlOutput(outputId = 'warehouse_output_summary'),
            tags$h4('In- and Output per timeframe'),
            bsModal('max_warehouse_input_barplot', '', 'link_warehouse_input_barplot', size='large', plotOutput(outputId="warehouse_input_barplot2")),
            a( href='#', id='link_warehouse_input_barplot', plotOutput(outputId='warehouse_input_barplot') %>% withSpinner() ),
            radioButtons(inputId = 'warehouse_modus_choice', label = 'In-/Output calculation mode:', choices = c('directed', 'load')),
            sliderInput(inputId = 'warehouse_bucket_size',
                        label = 'choose binwidth:',
                        value = 3600,
                        min = 900,
                        max = 3600*24,
                        step = 900)
          ),
          box(
            title='Warehouse Output to Montage per timeframe',
            htmlOutput(outputId = 'warehouse_loop_output_summary'),
            bsModal('max_warehouse_output_barplot', '', 'link_warehouse_output_barplot', size='large', plotOutput(outputId="warehouse_output_barplot2")),
            a( href='#', id='link_warehouse_output_barplot', plotOutput(outputId='warehouse_output_barplot') %>% withSpinner() ),
            sliderInput(inputId = 'warehouse_bucket_size2',
                        label = 'choose binwidth:',
                        value = 14400,
                        min = 3600,
                        max = 3600*24*7,
                        step = 3600)
          ),
          box(
            title='Output distribution',
            bsModal('max_warehouse_output_pie', '', 'link_warehouse_output_pie', size='large', plotOutput(outputId="warehouse_output_pie2")),
            a( href='#', id='link_warehouse_output_pie', plotOutput(outputId='warehouse_output_pie') %>% withSpinner() )
          )
        )
      ),
      tabItem(tabName='lifter',
        conditionalPanel(
          condition = "output.lifter1_present == true",
          box(
            title='Lifter Busyness',
            bsModal('max_lifter_busy_timeseries', '', 'link_lifter_busy_timeseries', size='large', plotOutput(outputId="lifter_busy_timeseries2")),
            a( href='#', id='link_lifter_busy_timeseries', plotOutput(outputId='lifter_busy_timeseries') %>% withSpinner() )
          ),
          box(
            title='summary: Lifter Busy',
            tags$p(htmlOutput(outputId='lifter_busy_summary'))
          )
        )
      ),
      tabItem(tabName='loop',
        conditionalPanel(
          condition = "output.loop1_present == true",
          box(
            title='Total boxes on loop',
            bsModal('max_loop_total_timeseries', '', 'link_loop_total_timeseries', size='large', plotOutput(outputId="loop_total_timeseries2")),
            a( href='#', id='link_loop_total_timeseries', plotOutput(outputId='loop_total_timeseries') %>% withSpinner() )
          ),
          box(
            title='summary: Total Boxes',
            tags$p(htmlOutput(outputId='loop_total_summary')),
            bsModal('max_loop_total_boxplot', '', 'link_loop_total_boxplot', size='large', plotOutput(outputId="loop_total_boxplot2")),
            a( href='#', id='link_loop_total_boxplot', plotOutput(outputId='loop_total_boxplot') %>% withSpinner() )
          ),
          box(
            title='Loop Box distribution',
            bsModal('max_loop_boxes_timeseries', '', 'link_loop_boxes_timeseries', size='large', plotOutput(outputId="loop_boxes_timeseries2")),
            a( href='#', id='link_loop_boxes_timeseries', plotOutput(outputId='loop_boxes_timeseries') %>% withSpinner() )
          )
        )
      )
    )
  )
)
}
