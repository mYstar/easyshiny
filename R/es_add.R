library(dplyr)
library(shiny)

# --- util ---
summaries_to_html <- function( summaries ) {
  # build the hmtl output
  summaries <- summaries %>%
    ungroup() %>%
    mutate( setname = setname %>% as.character ) %>%
    group_by( setname) %>%
    mutate( header = case_when( (setname != lag(setname , default='')) ~ setname,
                               TRUE ~ '') ) %>%
    ungroup

  if( 'prodnum' %in% colnames(summaries) )
    summaries <- summaries %>%
      mutate( prodnum =  paste0('order: ', prodnum %>% as.character()) )
  else
    summaries <- summaries %>%
      mutate(prodnum = '')
  if( 'lifternumber' %in% colnames(summaries) )
    summaries <- summaries %>%
      mutate( lifternumber =  paste0('lifter: ', lifternumber %>% as.character()) )
  else
    summaries <- summaries %>%
      mutate(lifternumber = '')
  if( 'machine' %in% colnames(summaries) )
    summaries <- summaries %>%
      mutate( machine =  paste0('machine: ', machine %>% as.character()) )
  else
    summaries <- summaries %>%
      mutate(machine = '')
  if( 'Simulation.Time' %in% colnames(summaries) & 'product' %in% colnames(summaries) )
    summaries <- summaries %>%
      mutate( max_prod =  paste0('p: ', product %>% as.character()) ) %>%
      mutate( max_time =  paste0('t: ', Simulation.Time %>% round( digits = 0 ) %>% as.character()) )
  else
    summaries <- summaries %>%
      mutate(max_prod = '') %>%
      mutate(max_time = '')

  summaries <- summaries %>%
    rowwise() %>%
    mutate( text = tags$p(
      tags$h5(header),
      tags$b(prodnum),
      tags$b(lifternumber),
      tags$b(machine),
      "Min:", summary['Min.'] %>% round(digits=2),
      ", 1st Qu.:", summary['1st Qu.'] %>% round(digits=2),
      ", Median:", summary['Median'] %>% round(digits=2),
      ", Mean:", summary['Mean'] %>% round(digits=2),
      ", 3rd Qu.:", summary['3rd Qu.'] %>% round(digits=2),
      ", Max:", summary['Max.'] %>% round(digits=2),
      '(',
      max_prod,
      max_time,
      ')'
      ) %>% as.character()
    )

  summaries$text %>%
    paste0( collapse = ' ' )
}
