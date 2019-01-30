### --------------------------------------- ###
### generic functions
### --------------------------------------- ###

# Reads a fileset from folder (shiny fileinput format)
#'
#' @param folders an array of paths to folders containing the data to use
#'
#' @return a dataframe in shiny fileinput format or NULL if the folder can not be found
#' @export
es_read_filesets <- function( folders ) {

  if( !is.null(folders) && all( dir.exists(folders) ) )
  data.frame( datapath = dir(folders, full.names = TRUE) ) %>%
    as_tibble() %>%
    mutate(datapath = datapath %>% as.character) %>%
    mutate( folder = dirname(datapath) ) %>%
    mutate( name = basename(datapath) ) %>%
    mutate( n = group_indices(., folder) )
  else
    NULL
}

#' Read a specific file from multiple sources and join to one data frame.
#'
#' @param filesets one or more filesets containing the file
#' @param filename the name of the file to use
#' @param callback (optional) a function to call on the read dataframe
#' @param ... arguments to pass on to \code{read.csv}
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by do filter tbl_df ungroup
#'
#' @return a tibble containing the read data, NULL if the file is not found
es_read_files <- function( filesets, filename, callback,... ) {
  if( !any( grepl(filename, filesets$name) ) )
    return(NULL)

  filesets %>%
    group_by(n) %>%
    do( {
        name_csv <- paste0(filename, '.csv')
        name_rds <- paste0(filename, '.rds')
        if(name_rds %in% .$name) {
          filedata <- filter(., name ==  name_rds)
          result <- readRDS(filedata$datapath)
        } else if(name_csv %in% .$name) {
          filedata <- filter(., name == name_csv )
          result <- filedata$datapath %>%
            read.csv(..., as.is = TRUE) %>%
            as_tibble %>%
            callback()
        } else {
          # TODO: remove this, when filetype sticking is implemented
          result <- tibble()
        }
        result
    } ) %>%
    ungroup
}

#' Wrapper for \code{\link{es_read_files}}. To use for reading files without header.
#'
#' @param filesets see \code{\link{es_read_files}}
#' @param filename see \code{\link{es_read_files}}
#' @param callback see \code{\link{es_read_files}}
#'
#' @return see \code{\link{es_read_files}}
es_read_exfiles <- function( filesets, filename, callback ) {
  base.data.read.files( filesets, filename, callback, header = FALSE )
}

#' Add setnames to a data frame using the group identifier n.
#'
#' @param simdata a dataframe without setnames
#' @param setnames a vector of setnames at least as long as n in the data frame
#'
#' @return the altered data frame
es_add_setname <- function( simdata, setnames ) {

  # create numbered DF from setnames
  setnames <- data.frame(setname = setnames) %>%
    mutate( n = row_number() ) %>%
    tbl_df()

  simdata %>%
    left_join( setnames )
}

#' Makes a file usable for user input. The file can then be supplied by the user at runtime and
#' is provided under its name for visuals creation. Understands:
#'
#' * .csv
#' * .mds
#'
#' @param filename the name of the file to read
#'
#' @return a reader function for the console mode
#' @export
es_read <- function(filename) {

  files_table <- get('files', envir = appData)
  file_basename <- strsplit(filename, split = '.', fixed = T)[[1]][1]
  files_table <- rbind(
    files_table,
    list(
      name=file_basename,
      reader=quote( reactive({es_read_files( input$files1 %>% mutate(n = 1), files[idx,]$name, function(data) {data} )}))
      )
    )
  assign('files', files_table, envir = appData)

  console_fileset <- get('console_fileset', envir = appData)
  reader <- function() { es_read_files( console_fileset, file_basename, function(data) {data} ) }
  reader
}
