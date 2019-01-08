library(dplyr)

### --------------------------------------- ###
### generic functions
### --------------------------------------- ###

# Reads a fileset from folder (shiny fileinput format)
#'
#' @param folders an array of paths to folders containing the data to use
#'
#' @return
es_read_filesets <- function( folders ) {

  data.frame( datapath = dir(folders, full.names = TRUE) ) %>%
    tbl_df() %>%
    mutate(datapath = datapath %>% as.character) %>%
    mutate( folder = dirname(datapath) ) %>%
    mutate( name = basename(datapath) ) %>%
    mutate( n = group_indices(., folder) )
}

#' Read a specific file from multiple sources and join to one data frame.
#'
#' @param filesets one or more filesets containing the file
#' @param filename the name of the file to use
#' @param callback (optional) a function to call on the read dataframe
#' @param ... arguments to pass on to \code{\link{read.csv}}
#'
#' @return a dataframe containing the read data
es_read_files <- function( filesets, filename, callback,... ) {
  filesets %>%
    group_by(n) %>%
    do( {
        filedata <- filter(., name == paste0(filename, '.rds') )
        if( filedata %>% nrow() != 0)
          result <- readRDS(filedata$datapath)
        else {
          filedata <- filter(., name == paste0(filename, '.csv') )
          result <- filedata$datapath %>%
            read.csv(..., as.is = TRUE) %>%
            tbl_df %>%
            callback()
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
es.read_exfiles <- function( filesets, filename, callback ) {
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

# Select the data inside the timerange.
#'
#' @param simdata a dataframe with a time column
#' @param time_min the minimal value for the time window
#' @param time_max the maximal value for the time window
#'
#' @return a data frame containing only the rows inside of the time window
es_timerange_select <- function( simdata , time_min, time_max ) {
  simdata %>%
    filter(Simulation.Time >= time_min & Simulation.Time <= time_max)
}

#' Makes a file usable for user input. The file can then be supplied by the user at runtime and
#' is provided under its name for visuals creation. Understands:
#'
#' * .csv
#' * .mds
#'
#' @param filename the name of the file to read
#'
#' @return NULL
#' @export
es_read <- function(filename) {
  files_table <- get('files', envir = appData)
  files_table <- rbind(
    files_table,
    list( name=strsplit(filename, split = '.', fixed = T)[[1]][1],  file=filename )
    )
  assign('files', files_table, envir = appData)
}
