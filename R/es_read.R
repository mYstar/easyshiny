#' @title Read fileset
#' @description Reads a fileset from folder (shiny \code{\link{fileInput}} format)
#'
#' @param folders an array of paths to folders containing the data to use
#'
#' @return a dataframe in shiny \code{\link{fileInput}} format or \code{NULL} if the folder can not be found
#'
#' @importFrom checkmate assert check_array check_null check_directory_exists
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate group_indices
#' @importFrom magrittr %>%
es_read_filesets <- function( folders ) {
  # argument checking
  assert(
    check_array(folders, mode = 'character'),
    check_null(folders),
    check_directory_exists(folders, access = 'r')
  )

  if( !is.null(folders) )
    data.frame( datapath = dir(folders, full.names = TRUE) ) %>%
      as_tibble() %>%
      mutate(datapath = datapath %>% as.character) %>%
      mutate( folder = dirname(datapath) ) %>%
      mutate( name = basename(datapath) ) %>%
      mutate( n = group_indices(., folder) )
  else
    NULL
}

#' @title Read files
#' @description Read a specific file from multiple sources in the fileset and join to one \code{\link[tibble]{tibble}}.
#'
#' @param filesets a filesets containing one or more files with the name
#' @param filename the name of the file to use
#' @param prepare (optional) a function to call on to alter the read \code{\link[tibble]{tibble}}
#' @param ... arguments to pass on to \code{\link{read.csv}}
#'
#' @importFrom checkmate assert_data_frame assert_subset expect_string assert_function test_choice check_choice
#' @importFrom magrittr %>%
#' @importFrom utils read.csv
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by do filter ungroup
#' @importFrom tools file_ext
#'
#' @return a \code{\link[tibble]{tibble}} containing the read data, \code{NULL} if the file is not found
es_read_files <- function( filesets, filename, prepare = function(data) { data },... ) {
  # parameter checking
  assert_data_frame(filesets, col.names = 'named')
  assert_subset(c('datapath', 'name', 'n'), colnames(filesets))
  expect_string(filename,
                min.chars = 1,
                pattern = '.*\\.csv|.*\\.rds',
                info = 'easyshiny in only able to read the following file formats: .csv, .rds')
  assert_function(prepare, nargs = 1)

  if( !test_choice(filename, filesets$name) ) {
    warning('The given fileset does not contain the file. ', check_choice(filename, filesets$name) ) # return a warning
    return(NULL)
  }

  filesets %>%
    filter(name == filename) %>%
    group_by(n) %>%
    do( {
        switch(file_ext(filename),
               # RDS reader
               rds = readRDS(.$datapath) %>%
                 as_tibble(),
               # CSV reader (+ prepare function)
               csv = read.csv(.$datapath, ..., as.is = TRUE) %>%
                 as_tibble %>%
                 prepare()
        )
    } ) %>%
    ungroup()
}

#' @title Read without header
#' @description Wrapper for \code{\link{es_read_files}}. To use for reading files without header.
#'
#' @param ... use the params from \code{\link{es_read_files}}
#'
#' @return see \code{\link{es_read_files}}
es_read_exfiles <- function( ... ) {
  es_read_files( ..., header = FALSE )
}

#' @title Set the setnames
#' @description Add setnames to a \code{\link[tibble]{tibble}} using the group identifier n.
#'
#' @param data a \code{\link[tibble]{tibble}} without setnames, but containing a column named 'n' for grouping
#' @param setnames a vector of setnames at least as long as \code{unique(n)} in the data \code{\link[tibble]{tibble}}
#'
#' @return the altered \code{\link[tibble]{tibble}}
#'
#' @importFrom checkmate assert_tibble assert_subset assert_vector
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom dplyr row_number left_join
es_add_setname <- function( data, setnames ) {
  # parameter checking
  assert_tibble(data, min.cols = 1, col.names = T)
  assert_subset(c('n'), colnames(data))
  assert_vector(setnames, min.len = 1)

  # create numbered tibble from setnames
  setnames <- tibble(setname = setnames) %>%
    mutate( n = row_number() )

  data %>%
    left_join( setnames )
}

#' @title Create Filereader
#' @description Makes a file usable for user input. The file can then be supplied by the user at runtime and
#' is provided as function under the given name for visuals creation. The function is also created
#' in the global environment for usage in console mode.
#'
#' Understands:
#'
#' * .csv
#'
#' * .mds
#'
#' @param filename the name of the file to read
#' @param readerId a name for the reader function, that can be used in the plots and console mode (default: derives the name from the filename, just cuts the ending)
#'
#' @importFrom checkmate assert_string
#' @importFrom tools file_path_sans_ext
#' @export
es_read <- function(filename, readerId = NULL) {
  # parameter checking
  assert_string(filename, pattern = '.*\\.csv|.*\\.rds')
  assert_string(readerId, null.ok = T)

  files_table <- get('files', envir = appData)
  if( is.null(readerId) )
    readerId <- file_path_sans_ext(filename)

  files_table <- rbind(
    files_table,
    list(
      id=readerId,
      name=filename,
      reader=quote( reactive({es_read_files( input$files1 %>% mutate(n = 1), files[idx,]$name )}))
      )
    )
  assign('files', files_table, envir = appData)

  console_fileset <- get('console_fileset', envir = appData)
  reader <- function() { es_read_files( console_fileset, filename ) }
  assign(readerId, reader, envir = globalenv())
}
