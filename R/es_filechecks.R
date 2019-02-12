#' @title Check for Fileset
#' @description Checks if a fileset has been uploaded. Used when dynamically loading the upload fields.
#'
#' @param filesets the fileset list
#' @param setnumber the number of the set to check
#' @return a boolean value (TRUE if the fileset exists, else FALSE)
#'
#' @importFrom checkmate assert_tibble assert_subset assert_int
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
es_fileset_present <- function( filesets, setnumber ) {
  # parameter checking
  assert_tibble(filesets)
  assert_subset('n', colnames(filesets))
  assert_int(setnumber)

  if( !is.null(filesets) ){
    ( filesets %>% filter(n == setnumber) %>% nrow ) > 0
  } else {
    return(FALSE)
  }
}

#' @title Check for File
#' @description Checks for the existence of a file in a fileset.
#'
#' @param filesets the fileset to search in
#' @param setnumber the number of the set to search
#' @param filename the name of the desired file
#' @return a boolean value (\code{TRUE} if the file exists, else \code{FALSE})
#'
#' @importFrom checkmate assert_tibble assert_subset assert_string assert_int
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
es_file_present  <- function( filesets, setnumber, filename ) {
  # parameter checking
  assert_tibble(filesets)
  assert_subset(c('n', 'name'), colnames(filesets))
  assert_int(setnumber)
  assert_string(filename)

  if( es_fileset_present(filesets, setnumber) )
    ( filesets %>% filter(n == setnumber & grepl(pattern=filename, x=name)) %>% nrow ) >= 1
  else
    return(FALSE)
}
