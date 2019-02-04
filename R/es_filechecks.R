#' @title Check for Fileset
#' @description Checks if a fileset has been uploaded. Used when dynamically loading the upload fields.
#'
#' @param filesets the fileset list
#' @param setnumber the number of the set to check
#'
#' @return a boolean value (TRUE if the fileset exists, else FALSE)
es_fileset_present <- function( filesets, setnumber ) {
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
#'
#' @return a boolean value (\code{TRUE} if the file exists, else \code{FALSE})
es_file_present  <- function( filesets, setnumber, filename ) {
  if( base.data.fileset.present(filesets, setnumber) )
    ( filesets %>% filter(n == setnumber & grepl(pattern=filename, x=name)) %>% nrow ) >= 1
  else
    return(FALSE)
}
