### --------------------------------------- ###
### checking for input files
### --------------------------------------- ###
base.data.fileset.present <- function( filesets, setnumber ) {
  if( !is.null(filesets) ){
    ( filesets %>% filter(n == setnumber) %>% nrow ) > 0
  } else {
    return(FALSE)
  }
}

base.data.file.present  <- function( filesets, setnumber, filename ) {
  if( base.data.fileset.present(filesets, setnumber) )
    ( filesets %>% filter(n == setnumber & grepl(pattern=filename, x=name)) %>% nrow ) >= 1
  else
    return(FALSE)
}
