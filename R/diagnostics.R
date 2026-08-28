#' Processing calibration files
#'
#' @description
#' Function to read input file/s (semicolon-separated files) with the
#' calibration information in `MCMCtree`.
#'
#' @param main_dir Character, (absolute or relative) path to the directory where
#'        the file/s are saved. Please type a "/" at the end of the path.
#' @param f_names Character, vector with the file name/s. If more than one file,
#'        please separate the names with comas as in a character vector.
#' @param dat Character, global vector created at the beginning of he script
#'        with the name that users have given to each analysis. The length
#'        of this vector is equivalent to the number of `MCMCtree` jobs that
#'        have been run for each hypothesis (e.g., different tree
#'        hypotheses,  different calibration hypotheses, etc.). The length
#'        of this vector must be equal to the length of the character vector
#'        passed to argument `f_names`.
#' @param head_avail Boolean, TRUE if the header is available in the input files.
#'        FALSE otherwise.
#'
#' @returns List, there are as many data frames as calibration files have been
#'        input.
#' @export
read_calib_f <- function( main_dir, f_names, dat, head_avail = TRUE )
{

  # Check length(dat) == length(f_names), otherwise abort
  if( length( dat ) != length( f_names ) ){
    stop( paste( "\nThe length of character vector passed to argument \"dat\" must be",
                 " the same length as of the character vector passed to argument \"f_names\"\n",
                 sep = "" ) )
  }
  # Generated list vector to save files
  calib_nodes          <- vector( mode = "list", length = length( dat ) )
  names( calib_nodes ) <- dat
  # Now, load the file/s on the list object `calib_nodes`
  for( i in 1:length(dat) ){

    if( head_avail == TRUE ){
      calib_nodes[[ i ]] <- utils::read.table( file = paste( main_dir, f_names[i],
                                                      sep = "" ),
                                        header = TRUE, sep = ";",
                                        stringsAsFactors = FALSE )
    }else{
      calib_nodes[[ i ]] <- utils::read.table( file = paste( main_dir, f_names[i],
                                                      sep = "" ),
                                        header = FALSE, sep = ";",
                                        stringsAsFactors = FALSE )
      colnames( calib_nodes ) <- c( "Calib", "node", "Prior" )
    }

  }

  # Return final object
  return( calib_nodes )

}
