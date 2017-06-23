#' Get the census tract that contains a King Country Metro stop
#'
#' Takes a string that specifies a King Country Metro route and returns a string that 
#' represents the ID for the US Census Bureau census tract that contains
#' that stop.
#' 
#' @param stop A string that specifies a King County Metro stop.
#' 
#' @return Returns a string that represents that ID for the US Census Bureau census tract
#' that contains the specified stop.
#' 
#' @export
#'
#' @examples
#'
#' #
#' # 8th Ave NW & NW 80th Street
#' #
#' census_tract_from_stop( 28100 ),
#' 
#' #
#' # Denny & Aurora
#' #
#' census_tract_from_stop( 6235 ),
#'
census_tract_from_stop  <- function( stop ) {

  #
  # Remove those stops (about 1%) that don't have census tract
  # information.
  #
  stops.with.tract <- stops[ !is.na( stops$tract ), ]

  tract.for.stop <- stops.with.tract[ stops.with.tract$stop_id == stop, ]$tract

  #
  # If a stop exists in more than one census tract, something is really
  # wrong.
  #
  stopifnot( 1 == length( tract.for.stop ) )

  as.character( tract.for.stop )

}
