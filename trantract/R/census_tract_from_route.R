#' Get all the census tracts that are served by a King Country Metro route
#'
#' Takes a string that specifies a King Country Metro route and returns a
#' character vector of all the US Census Bureau census tracts that are
#' served by that route.
#' 
#' @param route A string that specifies a King County Metro route.
#' 
#' @return A character vector that represents all the US Census Bureau census tracts that are
#' served by that route.
#' 
#' @export
#'
#' @examples
#'
#' census_tract_from_route( 45 ) 
#' census_tract_from_route( "D Line" ) 
#' census_tract_from_route( "LINK" ) 
#'
census_tract_from_route  <- function( route ) {

  #
  # Get all the stops for the specified route.
  #
  stops.for.route <- route.stops.df[ route.stops.df$route == route, ]$stop

  #
  # Get the census tracts for those stops . . . and take only unique
  # values.
  #
  unique( vapply( stops.for.route, function( stop ) census_tract_from_stop( stop ), character( 1 ) ) )

}
