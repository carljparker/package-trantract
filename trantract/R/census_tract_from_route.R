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
