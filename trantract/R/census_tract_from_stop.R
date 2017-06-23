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
