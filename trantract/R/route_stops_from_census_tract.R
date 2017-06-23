route_stops_from_census_tract <- function( tract ) {

  #
  # Remove those stops (about 1%) that don't have census tract
  # information.
  #
  stops.with.tract <- stops[ !is.na( stops$tract ), ]
  stops.cen.tract <- stops.with.tract[ as.character( stops.with.tract$tract ) == tract, ]

  #
  # Get the stops from the above dataframe.
  #
  stops.vec <- stops.cen.tract$stop_id 

  #
  # Create a list where the name of each item is the stop and the values
  # are the routes that serve that stop.
  #
  routes.li <- vapply( stops.vec, function( stop ) list( as.character( route.stops.df[ route.stops.df$stops == stop, ]$route ) ), list( 1 ) ) 
  names( routes.li ) <- stops.vec

  #
  # Convert that list into a DF. Thanks to Aurelien for this wonderful
  # code.
  #
  #   <https://aurelienmadouasse.wordpress.com/2012/05/22/r-code-how-to-convert-a-list-to-a-data-frame/>
  #
  stop.routes.df <- data.frame( 
                                stop  = rep( names( routes.li ), lapply( routes.li, length ) ),
                                route = unlist( routes.li )
                              )
  
  rownames( stop.routes.df )  <- NULL

  stop.routes.df

}
