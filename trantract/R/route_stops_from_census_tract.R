#' Get all the stops that serve a census tract and all the routes that
#' serve those stops.
#'
#' @param tract A string that specifies a US Census Bureau census tract.
#' 
#' @return A two-column data frame of stops and routes. If a stop is
#' served by more than one route, that stop will appear in multiple
#' rows of the dataframe, once for each route.
#' 
#' @export
#'
#' @examples
#'
#' route_stops_from_census_tract( 30 )
#' #     stop  route
#' # 1  13600     15
#' # 2  13600 D Line
#' # 3  13620     15
#' # 4  13620 D Line
#' # 5  13640     15
#' # 6  13640 D Line
#' # 7  14300     15
#' # 8  14300 D Line
#' # 9  14320     15
#' # 10 14320 D Line
#' # 11 14322 D Line
#' # 12 14340     15
#' # 13 14340 D Line
#' # 14 28080     28
#' # 15 28100     28
#' # 16 28100    994
#' # 17 28120     28
#' # 18 28130     28
#' # 19 35580     45
#' # 20 35580     40
#' # 21 35600     45
#' # 22 35630     45
#'
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
