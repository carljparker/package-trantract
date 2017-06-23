plot_tract <- function( tract, map.zoom = 14 ) {

  #'
  #' Get the lon/lat of the central point of the census tract.
  #' 
  cen.tract.lon <- as.numeric( as.character( kct.shp.proj@data[ kct.shp.proj@data$NAME10 == tract, ]$INTPTLON10 ) ) 
  cen.tract.lat <- as.numeric( as.character( kct.shp.proj@data[ kct.shp.proj@data$NAME10 == tract, ]$INTPTLAT10  ))
  cen.tract.id  <- as.numeric( as.character( kct.shp.proj@data[ kct.shp.proj@data$NAME10 == tract, ]$OBJECTID  ))

  mapImageData = get_map(
                     location = c( lon = cen.tract.lon, lat = cen.tract.lat ),
                     color    = "color",
                     source   = "google",
                     maptype  = "roadmap",
                     zoom     = map.zoom
                 )

  #
  # Remove those stops (about 1%) that don't have census tract
  # information.
  #
  stops.with.tract <- stops[ !is.na( stops$tract ), ]

  stops.cen.tract <- stops.with.tract[ as.character( stops.with.tract$tract ) == tract, ]
  stops.lon.lat <- data.frame( cbind( coordinates( stops.cen.tract )[ , "stop_lon" ], coordinates( stops.cen.tract )[ , "stop_lat" ] ) )
  names( stops.lon.lat ) <- c( "stop_lon", "stop_lat" )

  #
  # Convert shapefile to format ggmap can work with
  #
  # I think we are basically turning kct.shp.proj into a dataframe . . .
  #
  polys <- fortify( kct.shp.proj )
  head( polys ) 

  #
  # The ID of the polygon for a particular census tract is always one
  # less than the OBJECTID is the @data slot of the shape object.
  #
  # The reason this always works is that the polygon ID is zero-based
  # and the OBJECTID is one-based. So there you have it.
  #
  polys <- polys[ polys$id == cen.tract.id - 1, ]

  map.title <- paste( "Transit stops for census tract", tract, " [zoom level = ", map.zoom, "]" )  

  #
  # Create the final map
  #
  # Plot only those stops that are in the specified census tract.
  #
  # When you run this you might get a diagnostic similar to:
  #
  #   Removed 7593 rows containing missing values (geom_point).
  #
  # I'm pretty sure that this refers to all the points in my DF that don't
  # correspond to area bounded by my map.
  #
  ggmap( mapImageData ) +
      geom_polygon(
        aes( x = long, y = lat, group = group ),
        data = polys,
        color = colors[9],
        fill = colors[6],
        alpha = 0.5
      ) +
      labs( x = "Longitude", y = "Latitude" ) +
      geom_point( 
                   aes( 
                        x = stop_lon,
                        y = stop_lat 
                      ), 
                   data = stops.lon.lat,
                   col = "blue"
                ) +
      labs( title = map.title ) 

}
