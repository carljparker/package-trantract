
library( trantract )

#
# Pull in a library to calc distances from lat-lon.
#
# <https://github.com/cran/geosphere>
#
# <https://cran.r-project.org/web/packages/geosphere/index.html>
#
library(geosphere)


data( stops )
data( kct.shp.proj )
data( route.stops.df )
data( tract.demographics.kc )
dim( tract.demographics.kc )
names( tract.demographics.kc )

 
census.tracts <- unique( stops$tract )

service.distro.df <- data.frame( tract = census.tracts )

rm( route.stops.vec )
route.stops.vec <- numeric( 0 )

for( tract in service.distro.df$tract ) {

  route.stops.vec[ tract ] <- nrow( route_stops_from_census_tract( tract ) )

}

length( route.stops.vec )
service.distro.df$route.stops <- route.stops.vec

hist( service.distro.df$route.stops ) 
hist( log( service.distro.df$route.stops ) ) 

service.distro.df[ service.distro.df$route.stops == min( service.distro.df$route.stops ), ]
service.distro.df[ service.distro.df$route.stops == max( service.distro.df$route.stops ), ]

plot_tract( 320.07, map.zoom = 13 )
plot_tract( 81, map.zoom = 15 )

dim( tract.demographics.kc )

service.distro.df$Tract <- as.character( service.distro.df$tract )
tract.demographics.kc.routes <- merge( tract.demographics.kc, service.distro.df )  
head( tract.demographics.kc.routes ) 

mod.tract.multivar <- lm( route.stops ~ Population + Housing.Units + Land.Area, data = tract.demographics.kc.routes )
summary( mod.tract.multivar )

#
# The Population.Density column appears to be derived from Population
# and land area. However, if I divide the Population/LandArea I get
# numbers that, although close to Density, are actually somewhat
# different.
#
# Assuming that the Census knows what they are doing, we'll use their
# number rather than calculate our own.
#
mod.tract.density <- lm( route.stops ~ Population.Density, data = tract.demographics.kc.routes )
summary( mod.tract.density )

plot( 
     tract.demographics.kc.routes$Population, 
     log( tract.demographics.kc.routes$route.stops ), 
     main = "Level of service vs density",
     xlab = "Persons / Square Miles",
     ylab = "log( route-stops )"
    )


options( "width" = 210 )

#
# GetDistancesFromLatLon
#
GetDistancesFromLatLon <- function( latlon1, latlon2 ) {

  stopifnot( 2 == length( latlon1 ) )
  stopifnot( 2 == length( latlon2 ) )

  #
  # distGeo() wants the lon first and then the lat!
  #
  lat1 <- latlon1[ 1 ]
  lat2 <- latlon2[ 1 ]

  lon1 <- latlon1[ 2 ]
  lon2 <- latlon2[ 2 ]

  #
  # Direct distance; h for hypotenuse
  #
  h <- distGeo( c( lon1, lat1 ), c( lon2, lat2 ) )

  #
  # x component (E-W or lognitudinal distance)
  #
  x <- distGeo( c( lon1, lat1 ), c( lon2, lat1 ) )
   
  #
  # y component (N-S or latitudinal distance)
  #
  y <- distGeo( c( lon1, lat1 ), c( lon1, lat2 ) )

  return( list( dist=h, lat.dist=y, lon.dist=x ) )

}


head( tract.demographics.kc.routes )

is( tract.demographics.kc.routes$Land.Area )

#
# Add a column that represents that _density_ of the route.stops in each
# tract. Note, again, that stops in and of themselves are _not_ a good
# measure of service because of the wide variation in the number of
# buses that serve any given stop. (And even this is a simplification
# because we aren't considering number of trips that any given bus makes
# to that stop each day--what most people would think of as frequency.)
#

tract.demographics.kc.routes$route.stop.dens <- tract.demographics.kc.routes$route.stops / tract.demographics.kc.routes$Land.Area

#
# Now, add a column that represents the distance from the city center.
# According to Wikipedia, the coordinates for Seattle are:
#' 
#' LAT:  47°36′35″N 
#' 
#' LON: 122°19′59″W
#' 
# These correspond to what looks, visually, like the center, on 6th
# Avenue between Union and University.
# 
# Get the decimal equivalents.
#
lat.seattle <- 47 + ( 36 / 60 ) + ( 35 / ( 60 * 60 ) )

lon.seattle <- ( 122 + ( 19 / 60 ) + ( 59 / ( 60 * 60 ) ) ) * ( -1 )

tract.demographics.kc.routes$dist.to.cent <- 
  vapply( 1:nrow( tract.demographics.kc.routes ), function( dfrow ) {
      distGeo( 
        c( tract.demographics.kc.routes[ dfrow, ]$INTPTLON10, tract.demographics.kc.routes[ dfrow, ]$INTPTLAT10 ),
        c( lon.seattle, lat.seattle ) 
      )
    },
    numeric( 1 )
  )

#
# The `distGeo()` returns the distance in meters, which is perfectly
# reasonable. But all the other data is in English units, so even though
# I feel as though I am committing a sin. I will convert this into feet.
# God forgive me.
#
# Google says it is 3.28 feet.
#
m.to.f <- function( m ) {
  m * 3.28
}

tract.demographics.kc.routes$dist.to.cent <- m.to.f( tract.demographics.kc.routes$dist.to.cent )
