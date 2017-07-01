
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
     tract.demographics.kc.routes$Population.Density, 
     log( tract.demographics.kc.routes$route.stops ), 
     main = "Level of service vs density",
     xlab = "Persons / Square Mile",
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

tract.demographics.kc.routes$route.stops.dens <- tract.demographics.kc.routes$route.stops / tract.demographics.kc.routes$Land.Area

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
# The `distGeo()` function returns the distance in meters, which is
# perfectly reasonable. But all the other data is in English units, so
# even though I feel as though I am committing a sin. I will convert
# this into feet.  God forgive me.
#
# Google says it is 3.28 feet to a meter.
#
m_to_f <- function( m ) {
  m * 3.28
}

tract.demographics.kc.routes$dist.to.cent <- m_to_f( tract.demographics.kc.routes$dist.to.cent )

#
# Let's also use the lat-lon itself, but we will normalize each of them.
#
mean.INTPTLAT10 <- mean( tract.demographics.kc.routes$INTPTLAT10 )
sd.INTPTLAT10   <- sd(   tract.demographics.kc.routes$INTPTLAT10 )

mean.INTPTLON10 <- mean( tract.demographics.kc.routes$INTPTLON10 )
sd.INTPTLON10   <- sd(   tract.demographics.kc.routes$INTPTLON10 )

norm_lat <- function( lat.arg ) {

  ( lat.arg - mean.INTPTLAT10 ) / sd.INTPTLAT10 

}

norm_lon <- function( lon.arg ) {

  ( lon.arg - mean.INTPTLON10 ) / sd.INTPTLON10 

}

#
# Here is how the centroid of the internal points of all the tracts
# compares to Wikipedia canonical GPS location for Seattle.
#
mean.INTPTLAT10 
lat.seattle

mean.INTPTLON10 
lon.seattle

tract.demographics.kc.routes$norm.intptlat10 <- norm_lat( tract.demographics.kc.routes$INTPTLAT10 )

tract.demographics.kc.routes$norm.intptlon10 <- norm_lon( tract.demographics.kc.routes$INTPTLON10 )

head( tract.demographics.kc.routes$norm.intptlat10 ) 
sum( tract.demographics.kc.routes$norm.intptlat10 < 0 ) 
sum( tract.demographics.kc.routes$norm.intptlat10 > 0 ) 

#
# We've set the stage, let's rock a few more regressions.
#

#
# First, let's try route.stops.dens (density of route.stops) as the
# response.
#
mod.tract.rs.dens.density <- lm( route.stops.dens ~ Population.Density, data = tract.demographics.kc.routes )
summary( mod.tract.rs.dens.density )

#
# Wow. Okay. That made a difference!
#

plot( 
     tract.demographics.kc.routes$Population.Density, 
     tract.demographics.kc.routes$route.stops.dens, 
     main = "Level of service (density) vs population density",
     xlab = "Persons / Square Mile",
     ylab = "log( route-stops-density )"
    )

intercept <- coef( mod.tract.rs.dens.density )[ "(Intercept)" ]
slope     <- coef( mod.tract.rs.dens.density )[ "Population.Density" ]

abline( intercept, slope )

#
# Use log() scale for the y-axis.
#
plot( 
     tract.demographics.kc.routes$Population.Density, 
     log( tract.demographics.kc.routes$route.stops.dens ), 
     main = "Level of service (density) vs population density",
     xlab = "Persons / Square Mile",
     ylab = "log( route-stops-density )"
    )

#
# Show the population density that gets the greatest density of route
# stops.
#
max.rs.dens <- max( tract.demographics.kc.routes$route.stops.dens ) 
( pop.dens.for.max.service <- tract.demographics.kc.routes[ tract.demographics.kc.routes$route.stops.dens == max.rs.dens, ]$Population.Density )

abline( v = pop.dens.for.max.service, col = "red" )

#
# Show the route stop density that is given to the tract with the
# greatest population density.
#
max.pop.dens <- max( tract.demographics.kc.routes$Population.Density )
( rs.dens.for.max.pop.dens <- tract.demographics.kc.routes[ tract.demographics.kc.routes$Population.Density == max.pop.dens, ]$route.stops.dens )
( log.rs.dens.for.max.pop.dens <- log( rs.dens.for.max.pop.dens ) )

abline( h = log.rs.dens.for.max.pop.dens, col = "blue" )



# --- END --- #
