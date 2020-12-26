## ----warning = FALSE, message = FALSE, echo = TRUE-----------------------
library( trantract )

## ------------------------------------------------------------------------
data( stops )
data( kct.shp.proj )
data( route.stops.df )
data( tract.demographics.kc )

## ------------------------------------------------------------------------
census.tracts <- unique( stops$tract )

## ------------------------------------------------------------------------
service.distro.df <- data.frame( tract = census.tracts )

route.stops.vec <- numeric( 0 )

## ------------------------------------------------------------------------
for( tract in service.distro.df$tract ) {

  route.stops.vec[ tract ] <- nrow( route_stops_from_census_tract( tract ) )

}

## ------------------------------------------------------------------------
service.distro.df$route.stops <- route.stops.vec

## ------------------------------------------------------------------------
hist( service.distro.df$route.stops ) 

## ------------------------------------------------------------------------
hist( log( service.distro.df$route.stops ) ) 

## ------------------------------------------------------------------------
service.distro.df[ service.distro.df$route.stops == min( service.distro.df$route.stops ), ]
service.distro.df[ service.distro.df$route.stops == max( service.distro.df$route.stops ), ]

plot_tract( 320.07, map.zoom = 13 )
plot_tract( 81, map.zoom = 15 )

## ------------------------------------------------------------------------
service.distro.df$Tract <- as.character( service.distro.df$tract )
tract.demographics.kc.routes <- merge( tract.demographics.kc, service.distro.df )  

## ------------------------------------------------------------------------
mod.tract.multivar <- lm( route.stops ~ Population + Housing.Units + Land.Area, data = tract.demographics.kc.routes )
summary( mod.tract.multivar )

## ----set-options, warning = FALSE, message = FALSE, echo = FALSE------------------------------------------------------
options( "width" = 120 )
mod.tract.density <- lm( route.stops ~ Population.Density, data = tract.demographics.kc.routes )
summary( mod.tract.density )

## ---------------------------------------------------------------------------------------------------------------------
plot( 
     tract.demographics.kc.routes$Population.Density, 
     log( tract.demographics.kc.routes$route.stops ), 
     main = "Level of service vs density",
     xlab = "Persons / Square Miles",
     ylab = "log( route-stops )"
     )

## ---------------------------------------------------------------------------------------------------------------------
tract.demographics.kc.routes$route.stops.dens <- tract.demographics.kc.routes$route.stops / tract.demographics.kc.routes$Land.Area

## ---------------------------------------------------------------------------------------------------------------------
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

tract.demographics.kc.routes$norm.intptlat10 <- norm_lat( tract.demographics.kc.routes$INTPTLAT10 )
tract.demographics.kc.routes$norm.intptlon10 <- norm_lon( tract.demographics.kc.routes$INTPTLON10 )

## ---------------------------------------------------------------------------------------------------------------------
sum( tract.demographics.kc.routes$norm.intptlat10 < 0 ) 
sum( tract.demographics.kc.routes$norm.intptlat10 > 0 ) 

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.rs.dens.density <- lm( route.stops.dens ~ Population.Density, data = tract.demographics.kc.routes )
summary( mod.tract.rs.dens.density )

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.rs.dens.density.quad <- lm( route.stops.dens ~ poly( Population.Density, 2, raw = TRUE ), data = tract.demographics.kc.routes )
summary( mod.tract.rs.dens.density.quad )

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.rs.dens.density.cube <- lm( route.stops.dens ~ poly( Population.Density, 3, raw = TRUE ), data = tract.demographics.kc.routes )
summary( mod.tract.rs.dens.density.cube )

## ---------------------------------------------------------------------------------------------------------------------
AIC( mod.tract.rs.dens.density, mod.tract.rs.dens.density.quad, mod.tract.rs.dens.density.cube )

## ---------------------------------------------------------------------------------------------------------------------
slope     <- coef( mod.tract.rs.dens.density )[ "Population.Density" ]
intercept <- coef( mod.tract.rs.dens.density )[ "(Intercept)" ]

## ---------------------------------------------------------------------------------------------------------------------
( max.pop.dens <- max( tract.demographics.kc.routes$Population.Density ) )
( rs.dens.for.max.pop.dens <- tract.demographics.kc.routes[ tract.demographics.kc.routes$Population.Density == max.pop.dens, ]$route.stops.dens )

## ---------------------------------------------------------------------------------------------------------------------
( max.rs.dens <- max( tract.demographics.kc.routes$route.stops.dens ) ) 
( pop.dens.for.max.service <- tract.demographics.kc.routes[ tract.demographics.kc.routes$route.stops.dens == max.rs.dens, ]$Population.Density )

## ---------------------------------------------------------------------------------------------------------------------
cube.coef <- coef( mod.tract.rs.dens.density.cube ) 
x.cube <- 0:50000
y.cube <- vapply( 0:50000, function( x ) y = cube.coef[ 4 ] * ( x ** 3 ) + cube.coef[ 3 ] * ( x ** 2 ) + cube.coef[ 2 ] * ( x ) + cube.coef [ 1 ], numeric( 1 ) )

plot( 
     tract.demographics.kc.routes$Population.Density, 
     tract.demographics.kc.routes$route.stops.dens, 
     main = "Level of service (density) vs population density",
     xlab = "Persons / Square Mile",
     ylab = "route-stops-density"
     )

abline( intercept, slope )

abline( v = pop.dens.for.max.service, col = "red" )
abline( h = rs.dens.for.max.pop.dens, col = "green" )

points( x.cube, y.cube, cex = 0.10, col = "blue" ) 

## ---------------------------------------------------------------------------------------------------------------------
( log.rs.dens.for.max.pop.dens <- log( rs.dens.for.max.pop.dens ) )
log.y.cube <- log( y.cube )

## ---------------------------------------------------------------------------------------------------------------------
plot( 
     tract.demographics.kc.routes$Population.Density, 
     log( tract.demographics.kc.routes$route.stops.dens ), 
     main = "Level of service (density) vs population density",
     xlab = "Persons / Square Mile",
     ylab = "log( route-stops-density )"
     )

abline( v = pop.dens.for.max.service, col = "red" )
abline( h = log.rs.dens.for.max.pop.dens, col = "green" )

points( x.cube, log.y.cube, cex = 0.10, col = "blue" ) 

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.latlon <- lm( route.stops.dens ~ norm.intptlat10 + norm.intptlon10, data = tract.demographics.kc.routes )
summary( mod.tract.latlon )

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.latlon.quad <- lm( route.stops.dens ~ poly( norm.intptlat10, 2, raw = TRUE ) + poly( norm.intptlon10, 2, raw = TRUE ), data = tract.demographics.kc.routes )
summary( mod.tract.latlon.quad )

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.latlon.cube <- lm( route.stops.dens ~ poly( norm.intptlat10, 3, raw = TRUE ) + poly( norm.intptlon10, 3, raw = TRUE ), data = tract.demographics.kc.routes )
summary( mod.tract.latlon.cube )

## ---------------------------------------------------------------------------------------------------------------------
AIC( mod.tract.latlon, mod.tract.latlon.quad, mod.tract.latlon.cube )

## ---------------------------------------------------------------------------------------------------------------------
log.rs.dens.5n  <- fivenum( log( tract.demographics.kc.routes$route.stops.dens ) )
tukey.low.hinge <- log.rs.dens.5n[ 2 ]
tukey.up.hinge  <- log.rs.dens.5n[ 4 ]

color.tukey <- function( val ) {
  ifelse( log( val ) < tukey.low.hinge, "red", ifelse( log( val ) > tukey.up.hinge, "green", "blue" ) )
}

tract.demographics.kc.routes$color <- color.tukey( tract.demographics.kc.routes$route.stops.dens )

plot( x = tract.demographics.kc.routes$norm.intptlon10, 
      y = tract.demographics.kc.routes$norm.intptlat10,
      col = tract.demographics.kc.routes$color,
      main = "Normalized location of tracts",
      xlab = "Normalized longitude of tract internal point",
      ylab = "Normalized latitude of tract internal point"
    )

abline( h = 0, v = 0, col = "gray", lwd = 2, lty = 1 )

legend( x = "right", pch = 1, legend = c( "High", "Med", "Low" ), col = c( "green", "blue", "red" ), title = "Service level" ) 

## ---------------------------------------------------------------------------------------------------------------------
mod.tract.density.latlon.cube <- lm( route.stops.dens ~ 
                                       poly( Population.Density, 3, raw = TRUE ) +
                                       poly( norm.intptlat10, 3, raw = TRUE ) + poly( norm.intptlon10, 3, raw = TRUE ), 
                                       data = tract.demographics.kc.routes 
                                   )

summary( mod.tract.density.latlon.cube )
AIC( mod.tract.density.latlon.cube )

