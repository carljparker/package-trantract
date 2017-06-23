
library( trantract )

data( stops )
data( route.stops.df )
data( kct.shp.proj )
is( kct.shp.proj )
data( tract.demographics.kc )
head( tract.demographics.kc )

head( stops )
head( route.stops.df )

census_tract_from_route( 28 )
census_tract_from_stop( 28100 )
route_stops_from_census_tract( 30 )
plot_tract( 30 )

help( trantract )
vignette( "trantract" )
