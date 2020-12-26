\name{trantract-package}
\alias{trantract-package}
\alias{trantract}
\docType{package}
\title{
\packageTitle{trantract}
}
\description{
\packageDescription{trantract}
}
\details{

NB: This package requires a dev version of ggmap to avoid to avoid the
following error:

  Error: GeomRasterAnn was built with an incompatible version of ggproto.
  Please reinstall the package that provides this extension.

  \code{install_github("dkahle/ggmap")}

See the following URL for more information:

  \url{http://www.arilamstein.com/blog/2017/01/16/choroplethr-v3-5-3-now-cran/}

The trantract package contains the following functions and data sets.

The \code{census_tract_from_stop} function gets the census tract that
contains a King Country Metro stop.

The \code{census_tract_from_route} function gets all the census tracts
that are served by a King Country Metro route.

The \code{route_stops_from_census_tract} function gets all the stops
that serve a census tract and all the routes that serve those stops.

The \code{plot_tract} function plots a census tract on a map, and draws
all the King County Metro transit stops that are contained within that
tract.

The functions in the package are designed to work with data that is
provided in the package itself. King County Metro publishes transit data
in GTFS format, which is very comprehensive, but is not usable without
extensive pre-processing. 

Therefore, the package provides three data files: 

\code{kct.shp.proj} contains geo-spatial data for the US Census Bureau
census tracts for King County, WA. 

\code{route.stops.df} is a two-column dataframe that maps between routes and
stops.

\code{stops} is a four-column dataframe that contains metadata for all
the stops in the King Country Metro transit system. The metadata
includes GPS coordinates, the stop ID, a string that describes the stop
location, and the census tract that contains the stop.

The \code{route.stops.df} and \code{stops} data files were obtained by
processing the data published by King County Metro in GTFS format.

}
\author{
  Carl Parker (carljparker@gmail.com)

Maintainer: Carl Parker (carljparker@gmail.com)
}
\references{
For King County census tract data sets, see the following URL. Near the
bottom of the page is a section titled *ArcGIS Shapefiles* which has
links to download the data files.

  \url{http://www.seattle.gov/dpd/cityplanning/populationdemographics/geographicfilesmaps/2010census/default.htm}

For King County Metro transit, you can find data sets--in GTFS
format--that describe routes and stops for King County Metro transit at
the following URL.

  \url{http://metro.kingcounty.gov/GTFS/}

You can find instructions for how to interpret the preceding data files
at the following URL:

  \url{https://developers.google.com/transit/gtfs/reference?csw=1}

Note that the GTFS format is complex. The data provided in this package
was extracted from the GTFS files.

}
\keyword{ package }
\examples{
library( trantract )

data( stops )
data( route.stops.df )
data( kct.shp.proj )

head( stops )
head( route.stops.df )

census_tract_from_route( 28 )
census_tract_from_stop( 28100 )
route_stops_from_census_tract( 30 )
plot_tract( 30 )
}
