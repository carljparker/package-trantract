% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/route_stops_from_census_tract.R
\name{route_stops_from_census_tract}
\alias{route_stops_from_census_tract}
\title{Get all the stops that serve a census tract and all the routes that
serve those stops.}
\usage{
route_stops_from_census_tract(tract)
}
\arguments{
\item{tract}{A string that specifies a US Census Bureau census tract.}
}
\value{
A two-column data frame of stops and routes. If a stop is
served by more than one route, that stop will appear in multiple
rows of the dataframe, once for each route.
}
\description{
Get all the stops that serve a census tract and all the routes that
serve those stops.
}
\examples{

route_stops_from_census_tract( 30 )
#     stop  route
# 1  13600     15
# 2  13600 D Line
# 3  13620     15
# 4  13620 D Line
# 5  13640     15
# 6  13640 D Line
# 7  14300     15
# 8  14300 D Line
# 9  14320     15
# 10 14320 D Line
# 11 14322 D Line
# 12 14340     15
# 13 14340 D Line
# 14 28080     28
# 15 28100     28
# 16 28100    994
# 17 28120     28
# 18 28130     28
# 19 35580     45
# 20 35580     40
# 21 35600     45
# 22 35630     45

}

