% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_cast_bis.R
\name{st_cast_bis}
\alias{st_cast_bis}
\title{st_cast debugged}
\usage{
st_cast_bis(sfdata)
}
\arguments{
\item{sfdata}{sf object}
}
\value{
cast polygons
}
\description{
st_cast multipolygons to polygons, avoiding bugs when input spatial data are "incongruent".
See : https://github.com/r-spatial/sf/issues/763
}
