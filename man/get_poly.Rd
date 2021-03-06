% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projections.R
\name{get_poly}
\alias{get_poly}
\title{returns a poly as an owin (default) or as list of x (longs) and y (lats)}
\usage{
get_poly(
  long = -100,
  lat = 15,
  length = 5,
  width = 2,
  angle = 15,
  owin = T,
  clockwise = F,
  left = 1,
  right = 1,
  top = 1,
  bottom = 1
)
}
\arguments{
\item{long}{reference longtitude}

\item{lat}{reference latitude}

\item{length}{length in degrees}

\item{width}{width in degrees}

\item{angle}{angle of poly central axis}

\item{owin}{boolean (default= TRUE) to retuen owin or list}

\item{clockwise}{boolean (default=FALSE) reverse directions  allows ? holes}

\item{left}{}

\item{right}{}

\item{top}{}

\item{bottom}{}
}
\value{

}
\description{
returns a poly as an owin (default) or as list of x (longs) and y (lats)
}
