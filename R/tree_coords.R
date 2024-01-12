#' Tree spatial coordinates
#'
#' Convert stem mapping data (azimuth and distance from plot centre) to xy coordinates
#'
#' This function simply takes the azimuth and distance between plot centre and individual tree positions, and converts them to xy coordinates to
#' allow for stem mapping and neighborhood analysis.
#'
#' Note: this function makes a number of assumptions:
#'\itemize{
#'\item{Azimuth}{ is recorded from plot centre to individual tree position, and is recorded in
#'true north, not magnetic north direction (In other words, function assumes that the data are corrected for declination); and}
#'\item{Distance}{ is horizontal distance, in metres.  No correction is made for slope (you must do this separately).}
#'}
#'
#'

#' @param data data must be provided in data frame.
#' @param azi name of column in 'data' that contains bearing from plot centre to trees, in degrees.
#' @param dist name of column in 'data' that contains distance between trees and plot centre, in metres.
#'
#'@returns A data frame with two columns (x and y cartesian coordinates) and same rows as input data.
#'@export


tree_coords <- function(data,azi,dist) {

  stopifnot(is.data.frame(data))

  x<-
  data.frame(x=cos((450-data[,azi])*pi/180)*data[,dist],
             y=sin((450-data[,azi])*pi/180)*data[,dist])

  names(x)<-c("x","y")
    return(x)}



