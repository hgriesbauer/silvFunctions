#' Calculate tree basal area from diameter
#'
#' Simple function to convert tree diameter to basal area.
#'
#' Function assumes that diameter is measured in centimetres.  Basal area is returned in metres.
#'
#' @param x vector of diameter data.
#' @return A vector with basal area.
#' @export
#' @examples
#' data.frame(dbh=runif(min=7.5,max=56.7,n=100)) %>%
#' dplyr::mutate(ba=d2ba(dbh))

d2ba<-function(x) {(x/2/100)^2*pi}







