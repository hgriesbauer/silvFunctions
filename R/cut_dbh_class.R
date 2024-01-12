#' Cut diameters into classes
#'
#' This function cuts a vector of diameter measurements into discrete diameter classes.  User specifies diameter class width and type of label desired.
#'
#' Note that cuts are not closed on the right.  This means that a particular value falling on a cut boundary will belong to the next largest class, not smallest.
#' For example, a tree with 10.0cm will be placed in the (10.0-15.0) class, not the (5.0-10.0) class.
#'
#' @param x Vector of diameter data.
#' @param size_class Width of individual size classes.  Defaults to 5 (cm).
#' @param min_size Lowest value of first interval. Defaults to 7.5 (cm).
#' @param labels Must be one of c("mid","min","max","label").  Specify what type of cut labels you want.  Defaults to mid-point.
#' @return A vector with labels.
#' @export
#' @examples
#' cut_dbh_class(x=runif(min=7.5,max=56.7,n=100),labels="min")
#'
#' data.frame(dbh=runif(min=7.5,max=56.7,n=100)) %>%
#' dplyr::mutate(dbh_class_mid=cut_dbh_class(dbh),
#'       dbh_class_label=cut_dbh_class(x=dbh,labels="label"))

cut_dbh_class<- function(x, size_class=5,min_size=7.5,labels="mid") {

    stopifnot(labels %in% c("mid","min","max","level","label"))

    cut_levels<-
      cut(x,breaks=seq(from=min_size,to=max(x,na.rm=T)+size_class,by=size_class),include.lowest=T,right=F,labels=F)

    tibble::tibble(
      level=cut_levels,
      min=(cut_levels-1)*size_class+min_size,
      mid=(cut_levels-1)*size_class+min_size+(size_class/2),
      max=min+size_class,
      label=paste0(min," - ",max)) %>%
      dplyr::pull(dplyr::all_of(labels)) %>%
      return()


  }


