#' Identify trees within a neighborhood
#'
#' This function analyzes stem map data and identifies which trees lie within a certain distance of each other.  The function also determines which trees are situated within a certain distance of the plot boundary (generally
#' these trees will be removed from neighborhood analysis for that given distance).
#'
#'
#' @param x data frame with stem mapped trees for a \strong{single plot}.  Must be an \code{sf} object with coordinates in metres.
#' @param buffer_radius Width of radius around each tree, in metres.  This is used to define a tree's 'neighborhood'.
#' @param plot_radius Plot radius, in metres.
#' @param ids Vector of tree identifiers.  Must be same length as `nrow(x)` and it is assumed that \code{ids[1]} corresponds to first row of x, and so on.
#' @param diam Optional.  Vector of tree diameters, which if provided, must be in centimetres and will be used additively to increase buffer size.  If provided, must be equal length to data in x.
#' @param wt Optional.  Vector of weightings, which if provided, must be in metres and will be used multiplicatively to increase buffer size. If provided, must be equal length to data in x.
#'@returns A data frame with two columns that you can bind to x.  The first column is a nested data frame with `neighbour_treeID` providing the identifier of the neighbour tree,
#'and `trees_overlap` is a logical stating whether the tree buffers overlap (i.e., trees are within each other's "neighbourhood").  The second column `overlaps_plot` states if the tree's buffer overlaps with the plot boundary (in which case this tree should probably
#'be excluded from the analysis, as its full neighborhood extends past the plot and is therefore unknown).
#'@export
#'@examples
#'
#'library(dplyr)
#'library(stringr)
#'library(tibble)
#'library(sf)
#'library(tidyr)
#'library(ggplot2)
#'
#'# Create a data frame of trees, diameters and their positions
#'x<-
#'  tibble::tribble(
#'    ~treeID,~X,~Y,~dbh,
#'    "Tree A",2,3,32.5,
#'    "Tree B",-1,0,15.6,
#'    "Tree C",-2.7,-2.7,34.7
#'  ) %>%
#'  sf::st_as_sf(coords=c("X","Y"))
#'
#'  # Output from function.
#'x %>%
#'  sf::st_drop_geometry() %>%
#'  dplyr::left_join(
#'    list_tree_neighbour(x,buffer_radius=1.5,plot_radius=3.99,diam=x$dbh,ids=x$treeID),
#'    by="treeID") %>%
#'  tidyr::unnest(-c(treeID,overlaps_plot))
#'
#'# Function reports that tree 3 and 2 neighborhoods overlap when buffer is adjusted for tree diameter.
#'
#'  # Plotting to double check
#'x %>%
#'  ggplot2::ggplot()+
#'
#'  # first plot the plot area
#'  ggplot2::geom_sf(
#'    data=
#'      data.frame(X=0,Y=0) %>%
#'      sf::st_as_sf(coords=c("X","Y")) %>%
#'      sf::st_buffer(dist=3.99),
#'    alpha=0.2)+
#'
#'  # plot tree positions and their diameter
#'  ggplot2::geom_sf(data=
#'                     x %>%
#'                     st_buffer(dist=.$dbh/100),alpha=0.8,col="green")+
#'
#'  # plot buffer around each tree that adds DBH to the 1.5m radius, in red
#'  ggplot2::geom_sf(data=
#'                     x %>%
#'                     sf::st_buffer(dist=.$dbh/100+1.5),alpha=0.2,col="red")+
#'
#'  # plot buffer around each tree that ignores DBH, in blue
#'  ggplot2::geom_sf(data=
#'                     x %>%
#'                     sf::st_buffer(dist=1.5),alpha=0.2,col="blue")+
#'
#'  ggplot2::geom_sf()+
#'  ggplot2::geom_sf_label(aes(label=treeID))+
#'  ggplot2::theme_minimal()
#' # You can see that tree 3 and 2 neighborhoods only overlap if buffer is adjusted based on diameter.  Output from function is correct.
#'

list_tree_neighbour<-function(x,buffer_radius,plot_radius=12.6,ids,diam=NULL,wt=NULL) {

# Error messages
  if(any(class(x)%in%"sf")) {} else {stop("You must provide x as an sf object")}
  if(is.null(ids)) stop("You must provide tree ids")


  # incorporate diameter data into buffer
  if(is.null(diam)) {buffer_radius=buffer_radius} else {

    if(length(diam)!=nrow(x)) {stop("Length of diameter data does not match nrow(x)")} else {

      buffer_radius=buffer_radius+(diam/2/100) }} # remember that diameter has to be converted to metres!

  # incorporate weights into buffer
  if(is.null(wt)) {buffer_radius=buffer_radius} else {

    if(length(wt)!=nrow(x)) {stop("Length of weights does not match nrow(x)")} else {
      buffer_radius=buffer_radius*wt}}

  # determine which tree buffers overlap
  tree_overlap<-
    x %>%
    sf::st_buffer(dist=buffer_radius) %>%
    sf::st_overlaps() %>%
    as.matrix() %>%
    as.data.frame() %>%
    setNames(ids) %>%
    add_column(treeID=ids,.before=1) %>%
    tidyr::pivot_longer(-treeID,names_to="neighbour_treeID",values_to="trees_overlap") %>%
    tidyr::nest(overlaps_trees=c(neighbour_treeID,trees_overlap))

  # determine if tree buffers overlap with plot
  plot_overlap<-
    x %>%
    sf::st_buffer(dist=buffer_radius) %>%
    sf::st_overlaps(data.frame(X=0,Y=0,id="plot centre") %>%
                   sf::st_as_sf(coords=c("X","Y")) %>%
                   sf::st_buffer(dist=plot_radius)) %>%
    as.matrix() %>%
    as.data.frame() %>%
    setNames("overlaps_plot") %>%
    tibble::add_column(treeID=ids,.before=1)

  # Stop function
  if(nrow(plot_overlap)!=nrow(x) | nrow(tree_overlap)!=nrow(x)) {

    stop("Error: the output is a different size than nrow(x).  Something wrong.")

  }

  # return
  tree_overlap %>%
  dplyr::full_join(plot_overlap,by="treeID") %>%
   return()
}







