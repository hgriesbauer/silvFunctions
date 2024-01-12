#' Calculate tree expansion factors from variable-radius plots
#'
#' Small function to calculate tree expansion factors for each tree, based on BAF and diameter.  Tree expansion factors can be summed to get total stems per hectare.
#'
#' @param x vector of tree diameter data.
#' @param baf basal area factor used for plots (must be same for all plots).
#' @param num_plots number of plots.  You can provide either (i) a single integer with the number of plots or (ii) a vector of plot identifiers.  If the latter,
#' then the function will determine number of plots using \code{length(unique(num_plots))}.
#'
#' @return A vector with tree expansion factor for each tree.
#' @export
#' @examples
#' data.frame(dbh=runif(min=7.5,max=56.7,n=10)) %>%
#' dplyr::mutate(tree_exp=tree_exp(x=dbh,baf=7,num_plots=1))
#'
#' # Can combine with cut_dbh_class to calculate SPH by diameter class
#' data.frame(dbh=runif(min=7.5,max=56.7,n=100)) %>%
#' dplyr::mutate(dbh_class=silvFunctions::cut_dbh_class(dbh)) %>%
#' dplyr::mutate(tree_exp=tree_exp(x=dbh,baf=7,num_plots=15)) %>%
#' dplyr::group_by(dbh_class) %>%
#' dplyr::summarise(sph=sum(tree_exp))


tree_exp<-function(x,baf,num_plots) {

  n<-ifelse(length(num_plots)>1,length(unique(num_plots)),num_plots)

  return(baf/((x/2/100)^2*pi)/n)
}







