#' Compile monthly climate data into seasonal groupings
#'
#' One of the key challenges in dendroecology work is identifying the groupings of monthly
#' climate data most correlated with annual radial growth.  This function applies the approach used by
#' Meko et al. (2011) in their *seascorr* function to create groupings of monthly climate, based on user-defined windows.
#'

#' @param data data frame with monthly climate. This must be a data frame with only 12 columns (one for each month), and
#' years as row names.  Function will fail if data frame does not match these specs.  The intention is that user passes one climate variable to this function at a time
#' (but see examples below for ways to use this function efficiently for multiple climate variables).
#' @param windows over what time windows do you want to group the data?  The default is c(1,3,6,9) monthly groupings.
#' @param type do you want the monthly groupings to be mean (for temperature data) or sum (for precipitation data)?  Default is mean.
#' @returns A data frame with columns as monthly groupings of climate variable.  Because we are often interested in testing for lagged climate effects,
#'this function returns the climate for the previous and current year on the same row.  Climate variables from previous year have "prev_" in their name.
#'The month names also indicate the *last* month in the seasonal grouping.  For example, the value in *Feb* column for a 3-month window is the mean (or sum)
#'of that climate variable for December, January and February.
#' @export
#'
#' @examples
#'
#'library(dplyr)
#'library(stringr)
#'library(tibble)
#'library(tidyr)
#'library(ggplot2)
#'
#'# Apply function to a data frame with single climate variable
#'af1_clim %>%
#'filter(var=="PPT") %>% # filter for precipitation data
#'arrange(year) %>%
#'column_to_rownames("year") %>% # remember this step if you have year in column!
#'dplyr::select(-var) %>% # have to remove all non-climate columns from data frame
#'create_climate_seasons(windows=c(3,6))
#'
#'# Apply function efficiently to data frame with more than one climate variable
#'# Note the code below only uses type="mean".
#'unique(af1_clim$var) %>%
#'map_dfr(~af1_clim %>%
#'        filter(var==.x) %>%
#'        column_to_rownames("year") %>%
#'        dplyr::select(-var) %>%
#'        create_climate_seasons(windows=c(1,3,6)) %>%
#'        add_column(var=.x,.before="window")) %>%
#' mutate(var=factor(var))
#'
#'

#'
create_climate_seasons<-function(data,windows=c(1,3,6,9),type="mean") {

  # Test function
  if(ncol(data)!=12) stop("Data frame does not have 12 columns, please see instructions")

  # Sub-function to create seasonal means using a window
  seas_compile<-function(window,data,type) {


      # First, create seasonal means and sums
      seas_data<-
        data %>%
        rownames_to_column("year") %>%
        mutate(year=as.integer(year)) %>%
        arrange(year) %>%
        pivot_longer(-year,names_to="month",values_to="value") %>%
        runner::run_by(k=window,na_pad=T) %>%
        mutate(seas_grp=case_when(type=="mean"~runner::runner(x=.,f=function(x){mean(x$value)}),
                                  type=="sum"~runner::runner(x=.,f=function(x){sum(x$value)}))) %>%
        dplyr::select(year,month,seas_grp) %>%
        add_column(window=window,.after="month") %>%
        pivot_wider(names_from=month,values_from=seas_grp)

        # Second, get current and previous year seasonal means on the same row
      seas_data %>%
      mutate(year=year+1) %>% # add year to make previous
      slice(-n()) %>% # remove last row
      pivot_longer(-c(year,window)) %>%
      mutate(name=paste("prev",name,sep="_")) %>% # add previous to names
      bind_rows(seas_data %>%
                  pivot_longer(-c(year,window))) %>%
      pivot_wider() %>%
        arrange(year) %>%
        return()
  }

  # Apply subfunction for all windows
  windows %>%
    map_dfr(~seas_compile(window=.x,data=data,type=type)) %>%
    mutate(window=factor(window)) %>%
    arrange(window,year) %>%
    return()
}

