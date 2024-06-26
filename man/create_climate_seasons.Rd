% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_climate_seasons.R
\name{create_climate_seasons}
\alias{create_climate_seasons}
\title{Compile monthly climate data into seasonal groupings}
\usage{
create_climate_seasons(data, windows = c(1, 3, 6, 9), type = "mean")
}
\arguments{
\item{data}{data frame with monthly climate. This must be a data frame with only 12 columns (one for each month), and
years as row names.  Function will fail if data frame does not match these specs.  The intention is that user passes one climate variable to this function at a time
(but see examples below for ways to use this function efficiently for multiple climate variables).}

\item{windows}{over what time windows do you want to group the data?  The default is c(1,3,6,9) monthly groupings.}

\item{type}{do you want the monthly groupings to be mean (for temperature data) or sum (for precipitation data)?  Default is mean.}
}
\value{
A data frame with columns as monthly groupings of climate variable.  Because we are often interested in testing for lagged climate effects,
this function returns the climate for the previous and current year on the same row.  Climate variables from previous year have "prev_" in their name.
The month names also indicate the \emph{last} month in the seasonal grouping.  For example, the value in \emph{Feb} column for a 3-month window is the mean (or sum)
of that climate variable for December, January and February.
}
\description{
One of the key challenges in dendroecology work is identifying the groupings of monthly
climate data most correlated with annual radial growth.  This function applies the approach used by
Meko et al. (2011) in their \emph{seascorr} function to create groupings of monthly climate, based on user-defined windows.
}
\examples{

library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(ggplot2)

# Apply function to a data frame with single climate variable
af1_clim \%>\%
filter(var=="PPT") \%>\% # filter for precipitation data
arrange(year) \%>\%
column_to_rownames("year") \%>\% # remember this step if you have year in column!
dplyr::select(-var) \%>\% # have to remove all non-climate columns from data frame
create_climate_seasons(windows=c(3,6))

# Apply function efficiently to data frame with more than one climate variable
# Note the code below only uses type="mean".
unique(af1_clim$var) \%>\%
map_dfr(~af1_clim \%>\%
       filter(var==.x) \%>\%
       column_to_rownames("year") \%>\%
       dplyr::select(-var) \%>\%
       create_climate_seasons(windows=c(1,3,6)) \%>\%
       add_column(var=.x,.before="window")) \%>\%
mutate(var=factor(var))



}
