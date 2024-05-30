#' Bootstrapped correlation coefficients
#'
#' This function simply returns a correlation coefficient (and other statistics) between
#' two variables.  Function uses the cor.test and boot.ci functions.
#'
#' @details{
#'
#' * Confidence intervals are 95%.
#' * Bootstrapped CI are bias-corrected accelerated.
#' * NA values are allowed.
#'
#'
#' }
#'
#'
#' @param dat data frame
#' @param x name of first column (function will fail if not a character).
#' @param y name of second column (function will fail if not a character).
#' @param nboot Number of replicates for bootstrapping.  Defaults to 1000.
#'@returns A data frame with five columns:
#'  \itemize{
#'  \item{corr_coef: correlation coefficient, using cor.test}
#'  \item{p_value: p value of the test, using cor.test}
#'  \item{n: number of observations used to calculate coefficient}
#'  \item{LCL: Lower 95% confidence interval}
#'  \item{UCL: Upper 95% confidence interval}
#'  \item{SIG: Significance based on CI}
#'
#'
#'  }
#'
#'
#'
#'@export
#'@examples
#'
#'# Join climate and residual chronology data for Ancient Forest together
#'  af1_clim %>%
#'  dplyr::filter(var=="Tmax") %>%

#'  dplyr::left_join(af1_crn %>%
#'              tibble::rownames_to_column("year") %>%
#'              dplyr::mutate(year=as.integer(year))) %>%
#'  corr_boot(x="res",y="Jan")
#'
#'
#'
corr_boot<-function(dat,x,y,nboot=1000) {

  stopifnot(nrow(dat)>10,is.character(x),is.character(y),
            x %in% names(dat),
            y %in% names(dat))

  dat<-
    as.data.frame(dat) %>%
    dplyr::select(all_of(x),all_of(y))

  if(sd(dat[,1],na.rm=T)==0 | sd(dat[,2],na.rm=T)==0) {

    # send warning
    warning("Standard deviation of at least one variable is zero; returning NA")

    out<-
      data.frame(corr_coef=NA,
               p_value=NA,
               n=NA,
               LCL=NA,
               UCL=NA,
               SIG=NA)


  } else {

  # Extract correlation coefficient and pvalue
  cor_out<-cor.test(x=dat[,1],y=dat[,2])

  # Extract correlation coefficient confidence limits
  cor_ci<-
    boot(dat,
         statistic = function(data, i) {
           cor(data[i, 1], data[i, 2], method='pearson',use="pairwise.complete.obs")
         },
         R = nboot) %>%
    boot.ci(type="bca")

    out<-
    data.frame(corr_coef=cor_out$estimate,
                  p_value=cor_out$p.value,
               n=nrow(drop_na(dat)),
                  LCL=cor_ci$bca[4],
                  UCL=cor_ci$bca[5]) %>%
    mutate(SIG=ifelse(sum(sign(c(LCL,UCL)))==0,"FALSE","TRUE"))
  } # end if statement

  return(out)
} # close function




