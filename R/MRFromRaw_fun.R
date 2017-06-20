#' Takes raw metabolic data and returns cleaned estimates for each species.
#'
#' \code{MRFromRaw} Clean the input of raw metabolic trials and return semi-
#' parametric metaboic estimates.
#'
#' @param x a dataframe
#' @param species species name or code (as a colname within \code{x})
#' @return \code{MRFromRaw} returns an list of class \code{MRFromRaw} with the
#' following items:
#' \item{df}{
#' A dataframe filled with adjusted estimates of a species resting
#' metabolic rate (RMR), torpor metabolic rate minimum (TMRmin), the
#' temperature at which that rate is achieved (Ttormin), and average mass of
#' sampled individuals.}
#' \item{spEM}{
#' A list of the \code{\link[mixtools]{spEMsymloc}} items created
#' which can be plotted or examined individually.}
#' @details \code{MRFromRaw} uses the \code{\link[mixtools]{spEMsymloc}} method
#' to preform semi-parimetric fitting of data using the first and third
#' quantiles as priors for stocastic process.
MRFromRaw <- function(x, sp){
  require(dplyr);require(mixtools)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  if(sp %!in% levels(x$species)){
    cat(paste0(sp," is not found within ", x, " please try again."))
  }
  sp.df <- x %>%
    filter(species == sp)
  temp <- levels(sp.df$Ta)
  sp.x.temp <- data.frame(matrix(nrow = length(temp), ncol = 4))
  names(sp.x.temp) <- c("Ta", "MR", "mass", "n")
  sp.l <- list()
  for(i in 1:length(temp)){
    sp.df.t <- sp.df %>%
      filter(Ta == temp[i])
    sp.spEM <- spEMsymloc(sp.df.t$VO2.ml.h.g, mu0=quantile(sp.df.t$VO2.ml.h.g,
                                                           c(.25,.75)),stochastic=TRUE)
    sp.x.temp[i,] <- c(temp[i], sp.spEM$muhat[1], mean(sp.df.t$mass.in),
                       nrow(sp.df.t))
    sp.l[[i]] <- sp.spEM #; names(sp.l[[i]]) <- paste0(sp,".",sp.df.t$Ta[i])
  }
  ddf <- as.data.frame(sapply(sp.x.temp, as.numeric))
  dat.out <- data.frame(RMR = ddf[which(ddf$Ta == 35),]$MR,
                        TMRmin = min(ddf$MR),
                        Ttormin = ddf[which(ddf$MR == min(ddf$MR)),]$Ta,
                        mass = weighted.mean(ddf$mass, sapply(ddf$n, FUN =
                                                    function(x){x/max(ddf$n)})))

  out <- list(df = dat.out, spEM = sp.l)
  class(out) <- c("MRFromRaw")
  return(out)
}
