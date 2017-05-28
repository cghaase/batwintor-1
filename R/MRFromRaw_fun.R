#' Takes raw metabolic data and returns cleaned estimates for each species.
#'
#' \code{MRFromRaw} Clean the input of raw metabolic data and return species
#' estimates.
#'
#' @param x a dataframe
#' @param species species name or code (must be contained within \code{x})
#' @return Object of class \code{MRFromRaw} which contains adjusted estimates
#' of a species resting metabolic rate (RMR), torpor metabolic rate minimum
#' (TMRmin), the temperature at which that rate is achieved (Ttormin), and
#' average mass of sampled individuals.
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
  # sp.l <- list()
  for(i in 1:length(temp)){
    sp.df.t <- sp.df %>%
      filter(Ta == temp[i])
    sp.spEM <- spEMsymloc(sp.df.t$VO2.ml.h.g, mu0=quantile(sp.df.t$VO2.ml.h.g,
                                                           c(.25,.75)),stochastic=TRUE)
    sp.x.temp[i,] <- c(temp[i], sp.spEM$mu[1], mean(sp.df.t$mass.in),
                       nrow(sp.df.t))
    # assign(paste0(sp,".",i),sp.spEM)
    # sp.l <- c(sp.l, paste0(sp,".",i))
  }
  ddf <- as.data.frame(sapply(sp.x.temp, as.numeric))
  dat.out <- data.frame(RMR = ddf[which(ddf$Ta == 35),]$MR,
                        TMRmin = min(ddf$MR),
                        Ttormin = ddf[which(ddf$MR == min(ddf$MR)),]$Ta,
                        mass = weighted.mean(ddf$mass, sapply(ddf$n, FUN =
                                                    function(x){x/max(ddf$n)})))
  # objs <- dynGet(sp.l)
  # out <- list(MR.est = dat.out, unlist(objs))
  out <- dat.out
  # out <- list(RMR = dat.out$RMR,
  #             TMRmin = dat.out$TMRmin,
  #             Ttormin = dat.out$Ttormin,
  #             mass  = dat.out$mass)
   class(out) <- c("MRFromRaw", "data.frame")
  return(out)
}
