#' Takes raw metabolic data and returns cleaned estimates for each species.
#'
#' \code{MRFromRaw} Clean the input of raw metabolic data and return species
#' estimates.
#'
#' @param x a dataframe
#' @param species species name or code (must be contained within \code{x})
#' @param plot option to provide a plot of the parameter estimations
#'
MRFromRaw <- function(x, sp, plot = T){
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
  for(i in 1:length(temp)){
    sp.df.t <- sp.df %>%
      filter(Ta == temp[i])
    sp.spEM <- spEMsymloc(sp.df.t$VO2.ml.h.g, mu0=quantile(sp.df.t$VO2.ml.h.g,
                                                           c(.25,.75)),stochastic=TRUE)
    if(plot == T){
      plot(sp.spEM, title =paste0(sp," spEm Density Curves ",temp[i]," degress"))
    }
    sp.x.temp[i,] <- c(temp[i], sp.spEM$mu[1], mean(sp.df.t$mass.in),
                       nrow(sp.df.t))
  }
  df <- as.data.frame(sapply(sp.x.temp, as.numeric))
  dat.out <- data.frame(RMR = df[which(df$Ta == 35),]$MR,
                        TMRmin = min(df$MR),
                        Ttormin = df[which(df$MR == min(df$MR)),]$Ta,
                        mass = weighted.mean(df$mass, sapply(df$n, FUN =
                                                               function(x){x/max(df$n)}))
  )
  return(dat.out)
}
