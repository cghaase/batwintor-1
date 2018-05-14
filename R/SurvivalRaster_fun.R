#' Converts model results dataframe into results raster.
#'
#' \code{survivalRaster} uses supplied environmental rasters to query model
#' results and creating a raster filled with maximum hibernation estimates for
#' hibernation without infection, with infection and the difference between the
#' two.
#'
#' @param mod.df results from \code{DynamicEnegryPd}
#' @param pct.rh.rast raster of humidity
#' @param temp.rast raster of temperature values either in Kevil or degrees
#' Celcius
#'
#' @details It is suggested that both raster layers have the same extent,
#' resolution, and projection as there are currently no internal checks on the
#' consistance between rasters. Values for output are drawn from the humidity
#' layer.
#'
#'
#' @note humidity rasters in the original Hayman et al. 2016 represent the average
#' of the average humidities for the months Jan-Mar.
#' @family Plot Functions
#' @seealso \code{\link{dangerZone}}; \code{\link{survivalMultiplot}}; \code{\link{survivalPlotter}};
#' \code{\link{survivalHistogram}}
#' @export

survivalRaster <- function(mod.df, pct.rh.rast, temp.rast){
  #Raster modifications for Kelvin temperatures
  if(summary(temp.rast)[1] > 200){
    temp.c <- temp.rast - 273
  } else{
    temp.c <- temp.rast
  }

  #Creating output raster dimensions
  out <- raster(pct.rh.rast); values(out) <- NA
  out.s <- stack(out,out,out); names(out.s) <- c("max.inf", "max.null", "diff")

  #Extract data from rasters  to matrix for speed
  pct.rh <- as.matrix(pct.rh.rast, nrow = nrow(pct.rh.rast), ncol = ncol(pct.rh.rast))
  temp <- as.matrix(temp.c, nrow = nrow(temp.c), ncol = ncol(temp.c))

  mod.dif <- mod.df %>%
    group_by_(~Ta, ~pct.rh) %>%
    summarise_(max.null = ~hour.to.day(max(time*surv.null)), max.inf = ~hour.to.day(max(time*surv.inf))) %>%
    mutate_(diff = ~(max.null-max.inf)) %>%
    ungroup %>% data.table

  ####Look Up Table ####
  #Vectors for look up table structure
  Ta_vals <- unique(mod.dif$Ta)
  pct.rh_vals <- unique(mod.dif$pct.rh)

  #Look Up Table
  lut <- array(NA, dim=c(length(Ta_vals), length(pct.rh_vals), 3)) # 3 for max.inf, max.null, diff
  dimnames(lut)[[1]] <- Ta_vals
  dimnames(lut)[[2]] <- pct.rh_vals
  dimnames(lut)[[3]] <- c("max.inf", "max.null", "diff")

  #Fill look up table
  for (i in seq_len(nrow(mod.dif))) {
    d <- mod.dif[i,]
    if (i %% 10000 == 0) {
      cat("up to", i, "of", nrow(mod.dif), "\n")
    }
    lut[as.character(d$Ta), as.character(d$pct.rh),] <- c(d$max.inf,
                                                            d$max.null, d$diff)
  }

  ####Find closest####
  find_closest <- function(x, y) {
    # Find the closest item in the vector y to x.
    # NOTE: Assumes that y is increasing, equi-spaced vector
    dy <- (y[length(y)] - y[1]) / (length(y)-1)
    wch <- round((x - y[1]) / dy + 1)
    # check the range.
    clamp <- function(x, xmin, xmax) {
      min(max(x, xmin),xmax)
    }

   clamp(wch, 1, length(y))
  }


  #Run lookup
  for(j in 1:nlayers(out.s)){
    #Create output matrix
    out.z <- matrix(ncol = ncol(pct.rh), nrow = nrow(pct.rh))
    for(i in 1:length(pct.rh)){
      # first find the closest humidity and Ta
      if(i %% 1000 == 0){
        cat(j, "of", nlayers(out.s), "up to", i, "of", length(pct.rh), "\n")
      }
      pct.rh_i <- find_closest(pct.rh[[i]], pct.rh_vals)
      Ta_i  <- find_closest(temp[[i]], Ta_vals)
      out.z[[i]] <- lut[Ta_i, pct.rh_i,j]
    }
    # Set values back from matrix to raster
    out.s[[j]] <- setValues(out.s[[j]], out.z)
    }

  return(out.s)
}
