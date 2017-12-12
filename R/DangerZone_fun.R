#' Plotting the difference between infected and and uninfected hibernating
#' bats.
#'
#' \code{DangerZone} plots the difference in hibernation length potentials
#' between infected and uninfected bats accross the environmental space.
#'
#' @return returns an environmental surface accross which the difference
#' between infected and uninfected hibernation times is plotted.
#'
#' @param mod.df results data from \code{\link{DynamicEnergyPd}}
#' @param title title for the figure
#' @param save.name a name and file path relative to the working directory
#' where the figure will be saved.
#' @param ... used to pass arguments to \code{\link[ggplot2]{ggsave}}
#'
#' @details Function will be updated in the future to include more plotting
#' parameter accessability, and potentially a raster method as well.
#' \url{https://www.youtube.com/watch?v=kyAn3fSs8_A}
#' @author L. Cain, K. Loggins
#' @family Plot Functions
#' @seealso \code{\link{MapFigs}}; \code{\link{SurvPlotter}}; \code{\link{DiffHist}}
#' @export
DangerZone <-function(mod.df, title, save.name=NULL, ...){
  mod.dif <- mod.df %>%
    group_by_(~Ta, ~pct.rh) %>%
    summarise_(max.null = ~max(time*surv.null),max.inf = ~max(time*surv.inf)) %>%
    mutate_(diff = ~hour.to.month(max.inf - max.null))
  ungroup %>% data.table

  dz <- ggplot(mod.dif, aes_(~Ta, ~pct.rh, z = ~diff))  +
    scale_fill_gradientn("Difference\n(months)",colors = c("gold1", "grey95", "steelblue3"),
                         limits = c(-8,0)) +
    geom_raster(aes_(fill = ~diff), interpolate = T) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    geom_contour(binwidth = 1,
                 colour = "grey25") +
    ggtitle(title) +
    xlab("Temperature (C)") +
    ylab("Relative Humidity (%)")+
    theme_minimal()+
    theme(plot.title = element_text(size = 18,  family="serif"),
          axis.title = element_text(size = 16,  family="serif"),
          axis.text = element_text(size = 16,  family="serif"),
          aspect.ratio = 1,
          legend.key.size = unit(42, "points"),
          legend.title = element_text(size = 16,  family="serif"),
          legend.text = element_text(size = 16,  family="serif"))

  if(!is.null(save.name)){
    ggsave( filename = save.name, ...)
    return(invisible(NULL))
  }
  return(dz)

}
