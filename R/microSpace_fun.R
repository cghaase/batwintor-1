#' microSpace
#'
#' \code{microSpace} displays the influence of micro habitat selection
#' on predicted bat hibernation length.
#'
#' @param mod.df Model dataframe returned from \code{\link{hibernationModel}}
#' @param save.name optional agrument for saving
#' @param ... agruments to be passed to \code{\link{ggsave}}
#'
#' @return
#' @export
#'
#' @examples
microSpace <- function(mod.df, save.name = NULL, ...){
  ## modify dataframe coming in
  mod.dif <- mod.df %>%
    group_by_(~Ta, ~pct.rh) %>%
    summarise_(max.null = ~hour.to.month(max(time*surv.null)),
               max.inf = ~hour.to.month(max(time*surv.inf))) %>%
    mutate_(diff = ~(max.inf - max.null)) %>%
    tidyr::gather(key = "INF", value = "Months", starts_with("max"), factor_key = T)



  dz <- ggplot(mod.dif, aes_(~Ta, ~pct.rh, z = ~Months))  +
    scale_fill_gradientn("Months",
                         colors = c("#e66101", "#fdb863","#ffffff", "#b2abd2", "#5e3c99"), #purp low orange hi
                         limits = c(0,10)
    ) +
    geom_raster(aes_(fill = ~Months), interpolate = T) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    geom_contour(binwidth = 1,
                 colour = "grey15") +
    ggtitle("Micro habitat selection") +
    xlab("Temperature (C)") +
    ylab("Relative Humidity (%)")+
    theme_minimal()+
    theme(plot.title = element_text(size = 18,  family="serif"),
          axis.title = element_text(size = 16,  family="serif"),
          axis.text = element_text(size = 16,  family="serif"),
          aspect.ratio = 1,
          legend.key.size = unit(42, "points"),
          legend.title = element_text(size = 16,  family="serif"),
          legend.text = element_text(size = 16,  family="serif")) +
    facet_wrap(~INF, ncol = 2)

  if(!is.null(save.name)){
    ggsave( filename = save.name, ...)
    return(invisible(NULL))
  }
  return(dz)

}
