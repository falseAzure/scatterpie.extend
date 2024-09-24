#' @title Plot Scatterpie Chart
#'
#' @description
#' This function creates a scatterpie plot using ggplot2. It overlays scatterpie points on a spatial plot,
#' where the size of the scatterpie points are scaled to \code{var} and the pie slices are scaled to \code{cols}.
#' The size of the scatterpie points can be scaled by \code{area} or radius.
#'
#' @param sf_object A spatial object of class sf, sfc or sfg.
#' @param var A string representing the name of the variable in the data frame to be transformed.
#' @param cols A vector of variables to be used for the pie slices in the scatterpie plot. If NULL only \code{var} will be used (default is NULL).
#' @param legend_name A string to be used as the legend title (default is "Typ").
#' @param ratio A numeric value representing the ratio used for scaling (default is 0.08).
#' @param area A logical value indicating whether the transformation should be based on area (default is TRUE).
#' @param legend.x A numeric value representing the x-coordinate of the legend.
#' @param legend.y A numeric value representing the y-coordinate of the legend.
#' @param legend.n An integer representing the number of circles in the legend.
#' @param color.pie The line color of the pie slices in the scatterpie plot (default is NA).
#' @param alpha.pie The transparency of the pie slices in the scatterpie plot (default is 0.9).
#' @param big.mark A string to be used as the big mark in the labeller function (default is ".").
#' @param decimal.mark A string to be used as the decimal mark in the labeller function (default is ",").
#' @param scale_cut A numeric value to be used as the scale cut in the labeller function (default is NULL).
#' @param background_plot A ggplot object to be used as the background of the scatterpie plot (default is NULL.)
#' @param ... Additional arguments to be passed to the \code{geom_scatterpie_legend} function.
#'
#' @return A ggplot2 scatterpie plot.
#'
#' @examples
#' \dontrun{
#' sf_object <- sf::st_read("path_to_your_shapefile.shp")
#' plot <- plot_scatterpie(sf_object, "your_variable")
#' }
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom scatterpie geom_scatterpie geom_scatterpie_legend
#' @export
plot_scatterpie <- function(sf_object,
                            var, cols=NULL,
                            legend_name="Typ",
                            ratio=0.08,
                            area=TRUE,
                            legend.x = 142767.4,
                            legend.y = 483225.9,
                            legend.n = 3,
                            color.pie = NA,
                            alpha.pie = 0.9,
                            big.mark = ".",
                            decimal.mark = ",",
                            scale_cut=NULL,
                            background_plot = NULL,
                            ...) {
  X <- NULL
  Y <- NULL
  r <- NULL
  aes <- NULL
  if(is.null(cols)) {
    cols <- c(var, "delta")
  }

  transformed_data <- transform_sf(sf_object = sf_object, var = var, ratio = ratio, area = area)
  data_map <- transformed_data$data
  multiplier <- transformed_data$multiplier

  if (legend_name %in% names(data_map)) {
    legend_name_new <- paste0(legend_name, "_")
    warning(paste0("The legend name '", legend_name, "' is already in the data. The legend name has thus been changed to '", legend_name_new, "'!\n Consider using a different name."))
  } else {
    legend_name_new <- legend_name
  }

  if (is.null(background_plot)) {
    plot <- ggplot2::ggplot()
  } else {
    plot <- background_plot
  }

  plot <- plot +
    scatterpie::geom_scatterpie(
      data = data_map %>% dplyr::filter(!is.na(r)),
      aes(x = X, y = Y, r = r),
      cols = cols, color = color.pie, alpha = alpha.pie, legend_name = legend_name_new, sorted_by_radius = TRUE
    ) +
    scatterpie::geom_scatterpie_legend(
      radius = data_map$r,
      labeller = function(x) transform_labeller(
        values = x,
        multiplier = multiplier,
        area = area,
        digits = 2,
        big.mark = big.mark,
        decimal.mark = decimal.mark,
        scale_cut=scale_cut,
        ...=...),
      n = legend.n, x = legend.x, y = legend.y, ... = ...
    )

  return(plot)
}
