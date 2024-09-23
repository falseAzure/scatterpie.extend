#' @title Transforms \code{values} for \pkg{scatterpie}
#'
#' @description
#' \code{transform_values} transforms the given \code{values} in accordance with a spatial data object so it can be used in the \pkg{scatterpie} package.
#' The values can optionally be transformed so they match the \code{area} of the plotted circles instead of the radius and scales it by a specified \code{ratio.}
#' It can also return the \code{multiplier} used for the transformation instead of the transformed values.
#'
#' @param values A numeric vector representing the values/size of the spatial data points.
#' @param sf_object A spatial data object in accordance with which the values are transformed.
#' @param ratio A numeric value representing the ratio used for scaling the values (default is 0.08).
#' @param area A logical value indicating whether the transformation should be based on the area instead of the radius (default is FALSE).
#' @param return_multiplier A logical value indicating whether to return the multiplier used for the transformation (default is FALSE).
#'
#' @return Returns the transformed values as a vector.
#' If return_multiplier is TRUE, the function returns the transformed values and the multiplier used for the transformation as a list.
#'
#' @examples
#' \dontrun{
#' values <- c(100, 10, 200, 40)
#' sf_object <- sf::st_read('path_to_your_shapefile.shp')
#' r <- transform_values(
#'   values = values,
#'   sf_object = sf_object,
#'   ratio = 0.08,
#'   area = TRUE,
#'   return_multiplier = FALSE
#' )
#' }
#' @importFrom sf st_bbox
#' @export
transform_values <- function(values, sf_object, ratio = 0.08, area = FALSE, return_multiplier = FALSE) {
    bounding_box <- sf::st_bbox(sf_object)

    x_dif <- bounding_box["xmax"] - bounding_box["xmin"]
    y_dif <- bounding_box["ymax"] - bounding_box["ymin"]
    min_dif <- min(x_dif, y_dif)

    if (area == TRUE) {
        values_sqrt <- sqrt(values)
        max_r <- max(values_sqrt, na.rm = T)
    } else {
        max_r <- max(values, na.rm = T)
    }

    ratio_values <- max_r/min_dif
    multiplier <- signif(ratio/ratio_values, 2)

    if (area == TRUE) {
        transformed_values <- values_sqrt * multiplier
    } else {
        transformed_values <- values * multiplier
    }

    if (return_multiplier) {
        return(list(values = transformed_values, multiplier = multiplier))
    }
    return(transformed_values)
}


#' @title
#' Reverses the transformation of \code{\link{transform_values}}
#'
#' @description
#' Reverses the transformation of \code{\link{transform_values}} so it can be used for the labeller in the \pkg{scatterpie} legend.
#'
#' @details
#' If the values were transformed using \code{\link{transform_values}} and is used in a \pkg{scatterpie} the labeller of the \pkg{scatterpie} legend has to be transformed as well.
#' This is done using \code{transform_labeller}, which should take the same arguments as \code{\link{transform_values}}.
#' It is used directly as a function call in the labeller argument of \code{\link[scatterpie]{geom_scatterpie_legend}}.
#'
#' @param values A numeric vector representing the values/size of the spatial data points.
#' @param multiplier The multiplier with which the \code{values} was transformed using \code{\link{transform_values}(return_multiplier = TRUE)}.
#' @param area A logical value indicating whether the back transformation should be based on the area instead of the radius (default is FALSE).
#' @param digits Number of digits to round to.
#' @param big.mark Used for formatting (big) numbers (e.g. 1,000,000).
#' @param decimal.mark Used for formatting decimal numbers (e.g. 1.2).
#'
#' @examples
#' \dontrun{
#' values <- c(100, 10, 200, 40)
#' sf_object <- sf::st_read('path_to_your_shapefile.shp')
#' ratio <- 0.08
#' area <- TRUE
#'
#' transformed_values <- transform_values(
#'   values = values,
#'   sf_object = sf_object,
#'   ratio = 0.08,
#'   area = area,
#'   return_multiplier = TRUE
#' )
#'
#' radius <- transformed_values$values
#' multiplier <- transformed_values$multiplier
#' geom_scatterpie_legend(
#'   radius = radius,
#'   labeller = function(x) {
#'     transform_labeller(
#'       values = x,
#'       multiplier = multiplier,
#'       area = area
#'     )
#'   }
#' )
#' }
#' @export
transform_labeller <- function(values, multiplier, area = FALSE, digits = 2, big.mark = ",", decimal.mark = ".") {
    if (area) {
        return(format(signif((values/multiplier)^2, digits), big.mark = big.mark, decimal.mark = decimal.mark))
    } else {
        return(format(signif(values/multiplier, digits), , big.mark = big.mark, decimal.mark = decimal.mark))
    }
}


#' @title Get Centroids of Spatial Features
#'
#' @description
#' This function calculates the centroids of the spatial features in a given spatial object.
#' The centroids are calculated as points on the surface of the spatial features.
#'
#' @param sf_object A spatial object of class sf, sfc or sfg.
#'
#' @return A tibble with the coordinates of the centroids (X, Y) of the spatial features in the input object.
#'
#' @examples
#' \dontrun{
#' sf_object <- sf::st_read('path_to_your_shapefile.shp')
#' centroids <- get_centroids(sf_object)
#' }
#' @importFrom sf st_point_on_surface st_coordinates
#' @importFrom dplyr as_tibble select
#' @export
get_centroids <- function(sf_object) {
    centroids <- sf::st_point_on_surface(sf_object)
    centroids <- cbind(centroids, sf::st_coordinates(centroids)) %>%
        dplyr::as_tibble() %>%
        dplyr::select(!("geometry"))
    return(centroids)
}


#' @title Transform Spatial Data
#'
#' @description
#' This function transforms a variable in a data frame based on the area of spatial features.
#' It then adds the transformed variable as a new column (r) to the data frame.
#'
#' @param data A data frame containing the variable to be transformed.
#' @param var A string representing the name of the variable in the data frame to be transformed.
#' @param sf_object A spatial object of class sf, sfc or sfg.
#' @param ratio A numeric value representing the ratio used for scaling (default is 0.08).
#' @param area A logical value indicating whether the transformation should be based on area (default is TRUE).
#'
#' @return A list containing the data frame with the transformed variable and the multiplier used for the transformation.
#'
#' @examples
#' \dontrun{
#' data <- read.csv('your_data.csv')
#' sf_object <- sf::st_read('path_to_your_shapefile.shp')
#' transformed_data <- transform_data(data, 'your_variable', sf_object, ratio = 0.1, area = TRUE)
#' }
#' @importFrom dplyr mutate
#' @export
transform_data <- function(data, var, sf_object, ratio = 0.08, area = TRUE) {
    transformed_data <- transform_values(data[[var]], sf_object = sf_object, ratio = ratio, area = area, return_multiplier = TRUE)
    multiplier <- transformed_data$multiplier
    radius <- transformed_data$values
    data_map <- data %>%
      dplyr::mutate(r = radius)
    data_map <- data_map %>%
      dplyr::mutate(delta = 0)
    return(list(data = data_map, multiplier = multiplier))
}



#' @title Transform Spatial Data
#'
#' @description
#' This function calculates the centroids of the spatial features in a given spatial object and
#' transforms a variable based on the area of spatial features.
#' It then adds the transformed variable as a new column (r) to the data frame.
#'
#' @details
#' Convenience function that wraps \code{\link{get_centroids}} and \code{\link{transform_data}}.
#' It returns a data frame, since \pkg{scatterpie} uses data frames as input.
#'
#' @param sf_object A spatial object of class sf, sfc or sfg.
#' @param var A string representing the name of the variable in the spatial object to be transformed.
#' @param ratio A numeric value representing the ratio used for scaling (default is 0.08).
#' @param area A logical value indicating whether the transformation should be based on area (default is TRUE).
#'
#' @return A list containing the data frame with the transformed variable and the multiplier used for the transformation.
#'
#' @examples
#' \dontrun{
#' sf_object <- sf::st_read('path_to_your_shapefile.shp')
#' transformed_data <- transform_data(data, 'your_variable', sf_object, ratio = 0.1, area = TRUE)
#' }
#' @export
transform_sf <- function(sf_object, var, ratio = 0.08, area = TRUE) {
  data <- get_centroids(sf_object = sf_object)

  transformed_data <- transform_data(data = data, var = var, sf_object = sf_object, ratio = ratio, area = area)
  data_map <- transformed_data$data
  multiplier <- transformed_data$multiplier

  return(list(data = data_map, multiplier = multiplier))
}
