#' Exclude points out of their city
#' @description
#' - Retire les points en dehors du contour de leur commune
#' - Exclude points out of a border
#'
#' @param sfpoints points (sf)
#' @param sfcontour polygon (sf)
#' @param verbose if TRUE : prints information
#'
#' @return sfpoints without points out of sfContour
#'
#' @examples
#' sfpoints <- addresses_sample[addresses_sample$code_commune_ref == "29039", ]
#' sfpoints <- sfpoints %>%
#'   sf::st_as_sf(
#'     coords = c("longitude", "latitude"),
#'     crs = 4326,
#'     na.fail = FALSE
#'   )
#' sfcontour <- contours_com_sample[contours_com_sample$code_insee == "29039", ]
#' verbose <- TRUE
#' mapvotr:::f_inContour(sfpoints, sfcontour, verbose)

f_inContour <- function(sfpoints, sfcontour, verbose = FALSE) {
  
  logr::put("Launch f_inContour")

  test_out <- sf::st_intersects(sfpoints, sfcontour) %>% purrr::map_lgl(~ length(.x) == 0L)
  if (verbose) logr::put(paste0(sum(test_out), " points removed"))
  sfpoints[!test_out, ]
}
