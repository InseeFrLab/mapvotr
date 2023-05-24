#' Make Voronoï polygons
#'
#' Create voronoi polygons inside city contours.
#'
#' @description Partition a city into voronoi polygons :
#' - Use sf::st_voronoi with `envelope` parameter (= city contour)
#' - Cast the result and intersects with the city contour
#' @details
#'   - 1 point = several addresses = several BVs
#'
#' @param sfelecteurs sf points
#' @param com city contours (sf multipolygons)
#' @param var_bv column name for BV id column
#'
#' @return
#' voronoi multipolygons
#' @examples
#' sfelecteurs <- mapvotr::sf_input_voronoi
#' com <- mapvotr::contours_com_sample %>%
#'   dplyr::filter(codgeo == "29039") %>%
#'   sf::st_transform(2154)
#' var_bv <- "id_brut_bv"
#' mapvotr:::voronoi_com(sfelecteurs, com, var_bv)

voronoi_com <- function(sfelecteurs, com, var_bv) {
  logr::put("Launch voronoi_com")

  # Count how many points (=lines) on the same geographic position
  points_multiBV <- sfelecteurs %>%
    dplyr::group_by(geometry) %>%
    dplyr::count(.data[[var_bv]], name = "n") %>%
    dplyr::filter(n > 1)

  # Log how many of them
  if (nrow(points_multiBV) > 0L) 
    logr::put(paste0(nrow(points_multiBV), " points with addresses related to different vote offices"))

  # Create Voronoï polygons inside the city 'envelope"
  voronoi <- sf::st_voronoi(x = sf::st_union(sfelecteurs), envelope = com$geometry) %>% sf::st_as_sf()
  sf::st_geometry(voronoi) <- "geometry"
  voronoi <- voronoi %>% sf::st_cast()
  
  res <- sf::st_intersection(st_geometry(voronoi), st_geometry(com)) # avoid warnings this way
  res <- sf::st_as_sf(res) %>%
    dplyr::rename("geometry" = "x") %>%
    dplyr::mutate(id_voronoi = row.names(.)) # add id
  
  return(res)
}
