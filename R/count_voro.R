#' count_voro
#'
#' Count the number of voronoï polygons used to create voting areas (contours)
#' 
#' @param contours : multipolygons (union of voronoï polygons)
#' @param voronoi : polygons (or multipolygon sometimes)
#' @param marg : technical parameter
#' 
#' @return number of voronoi polygons per contour (using intersections)
#'
#' @examples
#' \dontrun{
#' #contours <- lcontours$`29199` %>% st_cast_bis
#' #voronoi <- lvoronoi$`29199` %>% st_cast_bis
#' #marg <-  0.0001
#' #count_voro(contours,voronoi)
#' }



count_voro <- function(contours , voronoi, marg = 0.0001){
  
  contours_is_polygon <- unique(as.vector(sf::st_geometry_type(contours))) %>% identical("POLYGON")
  voronoi_is_polygon <- unique(as.vector(sf::st_geometry_type(voronoi))) %>% identical("POLYGON")
  stopifnot("count_voro : inputs must be POLYGONS" = contours_is_polygon & voronoi_is_polygon)
  
  # Inner buffer des contours (sinon bugs aux frontières)
  contours_shaved <- sf::st_buffer(contours,-marg)
  
  inter <- sf::st_intersects(contours_shaved,voronoi)
  
  count_inter <- inter %>% purrr::map_dbl(~.x %>% unique() %>% length)
  
  stopifnot("count_voro : mismatch with intersection"=sum(count_inter) == nrow(voronoi))
  # stopifnot("count_voro : mismatch with intersection"=sum(count_inter) == nrow(st_cast_bis(voronoi)))
  
  return(count_inter)
}

