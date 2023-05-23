#' st_cast_bis
#' 
#' st_cast multipolygons to polygons, avoiding bugs when input spatial data are "incongruent".
#' See : https://github.com/r-spatial/sf/issues/763
#' @param sfdata : sf object
#'
#' @return cast polygons


st_cast_bis <- function(sfdata){
  sf::st_agr(sfdata) <- "constant" # avoid warnings
  sf::st_cast(sfdata,to="MULTIPOLYGON") %>% sf::st_cast(sfdata,to="POLYGON")
}
