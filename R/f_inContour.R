#' f_inContour
#' 
#' Exclude points out of a border
#'
#' @param sfpoints points (sf)
#' @param sfcontour polygon (sf)
#' @param verbose if TRUE : prints information 
#'
#' @return sfpoints without points out of sfContour
#'
#' @examples
#' sfpoints = addresses_sample[addresses_sample$code_commune_ref == "29039",]
#' sfpoints <- sfpoints %>%
#' sf::st_as_sf(
#'   coords = c("longitude", "latitude"), 
#'   crs = 4326,
#'   na.fail = FALSE
#' )
#' sfcontour = contours_com_sample[contours_com_sample$codgeo == "29039",]
#' verbose=TRUE
#' mapvotr:::f_inContour(sfpoints,sfcontour,verbose)


f_inContour <- function(sfpoints,sfcontour,verbose=FALSE){
  test_out <- sf::st_intersects(sfpoints,sfcontour) %>% purrr::map_lgl(~length(.x)==0L)
  if(verbose) print(paste0(sum(test_out)," points removed"))
  sfpoints[!test_out,]
}