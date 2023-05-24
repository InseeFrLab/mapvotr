#' Guess EPSG code
#' 
#' Guess epsg code using city code (choosing between metropolitan France and overseas territories)
#' 
#' @param cog Municipality official code
#'
#' @return epsg code
#' @export
#'
#' @examples
#' cog = 29019 
#' epsg_from_cog(cog)

epsg_from_cog <- function(cog) {
  
  logr::put("Launch : epsg_from_cog")
  stopifnot("epsg_from_cog : pb with cog"=stringr::str_length(as.character(cog))==5L) 

  proj_lambert93 <- 2154
  proj_reun <- 2975
  proj_mart <- 5490
  proj_guad <- proj_mart
  proj_guya <- 2972
  
  
  if (!stringr::str_detect(cog, "^97")) {
    proj_lambert93
  } else if (stringr::str_detect(cog, "^971")) {
    proj_guad
  } else if (stringr::str_detect(cog, "^972")) {
    proj_mart
  } else if (stringr::str_detect(cog, "^973")) {
    proj_guya
  } else if (stringr::str_detect(cog, "^974")) {
    proj_reun
  }else{
    stop("unvalide cog")
  }
  
}
