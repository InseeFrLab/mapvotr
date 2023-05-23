#' prepare_address
#' 
#' Prepare your data from REU addresses ans city contours
#'   
#' @details
#' 
#' prepares : 
#'   - a table of address (only those "properly" geolocalized ) 
#'   - list of cities with at least 2 BVs
#'   - List of city contours
#' 
#'  - only BAN quality "housenumber","interpolation","locality"
#'  - only address for cities with at leat 2 BVs
#'
#' @param address address
#' @param contours_com city contours
#' @param path_log path to write logs (optional)
#' @param var_cog1 name for city code variable in address table
#' @param var_bv1 name for bv code variable in address table
#' @param var_cog2 name for city code variable in city-contours table
#'
#' @return
#' List of : 
#'   - table of address
#'   - table of cities (geographic contours)
#'   - List of city-codes (with at least 2 bv)
#' @export
#' @examples 
#' addresses_sample <- mapvotr::addresses_sample
#' contours_com_sample <- mapvotr::contours_com_sample
#' prepare_address <- prepare_address(address = addresses_sample,
#' contours_com = contours_com_sample,
#' var_cog1 = "code_commune_ref",
#' var_bv1 = "id_brut_bv",
#' path_log=NULL)




prepare_address <- function(address,contours_com,var_cog1 = "code_commune_ref", var_cog2 = "codgeo",var_bv1 = "id_brut_bv", path_log=NULL){
  
  # Open logs (if needed)
  option_logr_init <- getOption("logr.on")
  if(!is.null(path_log)) {
    options("logr.on" = T)
    lf <- logr::log_open(paste0(path_log,"/log_prepare_address"))
  }else{
    options("logr.on" = F)
  }
  
  
  logr::sep("prepare_address")
  logr::put("Row number in initial data base")
  address %>% nrow() %>% logr::put()
  
  # Remove "arrondissements" codes for Paris-Lyon-Marseille
  address <- address %>% rm_arrond(var_cog1) 
  
  logr::put("Row number after filtering geo_type")
  address <- address %>% dplyr::filter(.data[["geo_type"]] %in% c("housenumber","interpolation","locality")) 
  logr::put(nrow(address))
  
  logr::put("Row number after filtering cities with at leat 2 BdV")
  lcog <- fget_multiBV(address,var_cog=var_cog1,var_bv=var_bv1)
  names(lcog) <- lcog
  
  address <- address %>% dplyr::filter(.data[[var_cog1]] %in% lcog)
  logr::put(nrow(address))
  
  logr::put("Convert to sf using BAN coordinates")
  address <- address %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"), 
      crs = 4326,
      na.fail = FALSE
    )
  
  logr::sep("Read cities contours")

  logr::put("Initial number of rows")
  logr::put(nrow(contours_com))
  
  logr::put("Filtering only cities in the scope")
  contours_com <- contours_com %>% dplyr::filter(.data[[var_cog2]] %in% lcog)
  
  if(length(lcog)!=nrow(contours_com)){
    logr::put("City codes of address not found in city contours...")
    lcog[!lcog %in% contours_com[[var_cog2]]] %>% logr::put()
  }
  
  if(!is.null(path_log))
    logr::log_close()
  
  # Initial options
  options("logr.on" = option_logr_init)
  
  return(list("address"=address,"contours_com"=contours_com,"lcog"=lcog))
  
}








