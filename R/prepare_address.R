#' Pre-process addresses
#'
#' @description
#' * Prétraitement des données à partir des adresses du REU et des contours de communes
#' * Prepares your data from REU addresses and city contours
#'
#' @details
#' prepares :
#'   - a table of address
#'     - scope : only those with a minimal geolocalization quality
#'     - BAN quality "housenumber", "interpolation" or "locality"
#'   - list of cities with at least 2 BVs (among filtered addresses)
#'   - List of city geometric contours
#'
#' @param address addresses from REU data (Insee)
#' @param contours_com city contours (ex : BDTopo IGN)
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
#' prepare_address <- prepare_address(
#'   address = addresses_sample,
#'   contours_com = contours_com_sample,
#'   var_cog1 = "code_commune_ref",
#'   var_bv1 = "id_brut_bv",
#'   path_log = NULL
#' )
prepare_address <- function(address, contours_com, var_cog1 = "code_commune_ref", var_cog2 = "codgeo", var_bv1 = "id_brut_bv", path_log = NULL) {

  # Open logs (if needed)
  if (!is.null(path_log)) {
    dir_log <- path_log
  } else {
    dir_log <- tempdir()
  }

  lf <- logr::log_open(file.path(dir_log, "log_prepare_address"))

  logr::sep("prepare_address")
  logr::put("Row number in initial data base")
  address %>%
    nrow() %>%
    logr::put()

  # Remove "arrondissements" codes for Paris-Lyon-Marseille
  address <- address %>% rm_arrond(var_cog1)

  logr::put("Row number after filtering geo_type")
  address <- address %>% dplyr::filter(.data[["geo_type"]] %in% c("housenumber", "interpolation", "locality"))
  logr::put(nrow(address))

  logr::put("Row number after filtering cities with at leat 2 BdV")
  lcog <- fget_multiBV(address, var_cog = var_cog1, var_bv = var_bv1)
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

  if (length(lcog) != nrow(contours_com)) {
    logr::put("City codes of address not found in city contours...")
    lcog[!lcog %in% contours_com[[var_cog2]]] %>% logr::put()
  }

  logr::log_close()

  return(list("address" = address, "contours_com" = contours_com, "lcog" = lcog))
}
