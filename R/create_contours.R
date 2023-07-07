#' create vote offices contours
#'
#' @description
#' - Crée les contours des bureaux de votes pour une ville donnée
#' - Create all outputs needed for a targeted city
#'
#' @details
#'  - Addresses out of their geographic contours are filtered
#'  - Only works for cities verifying at least "min_points_com" points
#'  - Create BV contours only if the BV contains at least "min_address_bv" addresses
#'  - Creates voronoi polygons (intersected with city borders)
#'  - Links each Voronoi polygon with one single BV (hypothesis in "decouplage_ptsBv" function documentation)
#'  - Creates BV contours grouping voronoi polygons by BVs
#'  - Gives a simplified version of those contours trying to remove little cast polygons
#'  with less than "min_address_shoot" addresses (see "shoot_isolated" documentation)
#'
#' @param prep_adr
#' Result from prepare_address function :
#'    - address : table of geolocalized addresses (all cities of the scope)
#'    - com : city contours (sf, all scope)
#'    - lcog_ok : scope of cities available
#' @param cog official targetted city code
#' @param min_points_com minimal number of address by city (global parameter)
#' @param min_address_bv minimal number of address by vote office (global parameter)
#' @param min_address_shoot minimal number of address by polygons in result contours (global parameter)
#' @param var_cog1 name for city code variable in address table
#' @param var_cog2 name for city code variable in city-contours table
#' @param var_bv1 name for bv code variable in address table
#' @param var_geo_score name for geo_score variable in address table
#' @param var_nbaddress name for number of addresses for each point in address table
#' @param path_log path where to write logs (default NULL)
#'
#' @return
#'    - if city in the scope : list of raw contours and simplified contours
#'    - NULL otherwise (for looping purpose)
#' @export
#' @examples
#' address <- mapvotr::addresses_sample
#' contours_com <- mapvotr::contours_com_sample
#' prep_adr <- prepare_address(
#'   address = address,
#'   contours_com = contours_com,
#'   var_cog1 = "code_commune_ref",
#'   var_bv1 = "id_brut_bv_reu",
#'   path_log = NULL
#' )
#' cog <- "29039"
#' path_log <- NULL
#' min_points_com <- 50
#' min_address_bv <- 15
#' min_address_shoot <- 5
#' var_cog1 <- "code_commune_ref"
#' var_cog2 <- "code_insee"
#' var_bv1 <- "id_brut_bv_reu"
#' var_geo_score <- "geo_score"
#' var_nbaddress <- "nb_adresses"
#' create_contours(prep_adr, cog, min_points_com, min_address_bv, min_address_shoot)
create_contours <- function(prep_adr, cog, min_points_com, min_address_bv, min_address_shoot, var_cog1 = "code_commune_ref", var_cog2 = "code_insee", var_bv1 = "id_brut_bv_reu", var_geo_score = "geo_score", var_nbaddress = "nb_adresses", path_log = NULL) {
  # Open logs
  if (!is.null(path_log)) {
    dir_log <- path_log
  } else {
    dir_log <- tempdir()
  }

  lf <- logr::log_open(file.path(dir_log, "log_create_contours"))

  address <- prep_adr$address
  com <- prep_adr$contours_com
  lcog_ok <- prep_adr$lcog
  code_epsg <- epsg_from_cog(cog)

  logr::put("First checks")
  if (!cog %in% lcog_ok) {
    logr::put("Only one BV in this city")
    return(NULL)
  }

  # Filter addresses and contours
  address <- address %>%
    dplyr::filter(.data[[var_cog1]] == cog) %>%
    sf::st_transform(code_epsg)
  com <- com %>%
    dplyr::filter(.data[[var_cog2]] == cog) %>%
    sf::st_transform(code_epsg)

  # Force addresses to be inside the city contour
  address <- f_inContour(address, com, verbose = T)

  # Stop if the city is out of scope
  logr::put("Check if the city can be in the scope")
  scope_test <- valid_for_contours(address, min_points_com, min_address_bv, var_bv1 = var_bv1, var_nbaddress = var_nbaddress)

  if (isFALSE(scope_test)) { # All the city is problematic
    return(NULL)
  } else if (!isTRUE(scope_test)) { # Some problematic BVs
    # Remove lines related to theses BVs
    nrow_init <- nrow(address)
    naddress_init <- sum(address[[var_nbaddress]])
    address <- address %>% filter(!.data[[var_bv1]] %in% scope_test)
    logr::put(paste0(nrow_init - nrow(address), " lines and ", naddress_init - sum(address[[var_nbaddress]]), " addresses removed for ", length(scope_test), " problematic Bvs"))
    logr::put("List of problematic BVs")
    logr::put(scope_test)
    # Re-test
    scope_test2 <- valid_for_contours(address, min_points_com, min_address_bv, var_bv1 = var_bv1, var_nbaddress = var_nbaddress)
    if (!isTRUE(scope_test2)) {
      logr::put("City not in the scope after we removed problematic BVs")
      return(NULL)
    }
  }


  # Create Voronoi
  voronoi <- voronoi_com(address, com, var_bv = var_bv1)

  # Link points to a unique vote office
  passage_ptsBv <- decouplage_ptsBv(address, com, var_bv = var_bv1, var_geo_score = var_geo_score, epsg = code_epsg)

  logr::put("Join voronoi and passage_ptsBv")
  voronoi <- sf::st_join(voronoi, passage_ptsBv)

  # Check : 1 BV per voronoi
  nb_problems <- voronoi[[var_bv1]] %>%
    is.na() %>%
    sum()
  stopifnot("create_contours : na values for BV code in voronois" = identical(nb_problems, 0L))

  # Creation of BV contours : Aggregation of voronois by BVs
  contours <- voronoi %>%
    dplyr::group_by(.data[[var_bv1]]) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))

  # Shoot isolated voronoi
  contours_simplified <- shoot_isolated(contours, voronoi, min_address_shoot = min_address_shoot, var_bv = var_bv1)

  logr::log_close()

  return(list(
    "contours" = contours,
    "contours_simplified" = contours_simplified
  ))
}
