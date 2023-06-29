#' Simplify voting contours
#'
#' Simplify contours removing isolated polygons
#'
#' @details
#'  - Counts the number of voronoï polygons used to create each BV contour
#'  - Targets cast BV Contours (cBVc) built with less than min_address_shoot voronois/points
#'  - if cBVc is not isolated (island for example) and at least one neighbor is big enough:
#'     - replace old BV code by the biggest neighbor 's BV code.
#'  - Reshape simplified BV contours
#'
#' @param contours contours
#' @param voronoi voronoi polygons
#' @param min_address_shoot minimal number of address by polygons in result contours (global parameter)
#' @param var_bv name for bv code variable in address table
#'
#' @return simplified contours
#'
#' @import sf dplyr

shoot_isolated <- function(contours, voronoi, min_address_shoot, var_bv = "id_brut_bv_reu") {
  logr::put("Launch : shoot_isolated")

  # Cast contours
  contours_cast <- st_cast_bis(contours)
  # Cast voronoi
  voronoi_cast <- st_cast_bis(voronoi)

  # Find how many "address/voronoi" per cast polygons
  contours_cast$nb_voro <- count_voro(contours_cast, voronoi_cast)
  
  # Targets too little cast polygons
  pos_target <- which(contours_cast$nb_voro <= min_address_shoot)
  
  # Get cast polygons neighbors
  contours_touch <- st_touches(contours_cast)

  # Initialize the simplified contours
  contours_res <- contours_cast

  # For each little cast polygon, give a new BV code depending on neighbors
  for (i in pos_target) {
    # print(i)
    pos_voisins <- contours_touch[[i]]

    # if cast polygon is not isolated (island for example)
    if (!identical(length(pos_voisins), 0L)) {
      # At leat one Neighboor is big enough
      nb_addresses_neigh <- contours_cast[pos_voisins, "nb_voro", drop = T]
      if (sum(nb_addresses_neigh > min_address_shoot) > 0) {
        bv_neighbor <- st_drop_geometry(contours_cast)[pos_voisins, var_bv, drop = T] %>% unique()
        if (length(bv_neighbor) > 1L) {
          # if several neighbors (code_bv), big is beautiful
          bv_replace <- contours_cast %>%
            st_drop_geometry() %>%
            filter(.data[[var_bv]] %in% bv_neighbor) %>%
            group_by(.data[[var_bv]]) %>%
            summarise(nb_voro = sum(nb_voro)) %>%
            filter(nb_voro == max(nb_voro)) %>%
            pull(.data[[var_bv]])
          # In case several neighbors of same "size"...
          if (length(bv_replace) > 1L) bv_replace <- bv_replace[sample(seq_along(bv_replace), 1)]
        } else {
          # If only one neighbor
          bv_replace <- bv_neighbor
        }
        contours_res[i, var_bv] <- bv_replace
      }
    }
  }

  # Union cast polygons using modified code_bv
  contours_res <- contours_res %>%
    select(.data[[var_bv]]) %>%
    group_by(.data[[var_bv]]) %>%
    summarise(geometry = st_union(geometry))
  return(contours_res)
}
