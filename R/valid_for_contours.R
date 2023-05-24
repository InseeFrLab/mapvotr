#' valid_for_contours
#' 
#' Check if a city can be in the scope (ok to be divided into several geographical contours)
#' The function uses the addresses of the city targeted
#'
#' @param tab_address : addresses
#' @param min_points_com : minimal number of points by city (global parameter)
#' @param min_address_bv :minimal number of address by vote office (global parameter)
#' @param var_bv1 name for bv code variable in address table
#' @param var_nbaddress name for number of addresses for each point in address table
#'
#' @return TRUE if the city is in the scope, otherwise FALSE or a list of problematic BVs
#' @examples
#' tab_address <- mapvotr::addresses_sample
#' min_points_com <- 50
#' min_address_bv <- 15
#' var_bv1 = "id_brut_bv"
#' var_nbaddress = "nb_adresses"
#' mapvotr:::valid_for_contours(tab_address, min_points_com, min_address_bv,var_bv1,var_nbaddress)
#' 
#' # With some problematic BVs
#' lines_pb <- tab_address[tab_address$id_brut_bv == "29039_1",][1:5,]
#' tab_address_pb <- tab_address[tab_address$id_brut_bv != "29039_1",] %>% rbind(lines_pb)
#' mapvotr:::valid_for_contours(tab_address_pb, min_points_com, min_address_bv,var_bv1,var_nbaddress)




valid_for_contours <- function(tab_address, min_points_com, min_address_bv,var_bv1,var_nbaddress) {
  
  logr::put("Launch valid_for_contours")
  
  tab_address <- tab_address %>% sf::st_drop_geometry()
  nb_row <- nrow(tab_address)
  

  if (nb_row < min_points_com) {
    logr::put(paste0(
      "Not enough points in the city : ",
      nb_row,
      " (threshold = ",
      min_points_com,
      ")"
    ))
    return(FALSE)

  }else{
    count_address_by_bv <- tab_address %>% dplyr::group_by(.data[[var_bv1]]) %>% dplyr::summarise(nb_address= sum(.data[[var_nbaddress]]))
    
    if (min(count_address_by_bv$nb_address) < min_address_bv) {
      logr::put(paste0("Not enough addresses by BV (threshold = ", min_address_bv, ")"))
      bv_problem <- count_address_by_bv %>% dplyr::filter(nb_address < min_address_bv) %>% logr::put()
      return(bv_problem[[var_bv1]])
      
    }else{
      logr::put("No problem")
      return(T)
    }
  }
  
}
