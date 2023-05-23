#' fget_multiBV
#' 
#' Get COG for cities with at leat 2 vote offices
#'
#' @param tab_addresses addresses
#' @param var_cog column name for cog
#' @param var_bv column name for BD id
#'
#' @return vector of city codes (for cities with at least 2 vote offices)
#'
#' @examples
#' var_cog = "code_commune_ref"
#' var_bv = "id_brut_bv"
#' mapvotr:::fget_multiBV(mapvotr::addresses_sample,"code_commune_ref","id_brut_bv")




fget_multiBV <- function(tab_addresses,var_cog,var_bv){
  nbBV_par_cog <- tab_addresses %>% 
    dplyr::group_by(.data[[var_cog]]) %>% 
    dplyr::summarise(nb_bv=length(unique(.data[[var_bv]])))
  
  nbBV_par_cog %>% dplyr::filter(nb_bv>1) %>% dplyr::pull(.data[[var_cog]])
  
}