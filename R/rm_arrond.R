#' rm_arrond
#'
#' replace "arrondissements" cog by city cog for Paris, Lyon and Marseille
#' 
#' @param tab table with a cog column 
#' @param var_cog name of the cog column
#'
#' @return tab with modified cog column (Paris, Lyon and Marseilles codes without arrondissement)
#'
#' @examples
#' tab <- data.frame(id=1:5,code_commune_ref=c("75118","75000","35000","69382",NA))
#' var_cog <- "code_commune_ref"
#' mapvotr:::rm_arrond(tab,var_cog)


rm_arrond <- function(tab,var_cog){
  
  logr::put("Launch rm_arrond")
  
  tab[[var_cog]][tab[[var_cog]] %>% stringr::str_detect("^751")] <- "75056"
  tab[[var_cog]][tab[[var_cog]] %>% stringr::str_detect("^132")] <- "13055"
  tab[[var_cog]][tab[[var_cog]] %>% stringr::str_detect("^6938")] <- "69123"
  return(tab)
}
