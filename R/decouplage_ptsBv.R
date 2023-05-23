#' decouplage_ptsBv
#' 
#' Gives a single BV for each point
#' 
#' @details
#'  - when several addresses geolocalized on the same point, and those addresses are related with different BVs :
#'     - If possible : Take the BV with a maximum number of addresses geolocalized on that point
#'     - If as many number of addresses for several BVs : take the BV related to the best geolocalised point 
#'     - Else, random choice (rare)
#'
#' @param sfelecteurs : table of addresses (sf object)
#' @param com : city contour (sf object)
#' @param epsg : epsg code
#' @param var_bv name for bv code variable in address table
#' @param var_geo_score name for geo_score variable in address table
#' @param var_nbaddress name for number of addresses for each point in address table
#'
#' @return passing table between BVs and points
#'
#' @examples
#' sfelecteurs <- mapvotr::sf_input_voronoi
#' com <- mapvotr::contours_com_sample%>% dplyr::filter(codgeo == "29039") %>% sf::st_transform(2154)
#' var_bv <- "id_brut_bv"
#' var_geo_score <- "geo_score"
#' var_nbaddress <-  "nb_adresses"
#' epsg <- 2154
#' mapvotr:::decouplage_ptsBv(sfelecteurs,com,var_bv,var_geo_score,var_nbaddress,epsg)


decouplage_ptsBv <- function(sfelecteurs,com,var_bv = "id_brut_bv",var_geo_score="geo_score",var_nbaddress = "nb_adresses",epsg){
  
  # Manage variable names...
  # colnames(sfelecteurs)[colnames(sfelecteurs)==var_bv] <- "code_bv"
  
  # Retrieve numeric coordinates (to group addresses by geometric points later)
  xy <- sf::st_coordinates(st_geometry(sfelecteurs)) %>% 
    dplyr::as_tibble() %>% 
    dplyr::rename(x_point=X,y_point=Y) %>% 
    dplyr::mutate(xy=paste(x_point,y_point,sep = "_")) %>% 
    dplyr::pull(xy)
  sfelecteurs <- sfelecteurs %>% cbind(xy)
  
  # Si plusieurs bureaux pour 1 coord, prendre le bureau ayant le max d'adresses.
  dfelecteurs <- sfelecteurs %>% sf::st_drop_geometry() 
  passage <- dfelecteurs %>% dplyr::group_by(.data[[var_bv]],xy) %>% dplyr::summarise(n_address=sum(.data[[var_nbaddress]]))
  passage <- passage %>% dplyr::group_by(xy) %>%
    dplyr::summarise( {{var_bv}} := .data[[var_bv]][n_address==max(n_address)]) 

  
  # Si autant d'adresses pour chaque BV en 1 coord, la coord la mieux géolocalisée impose son BV
  if(sum(duplicated(passage$xy))>0){
    coords_multiBV_egalite <- passage$xy[duplicated(passage$xy)] %>% unique()
    passage_qualiteGeo <- dfelecteurs %>% dplyr::filter(xy %in% coords_multiBV_egalite)
    passage_qualiteGeo <- passage_qualiteGeo %>% 
      dplyr::group_by(xy) %>% 
      dplyr::summarise({{var_bv}} :=.data[[var_bv]][.data[[var_geo_score]]==max(.data[[var_geo_score]])])

    passage <- passage %>% dplyr::filter(!xy %in% coords_multiBV_egalite) %>% rbind(passage_qualiteGeo)
    
    # S'il y a égalité des qualités de géolocalisation, en reste, chaque du premier BV dans la liste : 
    # passage <- passage %>% dplyr::group_by(xy) %>% dplyr::summarise(dplyr::across(.cols = dplyr::all_of(var_bv),.fns = ~.[1],.names = var_bv))
    passage <- passage %>% dplyr::group_by(xy) %>% dplyr::summarise( {{ var_bv }} := .data[[var_bv]][1])
  }
 
  
  nb_dup <- passage$xy %>% duplicated() %>% sum()
  if(nb_dup>0) stop("Probleme avec la table de passage")
  
  ##### Remise en forme geometrique
 
  passage <- passage %>% dplyr::mutate(
    x = stringr::str_replace(xy,"_.*","") %>% as.numeric(),
    y = stringr::str_replace(xy,".*_","") %>% as.numeric()
  )
  
  passage <- sf::st_as_sf(passage, coords = c("x","y"), crs = as.numeric(epsg)) 
  passage$xy <- NULL
  
  # Manage variable names...
  #colnames(passage)[colnames(passage)=="code_bv"] <- var_bv
  
  
  return(passage)
  
}
