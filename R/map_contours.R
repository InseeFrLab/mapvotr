#' map_contours
#' 
#' Create a dynamic map of contours
#' 
#' @param cog : Municipality official code
#' @param sfelecteurs : addresses of the targeted city
#' @param contours_bv : contours of the targeted city
#' @param var_code_bv : name of column : id BV
#' @param var_score  : name of column : quality of geocoding
#' @param var_cog : name of column : official city code (COG)
#'
#' @return dynamic map (leaflet style)
#' @export
#' @import sf leaflet
#' @examples
#' \dontrun{
#' cog <- city_code
#' sfelecteurs=lres$addresses
#' contours_bv=lcontours$contours_simplified
#' var_cog = "code_commune_ref"
#' var_score="geo_score"
#' var_code_bv="id_brut_bv"
#' palettecouleurs="Paired"
#' map_contours(cog,sfelecteurs,contours_bv)
#' }

map_contours <- function(cog,sfelecteurs,contours_bv, 
                         var_code_bv="id_brut_bv",var_score="geo_score",var_cog = "code_commune_ref"){
  
  # # Tranform to WGS84 proj
  # sfelecteurs <- st_transform(sfelecteurs,crs = 4326)
  contours_bv <- st_transform(contours_bv,crs = 4326)
  
  sfelecteurs <- sfelecteurs %>% filter(.data[[var_cog]]==cog)
  
  liste_bv <- unique(sfelecteurs[[var_code_bv]])
  nb_bv <- length(liste_bv)
  # couleurs <- RColorBrewer::brewer.pal(nb_bv,palettecouleurs)
  # pal <- leaflet::colorFactor(couleurs, domain = liste_bv)
  set.seed(2)
  domain <- liste_bv[sample(seq_len(nb_bv),nb_bv)]
  pal <- leaflet::colorFactor(grDevices::rainbow(nb_bv), domain ,ordered=T)
  
  label_electeurs <- sfelecteurs %>% 
    dplyr::mutate(
      lab=paste0(
        "BV : ",.data[[var_code_bv]],
        #" / addresse : ",.data[[var_addresse]],
        " / confiance geoloc : ",.data[[var_score]])
    ) %>% dplyr::pull(lab)
  
  carte <- leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
    leaflet::addProviderTiles(providers$GeoportailFrance.orthos, group = "Satellite2") %>%
    leaflet::addProviderTiles(providers$CartoDB.Positron, group = "Clair") %>%
    leaflet::addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    # leaflet::addPolygons(
    #   data = voronoi_com,
    #   weight = 2,
    #   color = "black",
    #   fillColor = ~ pal(code_bv),
    #   fillOpacity = 0.8,
    #   label =  ~ htmlEscape(paste("Code bv retenu :", code_bv)),
    #   group = "voronoi"
    # ) %>%
    leaflet::addPolygons(
      data = contours_bv,
      weight = 2,
      color = "black",
      fillColor = ~ pal(contours_bv[[var_code_bv]]),
      fillOpacity = 0.8,
      label =  ~ htmltools::htmlEscape(paste("Code bv retenu :", contours_bv[[var_code_bv]])),
      group = "Contours"
    ) %>%
    # addPolygons(
    #   data = sfbv %>% st_buffer(100),
    #   fillColor = ~ pal(code_bv),
    #   color = "white",
    #   opacity = 1,
    #   weight = 5,
    #   fillOpacity = 0.8,
    #   label = ~ code_bv,
    #   group = "BdV"
    #   
    # ) %>%
  addCircleMarkers(
    data = sfelecteurs,
    fillColor = ~ pal(sfelecteurs[[var_code_bv]]),
    color = "black",
    weight = 1,
    fillOpacity = 0.8,
    label = htmltools::htmlEscape(label_electeurs),
    group =  "electeurs"
  ) %>%
    addLayersControl(
      overlayGroups = c("electeurs", "Contours"), #, "BdV"
      baseGroups = c(
        "Clair",
        "OSM (default)",
        "Satellite",
        "Satellite2",
        "Toner Lite"
      )
    ) %>%
    hideGroup("electeurs") #on cache les électeurs par défaut
  
  return(carte)
}
