
# addresses_sample

addresses <- arrow::read_parquet(
  "X:/HAB-Adresses-REU/data/6_Tables_finales/table_adresses.parquet"
)

addresses_sample <- addresses %>% 
  dplyr::filter(code_commune_ref %in% c("29046","29116","29039")) %>% 
  dplyr::select(id_brut_bv,code_commune_ref,longitude,latitude,geo_type,geo_score,nb_adresses)

usethis::use_data(addresses_sample, overwrite = TRUE)


# contours_sample

contours_com <- sf::st_read("//pd_as_ge_d1_50/ge_data_pd/creacartes_pd/fichiers-ihm/2022/franceentiere/commune_bdtopo_franceentiere_2022.shp"
)

contours_com_sample <- contours_com %>% 
  filter(codgeo %in% c("29046","29116","29039")) %>% 
  select(codgeo,geometry)

usethis::use_data(contours_com_sample, overwrite = TRUE)

# *********************************************************
#      Input for the example of `voronoi_com` function
# *********************************************************

address <- mapvotr::addresses_sample
contours_com <- mapvotr::contours_com_sample
prep_adr <- mapvotr::prepare_address(address = address,contours_com = contours_com,var_cog1 = "code_commune_ref",var_bv1 = "id_brut_bv",path_log=NULL)

cog <- "29039"
path_log=NULL
min_points_com = 50 ; min_address_bv = 15  ; min_address_shoot = 5
address = prep_adr$address
com = prep_adr$contours_com
lcog_ok = prep_adr$lcog
code_epsg=mapvotr:::epsg_from_cog(cog)
var_cog1 <- 'code_commune_ref'
var_bv1 <- "id_brut_bv"
var_cog2 <- 'codgeo'
var_nbaddress <- 'nb_adresses'
address <- address %>% dplyr::filter(.data[[var_cog1]] == cog) %>% sf::st_transform(code_epsg)
com <- com %>% dplyr::filter(.data[[var_cog2]] == cog) %>% sf::st_transform(code_epsg)
sf_input_voronoi <-  mapvotr:::f_inContour(address,com,verbose = T)

#Reduce size of data
sample <- sf_input_voronoi$id_brut_bv[1:3]
sf_input_voronoi <- sf_input_voronoi %>% dplyr::filter(id_brut_bv %in% sample)

usethis::use_data(sf_input_voronoi, overwrite = TRUE)
