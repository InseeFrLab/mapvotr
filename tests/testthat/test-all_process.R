test_that("all_process", {
  
  # Create non-simplified BV contours
  
  address <- mapvotr::addresses_sample
  contours_com <- mapvotr::contours_com_sample
  path_log=NULL
  
  prep_adr <- prepare_address(
    address = address,
    contours_com = contours_com,
    var_cog1 = "code_commune_ref",
    var_bv1 = "id_brut_bv",
    path_log = path_log
  )
  
  cog <- "29039"
  min_points_com = 50 ; min_address_bv = 15  ; min_address_shoot = 5
  var_cog1 = "code_commune_ref";var_cog2 = "codgeo"
  var_bv1 = "id_brut_bv";var_geo_score = "geo_score" ;var_nbaddress = "nb_adresses"
  
  address = prep_adr$address
  com = prep_adr$contours_com
  lcog_ok = prep_adr$lcog
  code_epsg=mapvotr:::epsg_from_cog(cog)
  address <- address %>% dplyr::filter(.data[[var_cog1]] == cog) %>% sf::st_transform(code_epsg)
  com <- com %>% dplyr::filter(.data[[var_cog2]] == cog) %>% sf::st_transform(code_epsg)
  
  expect_no_error(address <-  mapvotr:::f_inContour(address,com),message = "Problem in f_inContour")
  expect_no_error(
    voronoi <- mapvotr:::voronoi_com(address,com,var_bv = var_bv1),message = "Problem in voronoi_com"
    )
  expect_no_error(
    passage_ptsBv <- mapvotr:::decouplage_ptsBv(address,com,var_bv = var_bv1,var_geo_score = var_geo_score,epsg = code_epsg),
    message = "Problem in decouplage_ptsBv"
  )
  
  voronoi <- sf::st_join(voronoi,passage_ptsBv)
  contours <- voronoi %>% dplyr::group_by(.data[[var_bv1]]) %>% dplyr::summarise(geometry=sf::st_union(geometry))

  expect_no_error(
    lcontours <- create_contours(prep_adr,cog,min_points_com,min_address_bv,min_address_shoot),
    message = "Problem in create_contours"
  )
  expect_no_error(
    map_contours(
      cog,
      prep_adr$address,
      lcontours$contours_simplified,
      var_code_bv = "id_brut_bv",
      var_score = "geo_score",
      var_cog = "code_commune_ref"
      #palettecouleurs = "Paired"
    ),
    message = "Problem in map_contours"
    
  )
  

})
