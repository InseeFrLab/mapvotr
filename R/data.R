#' Sample data for addresses
#'
#' A subset of addresses data 
#'
#' @format ## `addresses_sample`
#' A data frame with 12335 rows and 7 columns:
#' \describe{
#'   \item{id_brut_bv_reu}{BV id}
#'   \item{code_commune_ref}{City code}
#'   \item{longitude}{longitude (WGS84)}
#'   \item{latitude}{latitude (WGS84)}
#'   \item{geo_type}{Type of geolocalization (BAN variable)}
#'   \item{geo_score}{Geolocalization accuracy score (BAN variable)}
#'   \item{nb_adresses}{Number of addresses on each line (= for each geolocalization)}
#' }
#' @source Insee
"addresses_sample"



#' Sample data for city contours
#'
#' A subset of city contours data (IGN - BDTopo)
#'
#' @format ## `contours_com_sample`
#' A data frame with 3 rows and 2 columns:
#' \describe{
#'   \item{code_insee}{City code}
#'   \item{geometry}{sf column (WGS84)}
#' }
#' @source IGN
"contours_com_sample"


#' Example data for sf_input_voronoi
#'
#' A subset of real input
#'
#' @format ## `sf_input_voronoi`
#' A sf table with 269 rows and 6 columns:
#' \describe{
#'   \item{id_brut_bv_reu}{BV id}
#'   \item{code_commune_ref}{City code}
#'   \item{geo_type}{Type of geolocalization (BAN variable)}
#'   \item{geo_score}{Geolocalization accuracy score (BAN variable)}
#'   \item{nb_adresses}{Number of addresses on each line (= for each geolocalization)}
#'   \item{geometry}{sf column (WGS84)}
#' }
#' @source IGN
"sf_input_voronoi"
