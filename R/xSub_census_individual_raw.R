#' Census of individual-source event-level datasets in xSub (updated October 13, 2018)
#'
#' A list of data sources and countries available for download. Used by \code{info_xSub()}
#'
#' @format A list with 4 elements:
#' \describe{
#'   \item{level0_bysource}{Countries organized by \code{data_source}. List object, where each sub-entry is also a list, containing entries for \code{data_source},\code{country_iso3},\code{country_name}.}
#'   \item{level0_bycountry}{Data sources organized by country. List of data.frames, where each row is a country, with columns for \code{country_iso3},\code{country_name},\code{data_sources}.}
#'   \item{level1}{Detailed information on data sources, countries and spatial levels of analysis. data.frame, where each row is a source-country combination, with columns for \code{data_source},\code{country_iso3},\code{country_name},\code{units}.}
#'   \item{all_countries}{Vector of all country ISO3 codes. Used by \code{get_xSub_multi}.}
#' }
#' @source \url{http://www.x-sub.org/}
"xSub_census_individual_raw"
