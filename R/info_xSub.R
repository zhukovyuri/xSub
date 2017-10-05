#' Information on available xSub files
#'
#' This function reports the availability of files on the \code{www.x-sub.org} server, and corresponding country codes and units of analysis. For additional info, see \code{www.x-sub.org/about/what-is-xsub}.
#'
#' @param details Logical. If \code{details=TRUE}, function returns information on available units of analysis for each country.
#' @param data_source Subset results by data sources. Character string or vector.
#' @param country_iso3 Subset results by country codes (ISO3). Character string or vector.
#' @param country_name Subset results by country name. Character string or vector.
#' @import haven RCurl countrycode
#' @importFrom utils data download.file read.csv unzip write.csv globalVariables
#' @export
#' @seealso \code{\link{get_xSub}}, \code{\link{get_xSub_multi}}
#' @examples
#' # General info on data sources and countries
#' info_xSub()
#'
#' # Available files for Pakistan
#' info_xSub(country_name = "Pakistan")
#'
#' # Detailed info for Pakistan
#' info_xSub(details=TRUE,country_name = "Pakistan")
#'
#' # Available files for SCAD data source
#' info_xSub(data_source = "SCAD")


info_xSub <- function(details=FALSE,data_source=NULL,country_iso3=NULL,country_name=NULL){

  # data(xSub_census)

  if(length(data_source)==0&length(country_iso3)==0&length(country_name)==0){
    print(paste0(rep("%%%%",10),collapse=""));print(paste0(rep("%%%%",10),collapse=""))
    print("xSub datasets / by source")
    print(paste0(rep("%%%%",10),collapse=""));print(paste0(rep("%%%%",10),collapse=""))
    print(xSub_census$level0_bysource)
    print(paste0(rep("%%%%",10),collapse=""));print(paste0(rep("%%%%",10),collapse=""))
    print("xSub datasets / by country")
    print(paste0(rep("%%%%",10),collapse=""));print(paste0(rep("%%%%",10),collapse=""))
    print(xSub_census$level0_bycountry)
    print("For more info, select details=TRUE, and subset by data source or country")
  }
  if(details==TRUE&length(data_source)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1)
    print("For more info, subset by data source or country")
  }
  if(details==FALSE&length(data_source)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level1[xSub_census$level1$country_iso3%in%country_iso3,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(details==TRUE&length(data_source)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(details==FALSE&length(data_source)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level1[grep(paste0(country_name,collapse="|"),xSub_census$level1$country_name),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(details==TRUE&length(data_source)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grep(paste0(country_name,collapse="|"),xSub_census$level2$country_name),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(details==FALSE&length(data_source)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[xSub_census$level1$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(details==TRUE&length(data_source)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(length(data_source)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(length(data_source)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(length(data_source)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
}
