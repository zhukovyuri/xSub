#' Information on available xSub files
#'
#' This function reports the availability of files on the \code{www.x-sub.org} server, and corresponding country codes and units of analysis. For additional info, see \code{www.x-sub.org/about/what-is-xsub}.
#'
#' @param details Logical. If \code{details=TRUE}, function returns information on available units of analysis for each country.
#' @param data_source Subset results by data sources. Character string or vector.
#' @param sources_type Type of data sources ("individual" or "multiple"). Character string.
#' @param data_type Type of dataset ("event" or "panel"). Character string.
#' @param country_iso3 Subset results by country codes (ISO3). Character string or vector.
#' @param country_name Subset results by country name. Character string or vector.
#' @param geo_window Geographic window (if source_type="multiple"). Could be either of "1 km" or "5 km". Character string or vector.
#' @param time_window Time window (if source_type="multiple"). Could be either of "1 day" or "2 day". Character string or vector.
#' @param dyad_type Time window (if source_type="multiple"). Could be either of "undirected" or "directed". Character string or vector.
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
#'
#' # Available files for SCAD data source, event-level
#' info_xSub(data_source = "SCAD", data_type = "event")
#'
#' # Multiple data sources, directed dyads
#' info_xSub(sources_type = "multiple", dyad_type = "directed")
#'
#' # Multiple data sources, directed dyads, Russia
#' info_xSub(sources_type = "multiple", dyad_type = "directed", country_name = "Russia")


info_xSub <- function(details=FALSE,sources_type="individual",data_type="panel",
                      data_source=NULL,country_iso3=NULL,country_name=NULL,
                      geo_window=NULL,time_window=NULL,dyad_type=NULL){

  # # Suppres notes
  # suppressBindingNotes <- function(variablesMentionedInNotes) {
  #   for(variable in variablesMentionedInNotes) {
  #     assign(variable,NULL, envir = .GlobalEnv)
  #   }
  # }
  # suppressBindingNotes(c("countrycode_data","census_individual_raw","census_individual_spatial","census_multiple_raw","census_multiple_spatial"))
  # utils::suppressForeignCheck(c("countrycode_data","census_individual_raw","census_individual_spatial","census_multiple_raw","census_multiple_spatial"))
  # utils::globalVariables(c("countrycode_data","census_individual_raw","census_individual_spatial","census_multiple_raw","census_multiple_spatial"),add=FALSE)


  if(grepl("^I|^i",sources_type)&grepl("^s|^p|^S|^P",data_type)){xSub_census <- xSub_census_individual_spatial}
  if(grepl("^I|^i",sources_type)&grepl("^E|^e",data_type)){xSub_census <- xSub_census_individual_raw}
  if(grepl("^M|^m",sources_type)&grepl("^s|^p|^S|^P",data_type)){xSub_census <- xSub_census_multiple_spatial}
  if(grepl("^M|^m",sources_type)&grepl("^E|^e",data_type)){xSub_census <- xSub_census_multiple_raw}

  if(length(data_source)==0&length(country_iso3)==0&length(country_name)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0){
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

  if(grepl("^I|^i",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1)
    print("For more info, subset by data source or country")
  }
  if(grepl("^I|^i",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level1[xSub_census$level1$country_iso3%in%country_iso3,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^I|^i",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^I|^i",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level1[grep(paste0(country_name,collapse="|"),xSub_census$level1$country_name),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^I|^i",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grep(paste0(country_name,collapse="|"),xSub_census$level2$country_name),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^I|^i",sources_type)&details==FALSE&length(data_source)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[xSub_census$level1$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^I|^i",sources_type)&details==TRUE&length(data_source)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^I|^i",sources_type)&length(data_source)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^I|^i",sources_type)&length(data_source)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^I|^i",sources_type)&length(data_source)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }


  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(country_iso3)==0&length(country_name)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0){
    print(xSub_census$level1)
    print("For more info, subset by country, dyad type, geographic or time window")
  }


  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(country_iso3)>0&length(country_name)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0){
    print(xSub_census$level1[xSub_census$level1$country_iso3%in%country_iso3,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(country_iso3)>0&length(country_name)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(country_iso3)==0&length(country_name)>0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0){
    print(xSub_census$level1[grep(paste0(country_name,collapse="|"),xSub_census$level1$country_name),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(country_iso3)==0&length(country_name)>0&length(geo_window)==0&length(time_window)==0&length(dyad_type)==0){
    print(xSub_census$level2[grep(paste0(country_name,collapse="|"),xSub_census$level2$country_name),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[xSub_census$level1$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }


  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(geo_window,xSub_census$level1$geo_window),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(geo_window,xSub_census$level2$geo_window),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(time_window,xSub_census$level1$time_window),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(paste0("^",dyad_type),xSub_census$level1$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(geo_window,xSub_census$level1$geo_window)&grepl(time_window,xSub_census$level1$time_window),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(time_window,xSub_census$level1$time_window)&grepl(paste0("^",dyad_type),xSub_census$level1$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(geo_window,xSub_census$level1$geo_window)&grepl(paste0("^",dyad_type),xSub_census$level1$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(geo_window,xSub_census$level2$geo_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==FALSE&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level1[grepl(geo_window,xSub_census$level1$geo_window)&grepl(time_window,xSub_census$level1$time_window)&grepl(paste0("^",dyad_type),xSub_census$level1$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }
  if(grepl("^M|^m",sources_type)&details==TRUE&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)==0){
    print(xSub_census$level2[grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
    print("For more info, subset by country")
  }


  if(grepl("^M|^m",sources_type)&length(data_source)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }

  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)==0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }



  if(grepl("^M|^m",sources_type)&length(data_source)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(geo_window,xSub_census$level2$geo_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(geo_window,xSub_census$level2$geo_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)==0&length(country_name)>0){
    print(xSub_census$level2[grepl(paste0(country_name,collapse="|"),xSub_census$level2$country_name)&grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }



  if(grepl("^M|^m",sources_type)&length(data_source)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&xSub_census$level2$data_source%in%data_source,])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)==0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)==0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)==0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }
  if(grepl("^M|^m",sources_type)&length(data_source)==0&length(geo_window)>0&length(time_window)>0&length(dyad_type)>0&length(country_iso3)>0&length(country_name)>0){
    print(xSub_census$level2[xSub_census$level2$country_iso3%in%country_iso3&grepl(geo_window,xSub_census$level2$geo_window)&grepl(time_window,xSub_census$level2$time_window)&grepl(paste0("^",dyad_type),xSub_census$level2$dyad_type),])
    print("* daily-level data available only for adm0, adm1")
  }


}
